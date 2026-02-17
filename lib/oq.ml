open Core

module Exit_code = struct
  type t =
    | Success
    | Query_or_usage_error
    | Io_or_permission_error
    | Parse_coverage_error

  let to_int = function
    | Success -> 0
    | Query_or_usage_error -> 1
    | Io_or_permission_error -> 2
    | Parse_coverage_error -> 3
end

module Diagnostic = struct
  type section_candidate = {
    title : string;
    start_line : int;
    end_line : int;
  }

  type parse_reason =
    | Syntax_error
    | Unsupported_construct
    | Invalid_utf8
    | Internal_parser_error

  let parse_reason_to_string = function
    | Syntax_error -> "syntax_error"
    | Unsupported_construct -> "unsupported_construct"
    | Invalid_utf8 -> "invalid_utf8"
    | Internal_parser_error -> "internal_parser_error"

  let error message = sprintf "Error: %s" message
  let warning message = sprintf "Warning: %s" message

  let parse_warning ~path ~reason =
    warning
      (sprintf "failed to parse %s: %s" path (parse_reason_to_string reason))

  let ambiguous_section_error ~title ~candidates =
    let header =
      error
        (sprintf "ambiguous section title %S (%d matches)" title
           (List.length candidates))
    in
    let candidate_lines =
      List.map candidates ~f:(fun candidate ->
          sprintf "  - %s (lines %d:%d)" candidate.title candidate.start_line
            candidate.end_line)
    in
    let hint =
      match candidates with
      | { start_line; end_line; _ } :: _ ->
          sprintf "Hint: use .section(%S, %d:%d)" title start_line end_line
      | [] -> sprintf "Hint: use .section(%S, <start>:<end>)" title
    in
    String.concat ~sep:"\n" (header :: candidate_lines @ [ hint ])
end

module Ordering = struct
  type result_ref = {
    relative_path : string;
    start_line : int;
    end_line : int;
  }

  let normalize_relative_path relative_path =
    String.substr_replace_all relative_path ~pattern:"\\" ~with_:"/"

  let compare_result_ref left right =
    let by_path =
      String.compare
        (normalize_relative_path left.relative_path)
        (normalize_relative_path right.relative_path)
    in
    if by_path <> 0 then by_path
    else
      let by_start_line = Int.compare left.start_line right.start_line in
      if by_start_line <> 0 then by_start_line
      else Int.compare left.end_line right.end_line

  let sort_result_refs refs = List.sort refs ~compare:compare_result_ref
end

module Org = struct
  module Span = struct
    type t = {
      start_line : int;
      end_line : int;
    }
  end

  module Source_ref = struct
    type t = {
      path : string;
      span : Span.t;
    }
  end

  module Todo_config = struct
    type t = {
      open_states : string list;
      done_states : string list;
    }

    let default = { open_states = [ "TODO" ]; done_states = [ "DONE" ] }

    let is_known_state config state =
      List.mem config.open_states state ~equal:String.equal
      || List.mem config.done_states state ~equal:String.equal

    let parse_from_keyword_value value =
      let tokens text =
        String.split (String.strip text) ~on:' '
        |> List.filter ~f:(fun token -> not (String.is_empty token))
      in
      match String.lsplit2 value ~on:'|' with
      | Some (open_text, done_text) ->
          let open_states = tokens open_text in
          let done_states = tokens done_text in
          if List.is_empty open_states || List.is_empty done_states then None
          else Some { open_states; done_states }
      | None ->
          let open_states = tokens value in
          if List.is_empty open_states then None
          else Some { open_states; done_states = default.done_states }
  end

  type heading = {
    id : int;
    level : int;
    raw_title : string;
    title : string;
    todo_keyword : string option;
    priority : char option;
    tags : string list;
    source : Source_ref.t;
    section_source : Source_ref.t;
    parent_id : int option;
    child_ids : int list;
  }

  type planning_kind =
    | Scheduled
    | Deadline
    | Closed

  type planning_entry = {
    kind : planning_kind;
    raw_value : string;
    heading_id : int;
    source : Source_ref.t;
  }

  type property = {
    key : string;
    value : string;
    heading_id : int;
    source : Source_ref.t;
  }

  type drawer = {
    name : string;
    heading_id : int option;
    source : Source_ref.t;
  }

  type block_kind =
    | Src
    | Example
    | Quote

  type block = {
    kind : block_kind;
    language : string option;
    heading_id : int option;
    source : Source_ref.t;
  }

  type link_kind =
    | Bracket
    | Plain

  type link = {
    kind : link_kind;
    target : string;
    description : string option;
    heading_id : int option;
    source : Source_ref.t;
  }

  type table = {
    rows : string list list;
    heading_id : int option;
    source : Source_ref.t;
  }

  type section = {
    heading_id : int;
    source : Source_ref.t;
  }

  type index = {
    headings : heading list;
    sections : section list;
    todos : heading list;
    planning : planning_entry list;
    properties : property list;
    drawers : drawer list;
    blocks : block list;
    links : link list;
    tables : table list;
  }

  type t = {
    path : string;
    line_count : int;
    keywords : (string * string) list;
    todo_config : Todo_config.t;
    index : index;
  }

  type parse_error = {
    path : string;
    reason : Diagnostic.parse_reason;
    line : int option;
    detail : string;
  }

  module Utf8 = struct
    let get_byte text index = Char.to_int text.[index]
    let is_cont byte = byte land 0xC0 = 0x80

    let is_valid text =
      let length = String.length text in
      let rec loop index =
        if index >= length then true
        else
          let b0 = get_byte text index in
          if b0 land 0x80 = 0 then loop (index + 1)
          else if b0 >= 0xC2 && b0 <= 0xDF then
            if index + 1 >= length then false
            else
              let b1 = get_byte text (index + 1) in
              if is_cont b1 then loop (index + 2) else false
          else if b0 = 0xE0 then
            if index + 2 >= length then false
            else
              let b1 = get_byte text (index + 1) in
              let b2 = get_byte text (index + 2) in
              if b1 >= 0xA0 && b1 <= 0xBF && is_cont b2 then loop (index + 3)
              else false
          else if (b0 >= 0xE1 && b0 <= 0xEC) || b0 = 0xEE || b0 = 0xEF then
            if index + 2 >= length then false
            else
              let b1 = get_byte text (index + 1) in
              let b2 = get_byte text (index + 2) in
              if is_cont b1 && is_cont b2 then loop (index + 3) else false
          else if b0 = 0xED then
            if index + 2 >= length then false
            else
              let b1 = get_byte text (index + 1) in
              let b2 = get_byte text (index + 2) in
              if b1 >= 0x80 && b1 <= 0x9F && is_cont b2 then loop (index + 3)
              else false
          else if b0 = 0xF0 then
            if index + 3 >= length then false
            else
              let b1 = get_byte text (index + 1) in
              let b2 = get_byte text (index + 2) in
              let b3 = get_byte text (index + 3) in
              if b1 >= 0x90 && b1 <= 0xBF && is_cont b2 && is_cont b3 then
                loop (index + 4)
              else false
          else if b0 >= 0xF1 && b0 <= 0xF3 then
            if index + 3 >= length then false
            else
              let b1 = get_byte text (index + 1) in
              let b2 = get_byte text (index + 2) in
              let b3 = get_byte text (index + 3) in
              if is_cont b1 && is_cont b2 && is_cont b3 then loop (index + 4)
              else false
          else if b0 = 0xF4 then
            if index + 3 >= length then false
            else
              let b1 = get_byte text (index + 1) in
              let b2 = get_byte text (index + 2) in
              let b3 = get_byte text (index + 3) in
              if b1 >= 0x80 && b1 <= 0x8F && is_cont b2 && is_cont b3 then
                loop (index + 4)
              else false
          else false
      in
      loop 0
  end

  let make_source_ref ~path ~start_line ~end_line =
    { Source_ref.path; span = { Span.start_line; end_line } }

  let parse_heading_line line =
    let line_length = String.length line in
    let rec count_stars index =
      if index < line_length && Char.equal line.[index] '*' then
        count_stars (index + 1)
      else index
    in
    let star_count = count_stars 0 in
    if star_count = 0 then None
    else if star_count >= line_length || not (Char.equal line.[star_count] ' ') then
      None
    else
      let body = String.strip (String.drop_prefix line (star_count + 1)) in
      Some (star_count, body)

  let is_tag_token token =
    let len = String.length token in
    if len < 3 then false
    else if not (Char.equal token.[0] ':' && Char.equal token.[len - 1] ':') then
      false
    else
      let parts =
        String.split token ~on:':'
        |> List.filter ~f:(fun part -> not (String.is_empty part))
      in
      List.length parts >= 1

  let split_title_and_tags raw_heading_body =
    let trimmed = String.rstrip raw_heading_body in
    match String.rsplit2 trimmed ~on:' ' with
    | Some (prefix, suffix) when is_tag_token suffix ->
        let tags =
          String.split suffix ~on:':'
          |> List.filter ~f:(fun tag -> not (String.is_empty tag))
        in
        (String.rstrip prefix, tags)
    | _ -> (trimmed, [])

  let is_priority_cookie token =
    String.length token = 4
    && Char.equal token.[0] '['
    && Char.equal token.[1] '#'
    && Char.equal token.[3] ']'

  let parse_heading_parts ~todo_config ~raw_heading_body =
    let title_without_tags, tags = split_title_and_tags raw_heading_body in
    let tokens =
      String.split title_without_tags ~on:' '
      |> List.filter ~f:(fun token -> not (String.is_empty token))
    in
    let todo_keyword, after_todo =
      match tokens with
      | token :: rest when Todo_config.is_known_state todo_config token ->
          (Some token, rest)
      | _ -> (None, tokens)
    in
    let priority, title_tokens =
      match after_todo with
      | token :: rest when is_priority_cookie token -> (Some token.[2], rest)
      | _ -> (None, after_todo)
    in
    let title = String.concat ~sep:" " title_tokens |> String.strip in
    (title, todo_keyword, priority, tags)

  let parse_keyword_line line =
    let trimmed = String.strip line in
    if not (String.is_prefix trimmed ~prefix:"#+") then None
    else
      match String.lsplit2 trimmed ~on:':' with
      | None -> None
      | Some (left, right) ->
          if String.length left < 3 then None
          else
            let key = String.uppercase (String.drop_prefix left 2 |> String.strip) in
            if String.is_empty key then None
            else Some (key, String.strip right)

  let parse_drawer_open line =
    let trimmed = String.strip line in
    let len = String.length trimmed in
    if len < 3 then None
    else if not (Char.equal trimmed.[0] ':' && Char.equal trimmed.[len - 1] ':')
    then None
    else
      let name = String.sub trimmed ~pos:1 ~len:(len - 2) |> String.strip in
      if String.is_empty name then None
      else if String.Caseless.equal name "END" then None
      else Some name

  let is_drawer_end line = String.Caseless.equal (String.strip line) ":END:"

  let parse_property_line line =
    let trimmed = String.strip line in
    if String.length trimmed < 4 || not (Char.equal trimmed.[0] ':') then None
    else
      match String.substr_index ~pos:1 trimmed ~pattern:":" with
      | None -> None
      | Some separator_index ->
          if separator_index = 1 then None
          else
            let key =
              String.sub trimmed ~pos:1 ~len:(separator_index - 1) |> String.strip
            in
            if String.is_empty key || String.Caseless.equal key "END" then None
            else
              let value =
                String.drop_prefix trimmed (separator_index + 1) |> String.strip
              in
              Some (key, value)

  type block_begin =
    | Supported of block_kind * string option
    | Unsupported of string

  let parse_block_begin line =
    let trimmed = String.strip line in
    let upper = String.uppercase trimmed in
    let prefix = "#+BEGIN_" in
    if not (String.is_prefix upper ~prefix) then None
    else
      let after_prefix = String.drop_prefix trimmed (String.length prefix) in
      let kind_token, rest =
        match String.lsplit2 after_prefix ~on:' ' with
        | Some (kind, trailing) -> (String.uppercase (String.strip kind), trailing)
        | None -> (String.uppercase (String.strip after_prefix), "")
      in
      match kind_token with
      | "SRC" ->
          let language =
            String.split (String.strip rest) ~on:' '
            |> List.filter ~f:(fun token -> not (String.is_empty token))
            |> List.hd
          in
          Some (Supported (Src, language))
      | "EXAMPLE" -> Some (Supported (Example, None))
      | "QUOTE" -> Some (Supported (Quote, None))
      | _ -> Some (Unsupported kind_token)

  let parse_block_end line =
    let trimmed = String.strip line in
    let upper = String.uppercase trimmed in
    let prefix = "#+END_" in
    if not (String.is_prefix upper ~prefix) then None
    else
      let kind =
        String.drop_prefix upper (String.length prefix)
        |> String.strip
        |> String.uppercase
      in
      Some kind

  let block_kind_to_end_token = function
    | Src -> "SRC"
    | Example -> "EXAMPLE"
    | Quote -> "QUOTE"

  let is_table_line line =
    let trimmed = String.lstrip line in
    String.is_prefix trimmed ~prefix:"|"

  let parse_table_row line =
    let trimmed = String.strip line in
    let inner =
      let dropped_start =
        if String.is_prefix trimmed ~prefix:"|" then
          String.drop_prefix trimmed 1
        else trimmed
      in
      if String.is_suffix dropped_start ~suffix:"|" then
        String.drop_suffix dropped_start 1
      else dropped_start
    in
    String.split inner ~on:'|' |> List.map ~f:String.strip

  let trim_plain_link_token token =
    String.strip token ~drop:(function
      | '(' | ')' | '[' | ']' | '<' | '>' | ',' | '.' | ';' | ':' | '"' | '\''
        -> true
      | _ -> false)

  let extract_plain_links line =
    String.split line ~on:' '
    |> List.filter_map ~f:(fun token ->
           let normalized = trim_plain_link_token token in
           if String.is_prefix normalized ~prefix:"http://"
              || String.is_prefix normalized ~prefix:"https://"
           then Some normalized
           else None)

  let extract_bracket_links line =
    let rec loop position acc =
      match String.substr_index ~pos:position line ~pattern:"[[" with
      | None -> List.rev acc
      | Some start_index ->
          let search_from = start_index + 2 in
          (match String.substr_index ~pos:search_from line ~pattern:"]]" with
          | None -> List.rev acc
          | Some end_index ->
              let inner =
                String.sub line ~pos:search_from ~len:(end_index - search_from)
              in
              let link =
                match String.substr_index inner ~pattern:"][" with
                | Some split_index ->
                    let target = String.prefix inner split_index |> String.strip in
                    let description =
                      String.drop_prefix inner (split_index + 2) |> String.strip
                    in
                    (target, Some description)
                | None -> (String.strip inner, None)
              in
              loop (end_index + 2) (link :: acc))
    in
    loop 0 []

  let parse_planning_on_line line =
    let scan kind keyword =
      let pattern = keyword ^ ":" in
      match String.substr_index line ~pattern with
      | None -> None
      | Some index ->
          let value =
            String.drop_prefix line (index + String.length pattern) |> String.strip
          in
          if String.is_empty value then None else Some (kind, value)
    in
    [
      scan Scheduled "SCHEDULED";
      scan Deadline "DEADLINE";
      scan Closed "CLOSED";
    ]
    |> List.filter_opt

  type heading_builder = {
    id : int;
    level : int;
    raw_title : string;
    title : string;
    todo_keyword : string option;
    priority : char option;
    tags : string list;
    start_line : int;
    mutable end_line : int;
    mutable parent_id : int option;
    mutable child_ids_rev : int list;
  }

  type open_drawer = {
    name : string;
    start_line : int;
    heading_id : int option;
  }

  type open_block = {
    kind : block_kind;
    language : string option;
    start_line : int;
    heading_id : int option;
  }

  type open_table = {
    start_line : int;
    heading_id : int option;
    mutable rows_rev : string list list;
  }

  let parse_string ~path content =
    if not (Utf8.is_valid content) then
      Error
        {
          path;
          reason = Diagnostic.Invalid_utf8;
          line = None;
          detail = "input is not valid UTF-8";
        }
    else
      try
        let lines = String.split_lines content |> Array.of_list in
        let line_count = Array.length lines in
        let todo_config = ref Todo_config.default in
        let keywords_rev = ref [] in
        let heading_stack = ref [] in
        let current_heading_id = ref None in

        let headings_by_id : heading_builder Int.Table.t = Int.Table.create () in
        let heading_order_rev = ref [] in
        let next_heading_id = ref 0 in

        let planning_rev = ref [] in
        let properties_rev = ref [] in
        let drawers_rev = ref [] in
        let blocks_rev = ref [] in
        let links_rev = ref [] in
        let tables_rev = ref [] in

        let open_drawer_state : open_drawer option ref = ref None in
        let open_block_state : open_block option ref = ref None in
        let open_table_state : open_table option ref = ref None in

        let syntax_error ~line detail =
          Error
            { path; reason = Diagnostic.Syntax_error; line = Some line; detail }
        in
        let finalize_table end_line =
          match !open_table_state with
          | None -> ()
          | Some table_state ->
              let rows = List.rev table_state.rows_rev in
              let source =
                make_source_ref ~path ~start_line:table_state.start_line ~end_line
              in
              tables_rev :=
                { rows; heading_id = table_state.heading_id; source } :: !tables_rev;
              open_table_state := None
        in

        let add_links ~line ~line_no ~heading_id =
          let bracket_links = extract_bracket_links line in
          List.iter bracket_links ~f:(fun (target, description) ->
              if not (String.is_empty target) then
                let source =
                  make_source_ref ~path ~start_line:line_no ~end_line:line_no
                in
                links_rev :=
                  {
                    kind = Bracket;
                    target;
                    description;
                    heading_id;
                    source;
                  }
                  :: !links_rev);
          let plain_links = extract_plain_links line in
          List.iter plain_links ~f:(fun target ->
              let source =
                make_source_ref ~path ~start_line:line_no ~end_line:line_no
              in
              links_rev :=
                {
                  kind = Plain;
                  target;
                  description = None;
                  heading_id;
                  source;
                }
                :: !links_rev)
        in

        let add_heading ~line_no ~level ~raw_heading_body =
          let title, todo_keyword, priority, tags =
            parse_heading_parts ~todo_config:!todo_config ~raw_heading_body
          in
          let rec pop_stack () =
            match !heading_stack with
            | parent_id :: rest ->
                let parent = Hashtbl.find_exn headings_by_id parent_id in
                if parent.level < level then ()
                else (
                  heading_stack := rest;
                  pop_stack ())
            | [] -> ()
          in
          pop_stack ();
          let parent_id = List.hd !heading_stack in
          let heading_id = !next_heading_id in
          next_heading_id := heading_id + 1;
          let builder =
            {
              id = heading_id;
              level;
              raw_title = raw_heading_body;
              title;
              todo_keyword;
              priority;
              tags;
              start_line = line_no;
              end_line = line_count;
              parent_id;
              child_ids_rev = [];
            }
          in
          Hashtbl.set headings_by_id ~key:heading_id ~data:builder;
          heading_order_rev := heading_id :: !heading_order_rev;
          (match parent_id with
          | Some parent ->
              let parent_builder = Hashtbl.find_exn headings_by_id parent in
              parent_builder.child_ids_rev <- heading_id :: parent_builder.child_ids_rev
          | None -> ());
          heading_stack := heading_id :: !heading_stack;
          current_heading_id := Some heading_id
        in

        let add_planning_entries ~line ~line_no =
          match !current_heading_id with
          | None -> ()
          | Some heading_id ->
              let entries = parse_planning_on_line line in
              List.iter entries ~f:(fun (kind, raw_value) ->
                  let source =
                    make_source_ref ~path ~start_line:line_no ~end_line:line_no
                  in
                  planning_rev :=
                    { kind; raw_value; heading_id; source } :: !planning_rev)
        in

        for line_index = 0 to line_count - 1 do
          let line_no = line_index + 1 in
          let line = lines.(line_index) in

          match !open_block_state with
          | Some block_state ->
              (match parse_block_end line with
              | Some ending_kind
                when String.equal ending_kind
                       (block_kind_to_end_token block_state.kind)
                ->
                  let source =
                    make_source_ref ~path ~start_line:block_state.start_line
                      ~end_line:line_no
                  in
                  blocks_rev :=
                    {
                      kind = block_state.kind;
                      language = block_state.language;
                      heading_id = block_state.heading_id;
                      source;
                    }
                    :: !blocks_rev;
                  open_block_state := None
              | Some _ ->
                  raise
                    (Invalid_argument
                       (sprintf "line %d: mismatched block terminator" line_no))
              | None -> ())
          | None ->
              (match !open_drawer_state with
              | Some drawer_state ->
                  if is_drawer_end line then (
                    let source =
                      make_source_ref ~path ~start_line:drawer_state.start_line
                        ~end_line:line_no
                    in
                    drawers_rev :=
                      {
                        name = drawer_state.name;
                        heading_id = drawer_state.heading_id;
                        source;
                      }
                      :: !drawers_rev;
                    open_drawer_state := None)
                  else if String.Caseless.equal drawer_state.name "PROPERTIES" then
                    (match parse_property_line line with
                    | Some (key, value) ->
                        (match drawer_state.heading_id with
                        | Some heading_id ->
                            let source =
                              make_source_ref ~path ~start_line:line_no
                                ~end_line:line_no
                            in
                            properties_rev :=
                              { key; value; heading_id; source } :: !properties_rev
                        | None -> ())
                    | None -> ())
              | None ->
                  if is_table_line line then
                    (match !open_table_state with
                    | Some table_state ->
                        table_state.rows_rev <-
                          parse_table_row line :: table_state.rows_rev
                    | None ->
                        open_table_state :=
                          Some
                            {
                              start_line = line_no;
                              heading_id = !current_heading_id;
                              rows_rev = [ parse_table_row line ];
                            })
                  else (
                    finalize_table (line_no - 1);
                    match parse_keyword_line line with
                    | Some (key, value) ->
                        keywords_rev := (key, value) :: !keywords_rev;
                        if String.equal key "TODO" then
                          (match Todo_config.parse_from_keyword_value value with
                          | Some parsed -> todo_config := parsed
                          | None -> ())
                    | None ->
                        (match parse_block_begin line with
                        | Some (Unsupported block_kind) ->
                            raise
                              (Failure
                                 (sprintf "UNSUPPORTED:%d:%s" line_no block_kind))
                        | Some (Supported (kind, language)) ->
                            open_block_state :=
                              Some
                                {
                                  kind;
                                  language;
                                  start_line = line_no;
                                  heading_id = !current_heading_id;
                                }
                        | None ->
                            (match parse_drawer_open line with
                            | Some name ->
                                open_drawer_state :=
                                  Some
                                    {
                                      name;
                                      start_line = line_no;
                                      heading_id = !current_heading_id;
                                    }
                            | None ->
                                (match parse_heading_line line with
                                | Some (level, raw_heading_body) ->
                                    add_heading ~line_no ~level ~raw_heading_body
                                | None -> add_planning_entries ~line ~line_no);
                            add_links ~line ~line_no ~heading_id:!current_heading_id))))
        done;

        finalize_table line_count;

        (match !open_block_state with
        | Some block_state ->
            syntax_error ~line:block_state.start_line "unterminated block"
        | None ->
            match !open_drawer_state with
            | Some drawer_state ->
                syntax_error ~line:drawer_state.start_line "unterminated drawer"
            | None ->
                let heading_ids_in_order = List.rev !heading_order_rev in
                let heading_builders =
                  List.map heading_ids_in_order ~f:(Hashtbl.find_exn headings_by_id)
                in
                let heading_array = Array.of_list heading_builders in
                let heading_count = Array.length heading_array in
                for i = 0 to heading_count - 1 do
                  let current = heading_array.(i) in
                  let section_end = ref line_count in
                  let j = ref (i + 1) in
                  while !j < heading_count do
                    let candidate = heading_array.(!j) in
                    if candidate.level <= current.level then (
                      section_end := candidate.start_line - 1;
                      j := heading_count)
                    else incr j
                  done;
                  current.end_line <- Int.max current.start_line !section_end
                done;

                let headings =
                  List.map heading_builders ~f:(fun builder ->
                      let source =
                        make_source_ref ~path ~start_line:builder.start_line
                          ~end_line:builder.start_line
                      in
                      let section_source =
                        make_source_ref ~path ~start_line:builder.start_line
                          ~end_line:builder.end_line
                      in
                      {
                        id = builder.id;
                        level = builder.level;
                        raw_title = builder.raw_title;
                        title = builder.title;
                        todo_keyword = builder.todo_keyword;
                        priority = builder.priority;
                        tags = builder.tags;
                        source;
                        section_source;
                        parent_id = builder.parent_id;
                        child_ids = List.rev builder.child_ids_rev;
                      })
                in
                let sections =
                  List.map headings ~f:(fun heading ->
                      { heading_id = heading.id; source = heading.section_source })
                in
                let todos =
                  List.filter headings ~f:(fun heading ->
                      Option.is_some heading.todo_keyword)
                in
                Ok
                  {
                    path;
                    line_count;
                    keywords = List.rev !keywords_rev;
                    todo_config = !todo_config;
                    index =
                      {
                        headings;
                        sections;
                        todos;
                        planning = List.rev !planning_rev;
                        properties = List.rev !properties_rev;
                        drawers = List.rev !drawers_rev;
                        blocks = List.rev !blocks_rev;
                        links = List.rev !links_rev;
                        tables = List.rev !tables_rev;
                      };
                  })
      with
      | Failure message when String.is_prefix message ~prefix:"UNSUPPORTED:" ->
          (match String.split message ~on:':' with
          | [ _; line_text; block_kind ] ->
              let line = Int.of_string line_text in
              Error
                {
                  path;
                  reason = Diagnostic.Unsupported_construct;
                  line = Some line;
                  detail = sprintf "unsupported block construct %s" block_kind;
                }
          | _ ->
              Error
                {
                  path;
                  reason = Diagnostic.Internal_parser_error;
                  line = None;
                  detail = message;
                })
      | Invalid_argument message when String.is_prefix message ~prefix:"line " ->
          (match String.split message ~on:':' with
          | line_prefix :: detail_parts ->
              let line_text = String.drop_prefix line_prefix 5 in
              let line = Int.of_string line_text in
              let detail = String.concat ~sep:":" detail_parts |> String.strip in
              Error
                {
                  path;
                  reason = Diagnostic.Syntax_error;
                  line = Some line;
                  detail;
                }
          | _ ->
              Error
                {
                  path;
                  reason = Diagnostic.Internal_parser_error;
                  line = None;
                  detail = message;
                })
      | exn ->
          Error
            {
              path;
              reason = Diagnostic.Internal_parser_error;
              line = None;
              detail = Exn.to_string exn;
            }

  let parse_file path =
    try parse_string ~path (In_channel.read_all path)
    with
    | Sys_error message ->
        Error
          {
            path;
            reason = Diagnostic.Internal_parser_error;
            line = None;
            detail = message;
          }
end

module Cli = struct
  let run () = "oq: not implemented"
end
