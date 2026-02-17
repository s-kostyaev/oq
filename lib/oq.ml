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

module Query = struct
  module Ast = struct
    type comparison_op =
      | Eq
      | Ne
      | Lt
      | Le
      | Gt
      | Ge

    type literal =
      | String of string
      | Int of int
      | Float of float
      | Bool of bool

    type predicate =
      | Predicate_value of expr
      | Predicate_compare of {
          left : expr;
          op : comparison_op;
          right : expr;
        }
      | Predicate_and of predicate * predicate
      | Predicate_or of predicate * predicate

    and expr =
      | Path of string list
      | Literal of literal
      | Call of {
          name : string;
          args : expr list;
        }
      | Parenthesized of predicate

    type selector_arg =
      | Expr_arg of expr
      | Span_arg of int * int

    type selector = {
      name : string;
      args : selector_arg list;
    }

    type function_stage =
      | Filter of predicate
      | Map of expr

    type atom_stage =
      | Selector of selector
      | Function of function_stage

    type postfix =
      | Index of int
      | Slice of int option * int option
      | Field of string

    type stage = {
      atom_stage : atom_stage;
      postfixes : postfix list;
    }

    type t = stage list
  end

  type error = {
    message : string;
    position : int;
  }

  exception Parse_failure of error

  type state = {
    query : string;
    length : int;
    mutable index : int;
  }

  let make_state query = { query; length = String.length query; index = 0 }
  let is_eof state = state.index >= state.length

  let peek_char state =
    if is_eof state then None else Some state.query.[state.index]

  let current_position state = state.index + 1

  let failf state fmt =
    Printf.ksprintf
      (fun message ->
        raise (Parse_failure { message; position = current_position state }))
      fmt

  let char_at state offset =
    let index = state.index + offset in
    if index < 0 || index >= state.length then None else Some state.query.[index]

  let advance state = state.index <- state.index + 1

  let is_identifier_start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false

  let is_identifier_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

  let is_whitespace = function
    | ' ' | '\n' | '\r' | '\t' -> true
    | _ -> false

  let skip_whitespace state =
    while
      (match peek_char state with
      | Some char -> is_whitespace char
      | None -> false)
    do
      advance state
    done

  let consume_char state char =
    match peek_char state with
    | Some current when Char.equal current char ->
        advance state;
        true
    | _ -> false

  let expect_char state char =
    if not (consume_char state char) then failf state "expected %C" char

  let parse_identifier state =
    skip_whitespace state;
    match peek_char state with
    | Some char when is_identifier_start char ->
        let start = state.index in
        advance state;
        while
          match peek_char state with
          | Some next when is_identifier_char next ->
              advance state;
              true
          | _ -> false
        do
          ()
        done;
        String.sub state.query ~pos:start ~len:(state.index - start)
    | _ -> failf state "expected identifier"

  let parse_string_literal state =
    skip_whitespace state;
    let quote =
      match peek_char state with
      | Some '\'' ->
          advance state;
          '\''
      | Some '"' ->
          advance state;
          '"'
      | _ -> failf state "expected string literal"
    in
    let buffer = Buffer.create 16 in
    let rec loop () =
      match peek_char state with
      | None -> failf state "unterminated string literal"
      | Some char when Char.equal char quote ->
          advance state;
          Buffer.contents buffer
      | Some '\\' ->
          advance state;
          (match peek_char state with
          | None -> failf state "unterminated escape sequence in string literal"
          | Some escaped ->
              advance state;
              let resolved =
                match escaped with
                | '\\' -> '\\'
                | '/' -> '/'
                | 'n' -> '\n'
                | 'r' -> '\r'
                | 't' -> '\t'
                | '"' -> '"'
                | '\'' -> '\''
                | other ->
                    failf state
                      "unsupported escape sequence \\%C in string literal" other
              in
              Buffer.add_char buffer resolved;
              loop ())
      | Some char ->
          advance state;
          Buffer.add_char buffer char;
          loop ()
    in
    loop ()

  let try_parse_signed_int state =
    skip_whitespace state;
    let start = state.index in
    (match peek_char state with
    | Some '-' -> advance state
    | _ -> ());
    let digit_start = state.index in
    while
      match peek_char state with
      | Some char when is_digit char ->
          advance state;
          true
      | _ -> false
    do
      ()
    done;
    if digit_start = state.index then (
      state.index <- start;
      None)
    else
      let raw = String.sub state.query ~pos:start ~len:(state.index - start) in
      Some (Int.of_string raw)

  let parse_number_literal state =
    skip_whitespace state;
    let start = state.index in
    (match peek_char state with
    | Some '-' -> advance state
    | _ -> ());
    let int_digits_start = state.index in
    while
      match peek_char state with
      | Some char when is_digit char ->
          advance state;
          true
      | _ -> false
    do
      ()
    done;
    let int_digits = state.index - int_digits_start in
    let is_float =
      match peek_char state with
      | Some '.' ->
          advance state;
          let frac_start = state.index in
          while
            match peek_char state with
            | Some char when is_digit char ->
                advance state;
                true
            | _ -> false
          do
            ()
          done;
          if state.index = frac_start then
            failf state "malformed float literal (expected digits after decimal)";
          true
      | _ -> false
    in
    if int_digits = 0 then failf state "expected number literal";
    let raw = String.sub state.query ~pos:start ~len:(state.index - start) in
    if is_float then Ast.Literal (Float (Float.of_string raw))
    else Ast.Literal (Int (Int.of_string raw))

  let try_consume_keyword state keyword =
    skip_whitespace state;
    let start = state.index in
    let keyword_length = String.length keyword in
    if start + keyword_length > state.length then false
    else
      let candidate =
        String.sub state.query ~pos:start ~len:keyword_length
      in
      if not (String.equal candidate keyword) then false
      else
        match char_at state keyword_length with
        | Some char when is_identifier_char char -> false
        | _ ->
            state.index <- state.index + keyword_length;
            true

  let try_parse_comparison_op state =
    skip_whitespace state;
    match (peek_char state, char_at state 1) with
    | Some '=', Some '=' ->
        state.index <- state.index + 2;
        Some Ast.Eq
    | Some '!', Some '=' ->
        state.index <- state.index + 2;
        Some Ast.Ne
    | Some '<', Some '=' ->
        state.index <- state.index + 2;
        Some Ast.Le
    | Some '>', Some '=' ->
        state.index <- state.index + 2;
        Some Ast.Ge
    | Some '<', _ ->
        advance state;
        Some Ast.Lt
    | Some '>', _ ->
        advance state;
        Some Ast.Gt
    | _ -> None

  let rec parse_expr state =
    skip_whitespace state;
    match peek_char state with
    | Some '(' ->
        advance state;
        let predicate = parse_predicate state in
        skip_whitespace state;
        expect_char state ')';
        Ast.Parenthesized predicate
    | Some '.' -> parse_path state
    | Some '\'' | Some '"' ->
        let string = parse_string_literal state in
        Ast.Literal (Ast.String string)
    | Some ('0' .. '9' | '-') -> parse_number_literal state
    | Some char when is_identifier_start char ->
        let name = parse_identifier state in
        if String.equal name "true" then Ast.Literal (Ast.Bool true)
        else if String.equal name "false" then Ast.Literal (Ast.Bool false)
        else (
          skip_whitespace state;
          if consume_char state '(' then
            let args = parse_expr_arg_list state in
            Ast.Call { name; args }
          else failf state "unexpected identifier %S in expression" name)
    | Some other -> failf state "unexpected token %C in expression" other
    | None -> failf state "unexpected end of input while parsing expression"

  and parse_expr_arg_list state =
    skip_whitespace state;
    if consume_char state ')' then []
    else
      let rec gather acc =
        let expr = parse_expr state in
        skip_whitespace state;
        if consume_char state ',' then gather (expr :: acc)
        else (
          expect_char state ')';
          List.rev (expr :: acc))
      in
      gather []

  and parse_path state =
    expect_char state '.';
    let first = parse_identifier state in
    let rec gather parts =
      skip_whitespace state;
      if consume_char state '.' then
        let field = parse_identifier state in
        gather (field :: parts)
      else List.rev parts
    in
    Ast.Path (gather [ first ])

  and parse_predicate state = parse_disjunction state

  and parse_disjunction state =
    let left = parse_conjunction state in
    let rec consume current =
      if try_consume_keyword state "or" then
        let right = parse_conjunction state in
        consume (Ast.Predicate_or (current, right))
      else current
    in
    consume left

  and parse_conjunction state =
    let left = parse_comparison state in
    let rec consume current =
      if try_consume_keyword state "and" then
        let right = parse_comparison state in
        consume (Ast.Predicate_and (current, right))
      else current
    in
    consume left

  and parse_comparison state =
    let left = parse_expr state in
    match try_parse_comparison_op state with
    | Some op ->
        let right = parse_expr state in
        Ast.Predicate_compare { left; op; right }
    | None -> Ast.Predicate_value left

  let parse_selector_arg state =
    skip_whitespace state;
    let checkpoint = state.index in
    match try_parse_signed_int state with
    | Some first ->
        skip_whitespace state;
        if consume_char state ':' then
          (match try_parse_signed_int state with
          | Some second -> Ast.Span_arg (first, second)
          | None -> failf state "expected end line after ':' in span argument")
        else (
          state.index <- checkpoint;
          Ast.Expr_arg (parse_expr state))
    | None -> Ast.Expr_arg (parse_expr state)

  let parse_selector_args state =
    skip_whitespace state;
    if consume_char state ')' then []
    else
      let rec gather acc =
        let arg = parse_selector_arg state in
        skip_whitespace state;
        if consume_char state ',' then gather (arg :: acc)
        else (
          expect_char state ')';
          List.rev (arg :: acc))
      in
      gather []

  let selector_set =
    String.Set.of_list
      [
        "tree";
        "headings";
        "section";
        "section_contains";
        "sections";
        "code";
        "links";
        "tables";
        "search";
        "text";
        "length";
        "todos";
        "done";
        "properties";
        "property";
        "scheduled";
        "deadline";
        "closed";
        "tags";
      ]

  let regex_flag_set = Char.Set.of_list [ 'i'; 'm'; 's' ]

  let validate_regex_pattern_string state raw =
    if not (String.is_prefix raw ~prefix:"/") then ()
    else
      match String.rsplit2 raw ~on:'/' with
      | Some (prefix, flags) when String.is_prefix prefix ~prefix:"/" ->
          let pattern = String.drop_prefix prefix 1 in
          let seen_flags = Char.Hash_set.create () in
          String.iter flags ~f:(fun flag ->
              if not (Set.mem regex_flag_set flag) then
                failf state
                  "unsupported regex flags %S (allowed: i,m,s)" flags
              else if Hash_set.mem seen_flags flag then
                failf state "duplicate regex flag %C in %S" flag flags
              else Hash_set.add seen_flags flag);
          (try
             ignore (Re2.create_exn pattern : Re2.t)
           with
          | exn ->
              failf state "malformed regex pattern-string %S: %s" raw
                (Exn.to_string exn))
      | _ ->
          failf state
            "malformed regex pattern-string %S (expected /pattern/flags)" raw

  let arg_to_string = function
    | Ast.Expr_arg _ -> "expression"
    | Ast.Span_arg _ -> "span"

  let expect_string_literal_arg state selector index = function
    | Ast.Expr_arg (Ast.Literal (Ast.String value)) -> value
    | arg ->
        failf state "wrong type for .%s argument %d: expected string, got %s"
          selector index (arg_to_string arg)

  let expect_int_literal_arg state selector index = function
    | Ast.Expr_arg (Ast.Literal (Ast.Int value)) -> value
    | Ast.Expr_arg (Ast.Literal (Ast.Float _)) ->
        failf state "wrong type for .%s argument %d: expected integer, got float"
          selector index
    | arg ->
        failf state "wrong type for .%s argument %d: expected integer, got %s"
          selector index (arg_to_string arg)

  let expect_span_arg state selector index = function
    | Ast.Span_arg (start_line, end_line) -> (start_line, end_line)
    | arg ->
        failf state "wrong type for .%s argument %d: expected span start:end, got %s"
          selector index (arg_to_string arg)

  let ensure_no_args state selector args =
    if not (List.is_empty args) then
      failf state ".%s does not take arguments" selector

  let ensure_one_arg state selector args =
    if List.length args <> 1 then failf state ".%s expects exactly 1 argument" selector

  let ensure_zero_or_one_arg state selector args =
    if List.length args > 1 then
      failf state ".%s expects at most 1 argument" selector

  let validate_selector_args state selector args =
    match selector with
    | "tree" ->
        ensure_zero_or_one_arg state selector args;
        List.iteri args ~f:(fun index arg ->
            let mode = expect_string_literal_arg state selector (index + 1) arg in
            if
              not
                (List.mem [ "compact"; "preview"; "full" ] mode
                   ~equal:String.equal)
            then
              failf state
                "invalid .tree mode %S (supported: compact|preview|full)" mode)
    | "headings" ->
        ensure_zero_or_one_arg state selector args;
        List.iteri args ~f:(fun index arg ->
            ignore
              (expect_int_literal_arg state selector (index + 1) arg : int))
    | "section" ->
        if not (List.length args = 1 || List.length args = 2) then
          failf state ".section expects 1 or 2 arguments";
        ignore (expect_string_literal_arg state selector 1 (List.nth_exn args 0));
        if List.length args = 2 then
          ignore (expect_span_arg state selector 2 (List.nth_exn args 1))
    | "section_contains" ->
        ensure_one_arg state selector args;
        ignore (expect_string_literal_arg state selector 1 (List.hd_exn args))
    | "code" ->
        ensure_zero_or_one_arg state selector args;
        List.iteri args ~f:(fun index arg ->
            ignore (expect_string_literal_arg state selector (index + 1) arg))
    | "property" ->
        ensure_one_arg state selector args;
        ignore (expect_string_literal_arg state selector 1 (List.hd_exn args))
    | "search" ->
        ensure_one_arg state selector args;
        let raw = expect_string_literal_arg state selector 1 (List.hd_exn args) in
        validate_regex_pattern_string state raw
    | "scheduled" | "deadline" | "closed" ->
        ensure_zero_or_one_arg state selector args;
        List.iteri args ~f:(fun index arg ->
            ignore (expect_string_literal_arg state selector (index + 1) arg))
    | "sections" | "links" | "tables" | "text" | "length" | "todos" | "done"
    | "properties" | "tags" ->
        ensure_no_args state selector args
    | _ -> failf state "unknown selector .%s" selector

  let parse_selector_stage state =
    expect_char state '.';
    let name = parse_identifier state in
    if not (Set.mem selector_set name) then failf state "unknown selector .%s" name;
    skip_whitespace state;
    let args =
      if consume_char state '(' then parse_selector_args state else []
    in
    validate_selector_args state name args;
    Ast.Selector { name; args }

  let parse_function_stage state =
    let name = parse_identifier state in
    if String.equal name "length" then
      failf state "bad stage usage: bare `length` is invalid; use `.length`";
    skip_whitespace state;
    expect_char state '(';
    let parsed =
      match name with
      | "filter" ->
          let predicate = parse_predicate state in
          Ast.Filter predicate
      | "map" ->
          let expr = parse_expr state in
          Ast.Map expr
      | _ ->
          failf state "unknown function stage %S (supported: filter|map)" name
    in
    skip_whitespace state;
    expect_char state ')';
    Ast.Function parsed

  let parse_index_or_slice_postfix state =
    expect_char state '[';
    skip_whitespace state;
    let start_value = try_parse_signed_int state in
    skip_whitespace state;
    if consume_char state ':' then (
      let end_value = try_parse_signed_int state in
      skip_whitespace state;
      expect_char state ']';
      Ast.Slice (start_value, end_value))
    else
      match start_value with
      | Some index ->
          skip_whitespace state;
          expect_char state ']';
          Ast.Index index
      | None -> failf state "bad postfix usage: expected index integer inside []"

  let parse_field_postfix state =
    expect_char state '.';
    let field = parse_identifier state in
    Ast.Field field

  let parse_postfixes state =
    let rec gather acc =
      skip_whitespace state;
      match peek_char state with
      | Some '[' ->
          let postfix = parse_index_or_slice_postfix state in
          gather (postfix :: acc)
      | Some '.' ->
          let postfix = parse_field_postfix state in
          gather (postfix :: acc)
      | _ -> List.rev acc
    in
    gather []

  let parse_pipe_stage state =
    skip_whitespace state;
    let atom_stage =
      match peek_char state with
      | Some '.' -> parse_selector_stage state
      | Some '[' ->
          failf state
            "bad postfix usage: bare indexing stage is invalid (use it after a \
             selector/function stage)"
      | Some char when is_identifier_start char -> parse_function_stage state
      | Some char -> failf state "expected stage start, found %C" char
      | None -> failf state "unexpected end of input while parsing stage"
    in
    let postfixes = parse_postfixes state in
    { Ast.atom_stage; postfixes }

  let parse query =
    let state = make_state query in
    try
      skip_whitespace state;
      if is_eof state then
        Error { message = "empty query"; position = 1 }
      else
        let first_stage = parse_pipe_stage state in
        let rec gather acc =
          skip_whitespace state;
          if consume_char state '|' then
            let stage = parse_pipe_stage state in
            gather (stage :: acc)
          else List.rev acc
        in
        let stages = gather [ first_stage ] in
        skip_whitespace state;
        if not (is_eof state) then
          Error
            {
              message =
                sprintf "unexpected trailing token %C" state.query.[state.index];
              position = current_position state;
            }
        else Ok stages
    with
    | Parse_failure err -> Error err

  let parse_exn query =
    match parse query with
    | Ok ast -> ast
    | Error { message; position } ->
        failwithf "query parse error at %d: %s" position message ()
end

module Cli = struct
  let run () = "oq: not implemented"
end
