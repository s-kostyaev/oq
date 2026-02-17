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

    let append_unique base extras =
      List.fold extras ~init:base ~f:(fun acc item ->
          if List.mem acc item ~equal:String.equal then acc else acc @ [ item ])

    let merge left right =
      {
        open_states = append_unique left.open_states right.open_states;
        done_states = append_unique left.done_states right.done_states;
      }

    let is_todo_keyword_key key =
      String.equal key "TODO"
      || String.equal key "SEQ_TODO"
      || String.equal key "TYP_TODO"

    let normalize_state_token raw_token =
      let token = String.strip raw_token in
      if String.is_empty token then None
      else
        let core =
          match String.substr_index token ~pattern:"(" with
          | Some index when index > 0 -> String.prefix token index
          | Some _ -> ""
          | None -> token
        in
        let normalized = String.strip core in
        if String.is_empty normalized then None else Some normalized

    let parse_from_keyword_value value =
      let tokens text =
        String.split_on_chars (String.strip text) ~on:[ ' '; '\t'; '\r'; '\n' ]
        |> List.filter_map ~f:normalize_state_token
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
    | Export

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
    lines : string array;
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

  let strip_utf8_bom text =
    if
      String.length text >= 3
      && Char.equal text.[0] (Char.of_int_exn 0xEF)
      && Char.equal text.[1] (Char.of_int_exn 0xBB)
      && Char.equal text.[2] (Char.of_int_exn 0xBF)
    then String.drop_prefix text 3
    else text

  let whitespace_tokens text =
    String.split_on_chars (String.strip text) ~on:[ ' '; '\t'; '\r'; '\n' ]
    |> List.filter ~f:(fun token -> not (String.is_empty token))

  let parse_heading_line line =
    let line_length = String.length line in
    let rec count_stars index =
      if index < line_length && Char.equal line.[index] '*' then
        count_stars (index + 1)
      else index
    in
    let star_count = count_stars 0 in
    if star_count = 0 then None
    else if star_count >= line_length || not (Char.is_whitespace line.[star_count])
    then
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
    let tags_of token =
      String.split token ~on:':'
      |> List.filter ~f:(fun tag -> not (String.is_empty tag))
    in
    if is_tag_token trimmed then ("", tags_of trimmed)
    else
    let len = String.length trimmed in
    let rec find_separator index =
      if index < 0 then None
      else if Char.is_whitespace trimmed.[index] then Some index
      else find_separator (index - 1)
    in
    match find_separator (len - 1) with
    | None -> (trimmed, [])
    | Some separator_index ->
        let suffix =
          String.drop_prefix trimmed (separator_index + 1) |> String.strip
        in
        if is_tag_token suffix then
          let tags = tags_of suffix in
          (String.prefix trimmed separator_index |> String.rstrip, tags)
        else (trimmed, [])

  let is_priority_cookie token =
    String.length token = 4
    && Char.equal token.[0] '['
    && Char.equal token.[1] '#'
    && Char.equal token.[3] ']'

  let parse_heading_parts ~todo_config ~raw_heading_body =
    let title_without_tags, tags = split_title_and_tags raw_heading_body in
    let tokens = whitespace_tokens title_without_tags in
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
            let raw_key = String.drop_prefix left 2 in
            if not (String.equal raw_key (String.strip raw_key)) then None
            else
              let key = String.uppercase raw_key in
              if String.is_empty key then None
              else if String.exists key ~f:Char.is_whitespace then None
              else Some (key, String.strip right)

  let parse_link_abbrev_keyword_value value =
    match whitespace_tokens value with
    | [] -> None
    | abbrev :: _ ->
        if String.is_substring abbrev ~substring:":" then None
        else Some (String.lowercase abbrev)

  let is_comment_line line =
    let trimmed = String.lstrip line in
    String.is_prefix trimmed ~prefix:"#"
    && not (String.is_prefix trimmed ~prefix:"#+")

  let parse_drawer_open line =
    let trimmed = String.rstrip line in
    let len = String.length trimmed in
    let is_valid_drawer_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
      | _ -> false
    in
    let is_valid_drawer_name name =
      not (String.is_empty name) && String.for_all name ~f:is_valid_drawer_char
    in
    let is_known_drawer_name name =
      String.Caseless.equal name "PROPERTIES"
      || String.Caseless.equal name "LOGBOOK"
    in
    if len < 3 then None
    else if not (Char.equal trimmed.[0] ':' && Char.equal trimmed.[len - 1] ':')
    then None
    else
      let raw_name = String.sub trimmed ~pos:1 ~len:(len - 2) in
      let name = String.strip raw_name in
      if String.is_empty name then None
      else if not (String.equal raw_name name) then None
      else if String.exists name ~f:Char.is_whitespace then None
      else if not (is_valid_drawer_name name) then None
      else if not (is_known_drawer_name name) then None
      else if String.Caseless.equal name "END" then None
      else Some name

  let is_drawer_end line = String.Caseless.equal (String.strip line) ":END:"

  let parse_property_line line =
    let trimmed = String.rstrip line in
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
    | Opaque of string

  let parse_block_begin line =
    let trimmed = String.strip line in
    let upper = String.uppercase trimmed in
    let prefix = "#+BEGIN_" in
    let split_first_whitespace text =
      let length = String.length text in
      let rec find index =
        if index >= length then None
        else if Char.is_whitespace text.[index] then Some index
        else find (index + 1)
      in
      match find 0 with
      | None -> (String.strip text, "")
      | Some index ->
          let left = String.prefix text index |> String.strip in
          let right = String.drop_prefix text (index + 1) |> String.strip in
          (left, right)
    in
    if not (String.is_prefix upper ~prefix) then None
    else
      let after_prefix = String.drop_prefix trimmed (String.length prefix) in
      let kind_token, rest =
        let kind, trailing = split_first_whitespace after_prefix in
        (String.uppercase kind, trailing)
      in
      let parse_optional_primary_token text =
        match whitespace_tokens text |> List.hd with
        | Some token
          when (not (String.is_prefix token ~prefix:":"))
               && not (String.is_prefix token ~prefix:"-")
               && not (String.is_prefix token ~prefix:"+") ->
            Some token
        | _ -> None
      in
      match kind_token with
      | "SRC" ->
          let language = parse_optional_primary_token rest in
          Some (Supported (Src, language))
      | "EXAMPLE" -> Some (Supported (Example, None))
      | "QUOTE" -> Some (Supported (Quote, None))
      | "EXPORT" ->
          let backend = parse_optional_primary_token rest in
          Some (Supported (Export, backend))
      | _ -> Some (Opaque kind_token)

  let parse_block_end line =
    let trimmed = String.strip line in
    let upper = String.uppercase trimmed in
    let prefix = "#+END_" in
    if not (String.is_prefix upper ~prefix) then None
    else
      let kind =
        let remainder =
          String.drop_prefix upper (String.length prefix) |> String.strip
        in
        let length = String.length remainder in
        let rec find_whitespace index =
          if index >= length then None
          else if Char.is_whitespace remainder.[index] then Some index
          else find_whitespace (index + 1)
        in
        match find_whitespace 0 with
        | Some index -> String.prefix remainder index |> String.strip
        | None -> remainder
      in
      Some kind

  let parse_dynamic_block_begin line =
    let trimmed = String.strip line in
    String.is_prefix (String.uppercase trimmed) ~prefix:"#+BEGIN:"

  let is_dynamic_block_end line =
    let trimmed = String.strip line in
    String.is_prefix (String.uppercase trimmed) ~prefix:"#+END:"

  let block_kind_to_end_token = function
    | Src -> "SRC"
    | Example -> "EXAMPLE"
    | Quote -> "QUOTE"
    | Export -> "EXPORT"

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
    let is_leading_trim_char = function
      | '(' | '[' | '<' | ',' | ';' | ':' | '"' | '\'' -> true
      | _ -> false
    in
    let is_trailing_trim_char = function
      | '>' | ',' | '.' | ';' | ':' | '"' | '\'' | ')' | ']' | '!' | '?' ->
          true
      | _ -> false
    in
    let rec drop_leading index =
      if index >= String.length token then index
      else if is_leading_trim_char token.[index] then drop_leading (index + 1)
      else index
    in
    let start_index = drop_leading 0 in
    let initial =
      if start_index >= String.length token then ""
      else String.drop_prefix token start_index
    in
    let has_unmatched_closer text closer opener =
      String.count text ~f:(Char.equal closer)
      > String.count text ~f:(Char.equal opener)
    in
    let should_drop_trailing text char =
      if not (is_trailing_trim_char char) then false
      else
        match char with
        | ')' -> has_unmatched_closer text ')' '('
        | ']' -> has_unmatched_closer text ']' '['
        | _ -> true
    in
    let rec drop_trailing text =
      let len = String.length text in
      if len = 0 then text
      else
        let last = text.[len - 1] in
        if should_drop_trailing text last then
          drop_trailing (String.drop_suffix text 1)
        else text
    in
    drop_trailing initial

  let is_plain_link_type text =
    let is_scheme_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.' -> true
      | _ -> false
    in
    let length = String.length text in
    length > 0
    &&
    match text.[0] with
    | 'a' .. 'z' | 'A' .. 'Z' ->
        let rec loop index =
          if index >= length then true
          else if is_scheme_char text.[index] then loop (index + 1)
          else false
        in
        loop 1
    | _ -> false

  let is_uri_with_scheme token =
    match String.substr_index token ~pattern:"://" with
    | None -> false
    | Some scheme_end ->
        scheme_end > 0
        && is_plain_link_type (String.prefix token scheme_end)

  let is_known_plain_scheme token =
    let lower = String.lowercase token in
    let has_prefix prefix =
      String.is_prefix lower ~prefix
      && String.length lower > String.length prefix
    in
    has_prefix "file:"
    || has_prefix "file+sys:"
    || has_prefix "file+emacs:"
    || has_prefix "mailto:"
    || has_prefix "id:"
    || has_prefix "custom-id:"
    || has_prefix "news:"
    || has_prefix "shell:"
    || has_prefix "elisp:"
    || has_prefix "help:"
    || has_prefix "info:"
    || has_prefix "man:"
    || has_prefix "woman:"
    || has_prefix "calc:"
    || has_prefix "doi:"
    || has_prefix "attachment:"
    || has_prefix "coderef:"
    || has_prefix "irc:"
    || has_prefix "gnus:"
    || has_prefix "rmail:"
    || has_prefix "docview:"
    || has_prefix "bbdb:"
    || has_prefix "mhe:"
    || has_prefix "wl:"
    || has_prefix "vm:"
    || has_prefix "vm-imap:"

  let is_plain_file_path token =
    let has_prefix prefix =
      String.is_prefix token ~prefix
      && String.length token > String.length prefix
    in
    let is_windows_drive_path =
      String.length token > 3
      &&
      match token.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' ->
          Char.equal token.[1] ':'
          && (Char.equal token.[2] '/' || Char.equal token.[2] '\\')
      | _ -> false
    in
    let is_tilde_path =
      has_prefix "~/"
      ||
      (String.length token > 2
      && Char.equal token.[0] '~'
      &&
      match String.substr_index token ~pattern:"/" with
      | Some index -> index > 1
      | None -> false)
    in
    has_prefix "/"
    || has_prefix "./"
    || has_prefix "../"
    || is_tilde_path
    || is_windows_drive_path

  let is_custom_plain_scheme ~custom_link_types token =
    match String.lsplit2 token ~on:':' with
    | None -> false
    | Some (scheme, rest) ->
        not (String.is_empty rest)
        && is_plain_link_type scheme
        && Hash_set.mem custom_link_types (String.lowercase scheme)

  let extract_angle_links ~custom_link_types line =
    let rec loop position acc =
      match String.substr_index ~pos:position line ~pattern:"<" with
      | None -> List.rev acc
      | Some start_index ->
          let search_from = start_index + 1 in
          (match String.substr_index ~pos:search_from line ~pattern:">" with
          | None -> List.rev acc
          | Some end_index ->
              let candidate =
                String.sub line ~pos:search_from ~len:(end_index - search_from)
                |> String.strip
              in
              let acc =
                if String.is_empty candidate then acc
                else if
                  is_uri_with_scheme candidate
                  || is_known_plain_scheme candidate
                  || is_plain_file_path candidate
                  || is_custom_plain_scheme ~custom_link_types candidate
                then candidate :: acc
                else acc
              in
              loop (end_index + 1) acc)
    in
    loop 0 []

  let extract_plain_links ~custom_link_types line =
    whitespace_tokens line
    |> List.filter_map ~f:(fun token ->
           let looks_like_delimited_fragment =
             String.is_substring token ~substring:"[["
             || String.is_substring token ~substring:"]]"
             || String.is_substring token ~substring:"]["
             || String.is_substring token ~substring:"<"
             || String.is_substring token ~substring:">"
           in
           if looks_like_delimited_fragment then None
           else
             let normalized = trim_plain_link_token token in
             if
               is_uri_with_scheme normalized
               || is_known_plain_scheme normalized
               || is_plain_file_path normalized
               || is_custom_plain_scheme ~custom_link_types normalized
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
    let trimmed = String.lstrip line in
    let normalized = String.uppercase trimmed in
    let starts_with_planning_keyword =
      String.is_prefix normalized ~prefix:"SCHEDULED:"
      || String.is_prefix normalized ~prefix:"DEADLINE:"
      || String.is_prefix normalized ~prefix:"CLOSED:"
    in
    if not starts_with_planning_keyword then []
    else
    let markers =
      [ (Scheduled, "SCHEDULED:"); (Deadline, "DEADLINE:"); (Closed, "CLOSED:") ]
      |> List.filter_map ~f:(fun (kind, pattern) ->
             Option.map (String.substr_index normalized ~pattern) ~f:(fun index ->
                 (kind, index, String.length pattern)))
      |> List.sort ~compare:(fun (_, left, _) (_, right, _) ->
             Int.compare left right)
    in
    let rec collect acc = function
      | [] -> List.rev acc
      | (kind, start_index, token_length) :: rest ->
          let value_start = start_index + token_length in
          let value_end =
            match rest with
            | (_, next_index, _) :: _ -> next_index
            | [] -> String.length trimmed
          in
          if value_end <= value_start then collect acc rest
          else
            let value =
              String.sub trimmed ~pos:value_start ~len:(value_end - value_start)
              |> String.strip
            in
            if String.is_empty value then collect acc rest
            else collect ((kind, value) :: acc) rest
    in
    collect [] markers

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

  type open_block =
    | Open_supported of {
        kind : block_kind;
        language : string option;
        start_line : int;
        heading_id : int option;
      }
    | Open_opaque of {
        expected_end_token : string;
        start_line : int;
      }
    | Open_dynamic of {
        start_line : int;
      }

  let open_block_end_token = function
    | Open_supported block_state -> block_kind_to_end_token block_state.kind
    | Open_opaque block_state -> block_state.expected_end_token
    | Open_dynamic _ -> failwith "internal error: dynamic block has dedicated end token"

  let open_block_start_line = function
    | Open_supported block_state -> block_state.start_line
    | Open_opaque block_state -> block_state.start_line
    | Open_dynamic block_state -> block_state.start_line

  type open_table = {
    start_line : int;
    heading_id : int option;
    mutable rows_rev : string list list;
  }

  let collect_link_abbreviations lines =
    let custom_link_types = String.Hash_set.create () in
    let line_count = Array.length lines in
    let open_drawer_state : open_drawer option ref = ref None in
    let open_block_state : open_block option ref = ref None in
    for line_index = 0 to line_count - 1 do
      let line = lines.(line_index) in
      match !open_block_state with
      | Some block_state ->
          (match block_state with
          | Open_dynamic _ ->
              if is_dynamic_block_end line then open_block_state := None
          | Open_supported _ | Open_opaque _ -> (
              match parse_block_end line with
              | Some ending_kind
                when String.equal ending_kind (open_block_end_token block_state) ->
                  open_block_state := None
              | Some _ -> ()
              | None -> ()))
      | None ->
          (match !open_drawer_state with
          | Some _ ->
              if is_drawer_end line then open_drawer_state := None else ()
          | None ->
              if is_table_line line then ()
              else if parse_dynamic_block_begin line then
                open_block_state := Some (Open_dynamic { start_line = line_index + 1 })
              else
                match parse_keyword_line line with
                | Some (key, value) ->
                    if String.equal key "LINK" then
                      (match parse_link_abbrev_keyword_value value with
                      | Some abbrev -> Hash_set.add custom_link_types abbrev
                      | None -> ())
                | None ->
                    if is_comment_line line then ()
                    else
                      match parse_block_begin line with
                      | Some (Supported (kind, language)) ->
                          open_block_state :=
                            Some
                              (Open_supported
                                 {
                                   kind;
                                   language;
                                   start_line = line_index + 1;
                                   heading_id = None;
                                 })
                      | Some (Opaque kind_token) ->
                          open_block_state :=
                            Some
                              (Open_opaque
                                 {
                                   expected_end_token = kind_token;
                                   start_line = line_index + 1;
                                 })
                      | None -> (
                          match parse_drawer_open line with
                          | Some name ->
                              open_drawer_state :=
                                Some
                                  {
                                    name;
                                    start_line = line_index + 1;
                                    heading_id = None;
                                  }
                          | None -> ()))
    done;
    custom_link_types

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
        let normalized_content = strip_utf8_bom content in
        let lines = String.split_lines normalized_content |> Array.of_list in
        let line_count = Array.length lines in
        let todo_config = ref Todo_config.default in
        let has_explicit_todo_config = ref false in
        let custom_link_types = collect_link_abbreviations lines in
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
          let plain_links =
            extract_angle_links ~custom_link_types line
            @ extract_plain_links ~custom_link_types line
          in
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
              (match block_state with
              | Open_dynamic _ ->
                  if is_dynamic_block_end line then open_block_state := None
              | Open_supported _ | Open_opaque _ -> (
                  match parse_block_end line with
                  | Some ending_kind
                    when String.equal ending_kind
                           (open_block_end_token block_state) ->
                      (match block_state with
                      | Open_supported supported ->
                          let source =
                            make_source_ref ~path ~start_line:supported.start_line
                              ~end_line:line_no
                          in
                          blocks_rev :=
                            {
                              kind = supported.kind;
                              language = supported.language;
                              heading_id = supported.heading_id;
                              source;
                            }
                            :: !blocks_rev
                      | Open_opaque _ -> ()
                      | Open_dynamic _ -> assert false);
                      open_block_state := None
                  | Some _ -> ()
                  | None -> ()))
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
                    if parse_dynamic_block_begin line then
                      open_block_state := Some (Open_dynamic { start_line = line_no })
                    else
                      match parse_keyword_line line with
                      | Some (key, value) ->
                          keywords_rev := (key, value) :: !keywords_rev;
                          if Todo_config.is_todo_keyword_key key then
                            (match Todo_config.parse_from_keyword_value value with
                            | Some parsed ->
                                if !has_explicit_todo_config then
                                  todo_config := Todo_config.merge !todo_config parsed
                                else (
                                  todo_config := parsed;
                                  has_explicit_todo_config := true)
                            | None -> ())
                          else if String.equal key "LINK" then
                            (match parse_link_abbrev_keyword_value value with
                            | Some abbrev ->
                                Hash_set.add custom_link_types abbrev
                            | None -> ())
                      | None ->
                        if is_comment_line line then ()
                        else
                          (match parse_block_begin line with
                          | Some (Supported (kind, language)) ->
                              open_block_state :=
                                Some
                                  (Open_supported
                                     {
                                       kind;
                                       language;
                                       start_line = line_no;
                                       heading_id = !current_heading_id;
                                     })
                          | Some (Opaque kind_token) ->
                              open_block_state :=
                                Some
                                  (Open_opaque
                                     {
                                       expected_end_token = kind_token;
                                       start_line = line_no;
                                     })
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
                              add_links ~line ~line_no
                                ~heading_id:!current_heading_id))))
        done;

        finalize_table line_count;

        (match !open_block_state with
        | Some block_state ->
            syntax_error ~line:(open_block_start_line block_state)
              "unterminated block"
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
                    lines;
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
        ensure_no_args state selector args
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

module Eval = struct
  open Org

  module Ast = Query.Ast

  type error =
    | Query_parse_error of Query.error
    | Eval_error of string

  exception Failure of string

  let failf fmt = Printf.ksprintf (fun message -> raise (Failure message)) fmt

  module Value = struct
    type section = {
      heading : Org.heading;
      source : Org.Source_ref.t;
    }

    type t =
      | Document of Org.t
      | Heading of Org.heading
      | Tree_heading of Org.heading
      | Section of section
      | Property of Org.property
      | Planning of Org.planning_entry
      | Block of Org.block
      | Link of Org.link
      | Table of Org.table
      | String of string
      | Int of int
      | Float of float
      | Bool of bool
      | Null
      | List of t list
  end

  module Date_context = struct
    type range =
      | Today
      | Tomorrow
      | Yesterday
      | This_week
      | Next_7d
      | Overdue

    type t = {
      zone : Time_float_unix.Zone.t;
      now : Time_float_unix.t;
      day_start_date : Date.t;
      day_start : Time_float_unix.t;
      week_start_date : Date.t;
      week_start : Time_float_unix.t;
    }

    let midnight = Time_float.Ofday.of_string "00:00:00"

    let strict_now_pattern =
      Re2.create_exn
        "^([0-9]{4}-[0-9]{2}-[0-9]{2})T([0-9]{2}:[0-9]{2}:[0-9]{2}(?:\\.[0-9]+)?)(Z|[+-][0-9]{2}:[0-9]{2})$"

    let offset_suffix_pattern = Re2.create_exn "(Z|[+-][0-9]{2}:[0-9]{2})$"

    let invalid_now now_value =
      failf
        "invalid --now value %S (expected RFC3339 with explicit offset, for \
         example 2026-02-17T10:30:00-08:00)"
        now_value

    let normalize_offset offset =
      if String.equal offset "Z" then "+00:00" else offset

    let zone_from_tz = function
      | None -> Lazy.force Time_float_unix.Zone.local
      | Some tz -> (
          match Time_float_unix.Zone.find tz with
          | Some zone -> zone
          | None ->
              failf "invalid --tz value %S (expected an IANA timezone)" tz)

    let parse_now_parts now_value =
      if not (Re2.matches strict_now_pattern now_value) then invalid_now now_value;
      let groups = Re2.find_submatches_exn strict_now_pattern now_value in
      match groups with
      | [| Some _; Some date; Some ofday; Some offset |] ->
          (date, ofday, normalize_offset offset)
      | _ -> invalid_now now_value

    let parse_absolute_now now_value =
      if not (Re2.matches strict_now_pattern now_value) then invalid_now now_value;
      try Time_float_unix.of_string_abs now_value
      with
      | _ -> invalid_now now_value

    let offset_of_time ~zone time =
      let rendered = Time_float_unix.to_string_abs_parts ~zone time in
      match rendered with
      | _date :: ofday_with_offset :: _ -> (
          let groups = Re2.find_submatches_exn offset_suffix_pattern ofday_with_offset in
          match groups with
          | [| Some _; Some offset |] -> normalize_offset offset
          | _ ->
              failf
                "internal error: could not extract timezone offset from %S"
                ofday_with_offset)
      | _ ->
          failf
            "internal error: unexpected Time_float_unix.to_string_abs_parts shape"

    let parse_local_civil_now ~zone now_value =
      let date_text, ofday_text, provided_offset = parse_now_parts now_value in
      let date =
        try Date.of_string date_text
        with
        | _ -> invalid_now now_value
      in
      let ofday =
        try Time_float.Ofday.of_string ofday_text
        with
        | _ -> invalid_now now_value
      in
      let anchored = Time_float_unix.of_date_ofday ~zone date ofday in
      let expected_offset = offset_of_time ~zone anchored in
      if not (String.equal expected_offset provided_offset) then
        failf
          "offset mismatch between --now (%s) and --tz (%s) at local datetime \
           %sT%s"
          provided_offset
          (Time_float_unix.Zone.to_string zone)
          date_text ofday_text;
      anchored

    let monday_start date =
      let weekday = Date.day_of_week date in
      let days_since_monday = Day_of_week.iso_8601_weekday_number weekday - 1 in
      Date.add_days date (-days_since_monday)

    let create ?now ?tz () =
      let zone = zone_from_tz tz in
      let now_time =
        match (now, tz) with
        | None, _ -> Time_float_unix.now ()
        | Some now_value, None -> parse_absolute_now now_value
        | Some now_value, Some _ -> parse_local_civil_now ~zone now_value
      in
      let day_start_date = Date.of_time now_time ~zone in
      let day_start = Time_float_unix.of_date_ofday ~zone day_start_date midnight in
      let week_start_date = monday_start day_start_date in
      let week_start = Time_float_unix.of_date_ofday ~zone week_start_date midnight in
      { zone; now = now_time; day_start_date; day_start; week_start_date; week_start }

    let parse_range ~selector ~allow_overdue range =
      match range with
      | "today" -> Today
      | "tomorrow" -> Tomorrow
      | "yesterday" -> Yesterday
      | "this_week" -> This_week
      | "next_7d" -> Next_7d
      | "overdue" when allow_overdue -> Overdue
      | "overdue" ->
          failf
            "invalid .%s range %S (supported: \
             today|tomorrow|yesterday|this_week|next_7d)"
            selector range
      | _ ->
          failf
            "invalid .%s range %S (supported: \
             today|tomorrow|yesterday|this_week|next_7d%s)"
            selector range
            (if allow_overdue then "|overdue" else "")

    let time_of_date t date = Time_float_unix.of_date_ofday ~zone:t.zone date midnight

    let range_bounds t = function
      | Today ->
          let stop = time_of_date t (Date.add_days t.day_start_date 1) in
          (t.day_start, stop)
      | Tomorrow ->
          let start = time_of_date t (Date.add_days t.day_start_date 1) in
          let stop = time_of_date t (Date.add_days t.day_start_date 2) in
          (start, stop)
      | Yesterday ->
          let start = time_of_date t (Date.add_days t.day_start_date (-1)) in
          (start, t.day_start)
      | This_week ->
          let stop = time_of_date t (Date.add_days t.week_start_date 7) in
          (t.week_start, stop)
      | Next_7d ->
          let stop = time_of_date t (Date.add_days t.day_start_date 7) in
          (t.day_start, stop)
      | Overdue -> failf "internal error: overdue has no closed interval bounds"
  end

  module Runtime = struct
    type t = {
      doc : Org.t;
      section_text_by_heading : string Int.Table.t;
      heading_by_id : Org.heading Int.Table.t;
      date_context : Date_context.t;
    }

    let create ?now ?tz doc =
      let heading_by_id = Int.Table.create () in
      List.iter doc.index.headings ~f:(fun heading ->
          Hashtbl.set heading_by_id ~key:heading.id ~data:heading);
      {
        doc;
        section_text_by_heading = Int.Table.create ();
        heading_by_id;
        date_context = Date_context.create ?now ?tz ();
      }

    let text_for_span doc (span : Org.Span.t) =
      let start_index = Int.max 0 (span.start_line - 1) in
      let end_index = Int.min (Array.length doc.Org.lines) span.end_line in
      if end_index <= start_index then ""
      else (
        let buffer = Buffer.create 256 in
        for line_index = start_index to end_index - 1 do
          if line_index > start_index then Buffer.add_char buffer '\n';
          Buffer.add_string buffer doc.Org.lines.(line_index)
        done;
        Buffer.contents buffer)

    let section_text t (heading : Org.heading) =
      match Hashtbl.find t.section_text_by_heading heading.id with
      | Some cached -> cached
      | None ->
          let text = text_for_span t.doc heading.section_source.span in
          Hashtbl.set t.section_text_by_heading ~key:heading.id ~data:text;
          text
  end

  let section_of_heading (heading : Org.heading) =
    Value.Section { heading; source = heading.section_source }

  let canonical_section_candidate (heading : Org.heading) :
      Diagnostic.section_candidate =
    {
      title = heading.title;
      start_line = heading.section_source.span.start_line;
      end_line = heading.section_source.span.end_line;
    }

  let type_name = function
    | Value.Document _ -> "document"
    | Value.Heading _ -> "heading"
    | Value.Tree_heading _ -> "heading"
    | Value.Section _ -> "section"
    | Value.Property _ -> "property"
    | Value.Planning _ -> "planning"
    | Value.Block _ -> "block"
    | Value.Link _ -> "link"
    | Value.Table _ -> "table"
    | Value.String _ -> "string"
    | Value.Int _ -> "int"
    | Value.Float _ -> "float"
    | Value.Bool _ -> "bool"
    | Value.Null -> "null"
    | Value.List _ -> "list"

  let as_list = function
    | Value.List values -> values
    | value -> failf "wrong type: expected list, got %s" (type_name value)

  let as_string = function
    | Value.String text -> text
    | value -> failf "wrong type: expected string, got %s" (type_name value)

  let as_bool = function
    | Value.Bool flag -> flag
    | value -> failf "wrong type: expected boolean, got %s" (type_name value)

  let selector_arg_expr_exn name index = function
    | Ast.Expr_arg expr -> expr
    | Ast.Span_arg _ ->
        failf "wrong type for .%s argument %d: expected expression, got span" name
          index

  let selector_arg_string_exn name index arg =
    match selector_arg_expr_exn name index arg with
    | Ast.Literal (Ast.String text) -> text
    | _ ->
        failf "wrong type for .%s argument %d: expected string literal" name index

  let selector_arg_int_exn name index arg =
    match selector_arg_expr_exn name index arg with
    | Ast.Literal (Ast.Int value) -> value
    | _ ->
        failf "wrong type for .%s argument %d: expected integer literal" name index

  let selector_arg_span_exn name index = function
    | Ast.Span_arg (start_line, end_line) -> (start_line, end_line)
    | _ ->
        failf "wrong type for .%s argument %d: expected span start:end" name index

  let planning_kind_to_string = function
    | Org.Scheduled -> "scheduled"
    | Org.Deadline -> "deadline"
    | Org.Closed -> "closed"

  let block_kind_to_string = function
    | Org.Src -> "src"
    | Org.Example -> "example"
    | Org.Quote -> "quote"
    | Org.Export -> "export"

  let link_kind_to_string = function
    | Org.Bracket -> "bracket"
    | Org.Plain -> "plain"

  let source_field_value (source : Org.Source_ref.t) field =
    match field with
    | "path" -> Value.String source.path
    | "start_line" -> Value.Int source.span.start_line
    | "end_line" -> Value.Int source.span.end_line
    | _ -> failf "unknown source field .%s" field

  let heading_by_id runtime heading_id =
    Hashtbl.find_exn runtime.Runtime.heading_by_id heading_id

  let parse_section_title_query_prefix raw_title =
    let trimmed = String.strip raw_title in
    let length = String.length trimmed in
    let rec consume_stars index =
      if index < length && Char.equal trimmed.[index] '*' then
        consume_stars (index + 1)
      else index
    in
    let star_count = consume_stars 0 in
    if
      star_count > 0 && star_count < length
      && Char.is_whitespace trimmed.[star_count]
    then
      let rec consume_whitespace index =
        if index < length && Char.is_whitespace trimmed.[index] then
          consume_whitespace (index + 1)
        else index
      in
      let title_start = consume_whitespace star_count in
      if title_start >= length then (None, trimmed)
      else
        (Some star_count, String.drop_prefix trimmed title_start |> String.strip)
    else (None, trimmed)

  let parse_section_title_query_span title =
    let parse_int text =
      try Some (Int.of_string text) with
      | _ -> None
    in
    match String.rsplit2 title ~on:'(' with
    | Some (before, after) when String.is_suffix after ~suffix:")" ->
        let inside =
          String.drop_suffix after 1 |> String.strip
        in
        let base_title = String.rstrip before in
        (match String.chop_prefix inside ~prefix:"lines " with
        | Some raw_span when not (String.is_empty base_title) -> (
            match String.lsplit2 (String.strip raw_span) ~on:':' with
            | Some (raw_start, raw_end) -> (
                match (parse_int (String.strip raw_start), parse_int (String.strip raw_end)) with
                | Some start_line, Some end_line ->
                    (base_title, Some (start_line, end_line))
                | _ -> (title, None))
            | None -> (title, None))
        | _ -> (title, None))
    | _ -> (title, None)

  let heading_field_value runtime (heading : Org.heading) field =
    match field with
    | "id" -> Value.Int heading.id
    | "level" -> Value.Int heading.level
    | "raw_title" -> Value.String heading.raw_title
    | "title" -> Value.String heading.title
    | "state" -> (
        match heading.todo_keyword with
        | Some state -> Value.String state
        | None -> Value.Null)
    | "priority" -> (
        match heading.priority with
        | Some priority -> Value.String (String.of_char priority)
        | None -> Value.Null)
    | "tags" -> Value.List (List.map heading.tags ~f:(fun tag -> Value.String tag))
    | "path" -> Value.String heading.source.path
    | "start_line" -> Value.Int heading.section_source.span.start_line
    | "end_line" -> Value.Int heading.section_source.span.end_line
    | "text" -> Value.String (Runtime.section_text runtime heading)
    | _ -> failf "unknown field .%s for heading" field

  let field_value runtime value field =
    let property_field_value (property : Org.property) =
      match field with
      | "key" -> Value.String property.key
      | "value" -> Value.String property.value
      | "heading_id" -> Value.Int property.heading_id
      | "heading_title" ->
          let heading = heading_by_id runtime property.heading_id in
          Value.String heading.title
      | "text" ->
          Value.String
            (Runtime.text_for_span runtime.Runtime.doc property.source.span)
      | "path" | "start_line" | "end_line" ->
          source_field_value property.source field
      | _ -> failf "unknown field .%s for property" field
    in
    let planning_field_value (planning : Org.planning_entry) =
      match field with
      | "kind" -> Value.String (planning_kind_to_string planning.kind)
      | "value" -> Value.String planning.raw_value
      | "heading_id" -> Value.Int planning.heading_id
      | "heading_title" ->
          let heading = heading_by_id runtime planning.heading_id in
          Value.String heading.title
      | "text" ->
          Value.String
            (Runtime.text_for_span runtime.Runtime.doc planning.source.span)
      | "path" | "start_line" | "end_line" ->
          source_field_value planning.source field
      | _ -> failf "unknown field .%s for planning" field
    in
    let block_field_value (block : Org.block) =
      match field with
      | "kind" -> Value.String (block_kind_to_string block.kind)
      | "language" -> (
          match block.language with
          | Some language -> Value.String language
          | None -> Value.Null)
      | "heading_id" -> (
          match block.heading_id with
          | Some heading_id -> Value.Int heading_id
          | None -> Value.Null)
      | "text" ->
          Value.String (Runtime.text_for_span runtime.Runtime.doc block.source.span)
      | "path" | "start_line" | "end_line" ->
          source_field_value block.source field
      | _ -> failf "unknown field .%s for block" field
    in
    let link_field_value (link : Org.link) =
      match field with
      | "kind" -> Value.String (link_kind_to_string link.kind)
      | "target" -> Value.String link.target
      | "description" -> (
          match link.description with
          | Some description -> Value.String description
          | None -> Value.Null)
      | "heading_id" -> (
          match link.heading_id with
          | Some heading_id -> Value.Int heading_id
          | None -> Value.Null)
      | "text" ->
          Value.String (Runtime.text_for_span runtime.Runtime.doc link.source.span)
      | "path" | "start_line" | "end_line" ->
          source_field_value link.source field
      | _ -> failf "unknown field .%s for link" field
    in
    let table_field_value (table : Org.table) =
      match field with
      | "rows" ->
          Value.List
            (List.map table.rows ~f:(fun row ->
                 Value.List (List.map row ~f:(fun cell -> Value.String cell))))
      | "row_count" -> Value.Int (List.length table.rows)
      | "heading_id" -> (
          match table.heading_id with
          | Some heading_id -> Value.Int heading_id
          | None -> Value.Null)
      | "text" ->
          Value.String (Runtime.text_for_span runtime.Runtime.doc table.source.span)
      | "path" | "start_line" | "end_line" ->
          source_field_value table.source field
      | _ -> failf "unknown field .%s for table" field
    in
    let section_field_value (section : Value.section) =
      match field with
      | "source_start_line" -> Value.Int section.source.span.start_line
      | "source_end_line" -> Value.Int section.source.span.end_line
      | _ -> heading_field_value runtime section.heading field
    in
    match value with
    | Value.Heading heading -> heading_field_value runtime heading field
    | Value.Tree_heading heading -> heading_field_value runtime heading field
    | Value.Section section -> section_field_value section
    | Value.Property property -> property_field_value property
    | Value.Planning planning -> planning_field_value planning
    | Value.Block block -> block_field_value block
    | Value.Link link -> link_field_value link
    | Value.Table table -> table_field_value table
    | _ -> failf "unknown field .%s for %s" field (type_name value)

  let float_of_numeric = function
    | Value.Int value -> Float.of_int value
    | Value.Float value -> value
    | value -> failf "wrong type: expected numeric value, got %s" (type_name value)

  let rec eval_comparison op left right =
    let compare_numbers ~f =
      let left_number = float_of_numeric left in
      let right_number = float_of_numeric right in
      Value.Bool (f left_number right_number)
    in
    match op with
    | Ast.Eq -> (
        match (left, right) with
        | Value.Int l, Value.Int r -> Value.Bool (Int.equal l r)
        | (Value.Int _ | Value.Float _), (Value.Int _ | Value.Float _) ->
            compare_numbers ~f:Float.equal
        | Value.String l, Value.String r -> Value.Bool (String.equal l r)
        | Value.Bool l, Value.Bool r -> Value.Bool (Bool.equal l r)
        | Value.Null, Value.Null -> Value.Bool true
        | _ -> Value.Bool false)
    | Ast.Ne -> (
        match eval_comparison Ast.Eq left right with
        | Value.Bool flag -> Value.Bool (not flag)
        | _ -> assert false)
    | Ast.Lt -> (
        match (left, right) with
        | (Value.Int _ | Value.Float _), (Value.Int _ | Value.Float _) ->
            compare_numbers ~f:(fun left_value right_value ->
                Float.(left_value < right_value))
        | Value.String left_text, Value.String right_text ->
            Value.Bool (String.compare left_text right_text < 0)
        | _ ->
            failf "wrong type: < supports only numeric or string comparisons")
    | Ast.Le -> (
        match (left, right) with
        | (Value.Int _ | Value.Float _), (Value.Int _ | Value.Float _) ->
            compare_numbers ~f:(fun left_value right_value ->
                Float.(left_value <= right_value))
        | Value.String left_text, Value.String right_text ->
            Value.Bool (String.compare left_text right_text <= 0)
        | _ ->
            failf "wrong type: <= supports only numeric or string comparisons")
    | Ast.Gt -> (
        match (left, right) with
        | (Value.Int _ | Value.Float _), (Value.Int _ | Value.Float _) ->
            compare_numbers ~f:(fun left_value right_value ->
                Float.(left_value > right_value))
        | Value.String left_text, Value.String right_text ->
            Value.Bool (String.compare left_text right_text > 0)
        | _ ->
            failf "wrong type: > supports only numeric or string comparisons")
    | Ast.Ge -> (
        match (left, right) with
        | (Value.Int _ | Value.Float _), (Value.Int _ | Value.Float _) ->
            compare_numbers ~f:(fun left_value right_value ->
                Float.(left_value >= right_value))
        | Value.String left_text, Value.String right_text ->
            Value.Bool (String.compare left_text right_text >= 0)
        | _ ->
            failf "wrong type: >= supports only numeric or string comparisons")

  let compile_regex pattern flags =
    let options =
      {
        Re2.Options.default with
        case_sensitive = not (String.contains flags 'i');
        one_line = not (String.contains flags 'm');
        dot_nl = String.contains flags 's';
      }
    in
    Re2.create_exn ~options pattern

  let compile_literal_search pattern =
    let options = { Re2.Options.default with case_sensitive = false } in
    Re2.create_exn ~options (Re2.escape pattern)

  let parse_regex_literal raw =
    if not (String.is_prefix raw ~prefix:"/") then None
    else
      match String.rsplit2 raw ~on:'/' with
      | Some (prefix, flags) when String.is_prefix prefix ~prefix:"/" ->
          Some (String.drop_prefix prefix 1, flags)
      | _ -> None

  let timestamp_time_pattern =
    Re2.create_exn
      "^[0-9]{2}:[0-9]{2}(?::[0-9]{2})?(?:-[0-9]{2}:[0-9]{2}(?::[0-9]{2})?)?$"

  let parse_org_timestamp ~zone raw_value =
    let trimmed = String.strip raw_value in
    let content =
      if String.is_empty trimmed then None
      else
        let close_char =
          match String.get trimmed 0 with
          | '<' -> Some '>'
          | '[' -> Some ']'
          | _ -> None
        in
        match close_char with
        | Some close_char -> (
            match String.index trimmed close_char with
            | Some close_index when close_index > 1 ->
                Some (String.sub trimmed ~pos:1 ~len:(close_index - 1))
            | _ -> None)
        | None -> None
    in
    match content with
    | None -> None
    | Some body ->
        let tokens =
          String.split body ~on:' '
          |> List.filter ~f:(fun token -> not (String.is_empty token))
        in
        (match tokens with
        | date_token :: rest -> (
            match Or_error.try_with (fun () -> Date.of_string date_token) with
            | Error _ -> None
            | Ok date ->
                let ofday =
                  match
                    List.find rest ~f:(fun token ->
                        Re2.matches timestamp_time_pattern token)
                  with
                  | None -> Some Date_context.midnight
                  | Some token ->
                      let base =
                        match String.lsplit2 token ~on:'-' with
                        | Some (start_time, _) -> start_time
                        | None -> token
                      in
                      let normalized =
                        if String.count base ~f:(Char.equal ':') = 1 then
                          base ^ ":00"
                        else base
                      in
                      Option.try_with (fun () -> Time_float.Ofday.of_string normalized)
                in
                Option.map ofday ~f:(fun parsed_ofday ->
                    Time_float_unix.of_date_ofday ~zone date parsed_ofday))
        | [] -> None)

  let heading_is_done doc (heading : Org.heading) =
    match heading.todo_keyword with
    | None -> false
    | Some state ->
        List.mem doc.todo_config.done_states state ~equal:String.equal

  let heading_is_open doc (heading : Org.heading) =
    match heading.todo_keyword with
    | None -> false
    | Some state ->
        List.mem doc.todo_config.open_states state ~equal:String.equal

  let heading_values_from_id_set doc id_set =
    doc.index.headings
    |> List.filter ~f:(fun heading -> Hash_set.mem id_set heading.id)
    |> List.map ~f:(fun heading -> Value.Heading heading)

  let rec eval_expr runtime current = function
    | Ast.Literal (Ast.String text) -> Value.String text
    | Ast.Literal (Ast.Int value) -> Value.Int value
    | Ast.Literal (Ast.Float value) -> Value.Float value
    | Ast.Literal (Ast.Bool value) -> Value.Bool value
    | Ast.Path fields ->
        List.fold fields ~init:current ~f:(fun value field ->
            field_value runtime value field)
    | Ast.Call { name; args } ->
        let resolved_args = List.map args ~f:(eval_expr runtime current) in
        (match (name, resolved_args) with
        | "contains", [ left; right ] ->
            let left_text = as_string left in
            let right_text = as_string right in
            Value.Bool (String.is_substring left_text ~substring:right_text)
        | "startswith", [ left; right ] ->
            let left_text = as_string left in
            let right_text = as_string right in
            Value.Bool (String.is_prefix left_text ~prefix:right_text)
        | "endswith", [ left; right ] ->
            let left_text = as_string left in
            let right_text = as_string right in
            Value.Bool (String.is_suffix left_text ~suffix:right_text)
        | "contains", _ | "startswith", _ | "endswith", _ ->
            failf "wrong arity for function %s (expected 2 args)" name
        | _ ->
            failf
              "unknown function call %S in expression (supported: \
               contains|startswith|endswith)"
              name)
    | Ast.Parenthesized predicate ->
        Value.Bool (eval_predicate runtime current predicate)

  and eval_predicate runtime current = function
    | Ast.Predicate_value expr ->
        eval_expr runtime current expr |> as_bool
    | Ast.Predicate_compare { left; op; right } ->
        let left_value = eval_expr runtime current left in
        let right_value = eval_expr runtime current right in
        eval_comparison op left_value right_value |> as_bool
    | Ast.Predicate_and (left, right) ->
        eval_predicate runtime current left
        && eval_predicate runtime current right
    | Ast.Predicate_or (left, right) ->
        eval_predicate runtime current left
        || eval_predicate runtime current right

  let apply_selector runtime ({ Ast.name; args } : Ast.selector) current =
    let doc = runtime.Runtime.doc in
    match name with
    | "tree" ->
        let headings = doc.index.headings in
        let selected =
          match args with
          | [] -> headings
          | _ -> failf ".%s does not take arguments" name
        in
        Value.List
          (List.map selected ~f:(fun heading -> Value.Tree_heading heading))
    | "headings" ->
        let headings = doc.index.headings in
        let selected =
          match args with
          | [] -> headings
          | [ arg ] ->
              let max_level = selector_arg_int_exn name 1 arg in
              if max_level < 1 then
                failf ".%s expects a positive level when an argument is provided"
                  name;
              List.filter headings ~f:(fun heading -> heading.level <= max_level)
          | _ -> failf ".%s expects at most one argument" name
        in
        Value.List (List.map selected ~f:(fun heading -> Value.Heading heading))
    | "sections" ->
        Value.List (List.map doc.index.headings ~f:section_of_heading)
    | "section" ->
        let raw_title =
          match args with
          | first :: _ -> selector_arg_string_exn name 1 first
          | [] -> failf ".section expects 1 or 2 arguments"
        in
        let requested_level, title_with_suffix =
          parse_section_title_query_prefix raw_title
        in
        let title_without_span, embedded_span =
          parse_section_title_query_span title_with_suffix
        in
        let level_matches_requested (heading : Org.heading) =
          match requested_level with
          | None -> true
          | Some level -> Int.equal heading.level level
        in
        let has_title_match title =
          List.exists doc.index.headings ~f:(fun (heading : Org.heading) ->
              String.equal heading.title title
              && level_matches_requested heading)
        in
        let title, embedded_span =
          match embedded_span with
          | Some span when has_title_match title_without_span ->
              (title_without_span, Some span)
          | Some _ -> (title_with_suffix, None)
          | None -> (title_without_span, None)
        in
        let matches =
          List.filter doc.index.headings ~f:(fun (heading : Org.heading) ->
              String.equal heading.title title
              && level_matches_requested heading)
        in
        let requested_span =
          match args with
          | [ _ ] -> embedded_span
          | [ _; span_arg ] -> Some (selector_arg_span_exn name 2 span_arg)
          | _ -> failf ".section expects 1 or 2 arguments"
        in
        (match requested_span with
        | None -> (
            match matches with
            | [] -> failf "section not found for title %S" raw_title
            | [ heading ] -> section_of_heading heading
            | candidates ->
                let formatted =
                  List.map candidates ~f:canonical_section_candidate
                in
                failf "%s"
                  (Diagnostic.ambiguous_section_error ~title
                     ~candidates:formatted))
        | Some (requested_start, requested_end) ->
            let disambiguated =
              List.find matches ~f:(fun heading ->
                  Int.equal heading.section_source.span.start_line requested_start
                  && Int.equal heading.section_source.span.end_line requested_end)
            in
            (match disambiguated with
            | Some heading -> section_of_heading heading
            | None ->
                failf "section not found for title %S with span %d:%d" raw_title
                  requested_start requested_end)
        )
    | "section_contains" ->
        let term =
          match args with
          | [ arg ] -> selector_arg_string_exn name 1 arg
          | _ -> failf ".section_contains expects exactly 1 argument"
        in
        doc.index.headings
        |> List.filter ~f:(fun heading ->
               String.is_substring heading.title ~substring:term)
        |> List.map ~f:section_of_heading |> fun values -> Value.List values
    | "search" ->
        let raw =
          match args with
          | [ arg ] -> selector_arg_string_exn name 1 arg
          | _ -> failf ".search expects exactly 1 argument"
        in
        let matcher =
          match parse_regex_literal raw with
          | Some (pattern, flags) ->
              let regex = compile_regex pattern flags in
              fun text -> Re2.matches regex text
          | None ->
              let regex = compile_literal_search raw in
              fun text -> Re2.matches regex text
        in
        let matched =
          List.filter doc.index.headings ~f:(fun heading ->
              matcher (Runtime.section_text runtime heading))
        in
        Value.List (List.map matched ~f:section_of_heading)
    | "todos" ->
        let selected = List.filter doc.index.headings ~f:(heading_is_open doc) in
        Value.List (List.map selected ~f:(fun heading -> Value.Heading heading))
    | "done" ->
        let selected = List.filter doc.index.headings ~f:(heading_is_done doc) in
        Value.List (List.map selected ~f:(fun heading -> Value.Heading heading))
    | "properties" ->
        Value.List
          (List.map doc.index.properties ~f:(fun property -> Value.Property property))
    | "property" ->
        let key =
          match args with
          | [ arg ] -> selector_arg_string_exn name 1 arg
          | _ -> failf ".property expects exactly 1 argument"
        in
        let selected =
          List.filter doc.index.properties ~f:(fun property ->
              String.Caseless.equal property.key key)
        in
        Value.List (List.map selected ~f:(fun property -> Value.Property property))
    | "scheduled" | "deadline" | "closed" ->
        let planning_kind =
          match name with
          | "scheduled" -> Org.Scheduled
          | "deadline" -> Org.Deadline
          | "closed" -> Org.Closed
          | _ -> assert false
        in
        let range =
          match args with
          | [] -> None
          | [ arg ] ->
              let range_text = selector_arg_string_exn name 1 arg in
              Some
                (Date_context.parse_range ~selector:name
                   ~allow_overdue:(not (String.equal name "closed"))
                   range_text)
          | _ -> failf ".%s expects at most 1 argument" name
        in
        let date_context = runtime.Runtime.date_context in
        let entries =
          List.filter doc.index.planning ~f:(fun entry ->
              Poly.equal entry.kind planning_kind)
        in
        let matched_entries =
          List.filter entries ~f:(fun entry ->
              match range with
              | None -> true
              | Some _ -> (
                  match parse_org_timestamp ~zone:date_context.zone entry.raw_value with
                  | None -> false
                  | Some timestamp -> (
                      match range with
                      | None -> assert false
                      | Some Date_context.Overdue ->
                          Time_float.compare timestamp date_context.day_start < 0
                          &&
                          if
                            String.equal name "scheduled"
                            || String.equal name "deadline"
                          then
                            let heading = heading_by_id runtime entry.heading_id in
                            not (heading_is_done doc heading)
                          else true
                      | Some range ->
                          let start_time, stop_time =
                            Date_context.range_bounds date_context range
                          in
                          Time_float.compare timestamp start_time >= 0
                          && Time_float.compare timestamp stop_time < 0)))
        in
        let heading_ids = Int.Hash_set.create () in
        List.iter matched_entries ~f:(fun entry ->
            Hash_set.add heading_ids entry.heading_id);
        Value.List (heading_values_from_id_set doc heading_ids)
    | "tags" ->
        let seen = String.Hash_set.create () in
        let tags =
          List.concat_map doc.index.headings ~f:(fun heading -> heading.tags)
          |> List.filter ~f:(fun tag ->
                 if Hash_set.mem seen tag then false
                 else (
                   Hash_set.add seen tag;
                   true))
        in
        Value.List (List.map tags ~f:(fun tag -> Value.String tag))
    | "code" ->
        let language_filter =
          match args with
          | [] -> None
          | [ arg ] -> Some (selector_arg_string_exn name 1 arg)
          | _ -> failf ".code expects at most 1 argument"
        in
        let selected =
          List.filter doc.index.blocks ~f:(fun block ->
              Poly.equal block.kind Org.Src
              &&
              match language_filter with
              | None -> true
              | Some expected -> (
                  match block.language with
                  | Some actual -> String.Caseless.equal actual expected
                  | None -> false))
        in
        Value.List (List.map selected ~f:(fun block -> Value.Block block))
    | "links" ->
        Value.List (List.map doc.index.links ~f:(fun link -> Value.Link link))
    | "tables" ->
        Value.List (List.map doc.index.tables ~f:(fun table -> Value.Table table))
    | "text" -> (
        let text_of_value value =
          match value with
          | Value.Document value ->
              Some (String.concat ~sep:"\n" (Array.to_list value.Org.lines))
          | Value.Heading heading -> Some (Runtime.section_text runtime heading)
          | Value.Tree_heading heading ->
              Some (Runtime.section_text runtime heading)
          | Value.Section section -> Some (Runtime.section_text runtime section.heading)
          | Value.Property property ->
              Some
                (Runtime.text_for_span runtime.Runtime.doc property.source.span)
          | Value.Planning planning ->
              Some
                (Runtime.text_for_span runtime.Runtime.doc planning.source.span)
          | Value.Block block ->
              Some (Runtime.text_for_span runtime.Runtime.doc block.source.span)
          | Value.Link link ->
              Some (Runtime.text_for_span runtime.Runtime.doc link.source.span)
          | Value.Table table ->
              Some (Runtime.text_for_span runtime.Runtime.doc table.source.span)
          | _ -> None
        in
        match current with
        | Value.Document _
        | Value.Heading _
        | Value.Tree_heading _
        | Value.Section _
        | Value.Property _
        | Value.Planning _
        | Value.Block _
        | Value.Link _
        | Value.Table _ -> (
            match text_of_value current with
            | Some text -> Value.String text
            | None ->
                failf "wrong type: .text is not supported for %s" (type_name current))
        | Value.List values ->
            Value.List
              (List.map values ~f:(fun value ->
                   match text_of_value value with
                   | Some text -> Value.String text
                   | None ->
                       failf
                         "wrong type: .text on list requires element types that \
                          have source text"))
        | value ->
            failf "wrong type: .text is not supported for %s" (type_name value))
    | "length" -> (
        match current with
        | Value.List values -> Value.Int (List.length values)
        | Value.String text -> Value.Int (String.length text)
        | value ->
            failf "wrong type: .length is not supported for %s" (type_name value))
    | _ -> failf "unknown selector .%s" name

  let apply_index values raw_index =
    let length = List.length values in
    let index = if raw_index < 0 then length + raw_index else raw_index in
    if index < 0 || index >= length then
      failf "index %d out of bounds for length %d" raw_index length;
    List.nth_exn values index

  let normalize_slice_bound length value default =
    let normalized =
      match value with
      | None -> default
      | Some index -> if index < 0 then length + index else index
    in
    Int.min length (Int.max 0 normalized)

  let apply_slice values start_opt end_opt =
    let length = List.length values in
    let start_index = normalize_slice_bound length start_opt 0 in
    let end_index = normalize_slice_bound length end_opt length in
    if end_index <= start_index then Value.List []
    else Value.List (List.slice values start_index end_index)

  let apply_field runtime value field =
    match value with
    | Value.List items ->
        Value.List
          (List.mapi items ~f:(fun index item ->
               try field_value runtime item field
               with
               | Failure message ->
                   failf "field access failed at index %d: %s" index message))
    | _ -> field_value runtime value field

  let apply_postfix runtime value = function
    | Ast.Index index ->
        let values = as_list value in
        apply_index values index
    | Ast.Slice (start_opt, end_opt) ->
        let values = as_list value in
        apply_slice values start_opt end_opt
    | Ast.Field field -> apply_field runtime value field

  let apply_function runtime fn current =
    match fn with
    | Ast.Filter predicate ->
        let values = as_list current in
        let kept =
          List.filter values ~f:(fun item -> eval_predicate runtime item predicate)
        in
        Value.List kept
    | Ast.Map expr ->
        let values = as_list current in
        Value.List (List.map values ~f:(fun item -> eval_expr runtime item expr))

  let apply_stage runtime ({ Ast.atom_stage; postfixes } : Ast.stage) current =
    let base =
      match atom_stage with
      | Ast.Selector selector -> apply_selector runtime selector current
      | Ast.Function fn -> apply_function runtime fn current
    in
    List.fold postfixes ~init:base ~f:(apply_postfix runtime)

  let eval ?now ?tz ~doc stages =
    let runtime = Runtime.create ?now ?tz doc in
    List.fold stages ~init:(Value.Document doc) ~f:(fun current stage ->
        apply_stage runtime stage current)

  let run ?now ?tz ~doc query =
    match Query.parse query with
    | Error error -> Error (Query_parse_error error)
    | Ok stages -> (
        try Ok (eval ?now ?tz ~doc stages) with
        | Failure message -> Error (Eval_error message))

  let render_scalar = function
    | Value.String text -> text
    | Value.Int value -> Int.to_string value
    | Value.Float value -> Float.to_string value
    | Value.Bool value -> Bool.to_string value
    | Value.Null -> "null"
    | Value.Heading heading ->
        sprintf "%s %s (lines %d:%d)"
          (String.make heading.level '*')
          heading.title heading.section_source.span.start_line
          heading.section_source.span.end_line
    | Value.Tree_heading heading ->
        sprintf "%s %s (lines %d:%d)"
          (String.make heading.level '*')
          heading.title heading.section_source.span.start_line
          heading.section_source.span.end_line
    | Value.Section section ->
        sprintf "%s %s (lines %d:%d)"
          (String.make section.heading.level '*')
          section.heading.title
          section.source.span.start_line section.source.span.end_line
    | Value.Property property ->
        sprintf "%s=%s (lines %d:%d)" property.key property.value
          property.source.span.start_line property.source.span.end_line
    | Value.Planning planning ->
        sprintf "%s %s (lines %d:%d)"
          (String.uppercase (planning_kind_to_string planning.kind))
          planning.raw_value planning.source.span.start_line
          planning.source.span.end_line
    | Value.Block block ->
        let kind = block_kind_to_string block.kind in
        let language =
          match block.language with
          | Some language -> ":" ^ language
          | None -> ""
        in
        sprintf "%s%s (lines %d:%d)" kind language block.source.span.start_line
          block.source.span.end_line
    | Value.Link link ->
        sprintf "%s (lines %d:%d)" link.target link.source.span.start_line
          link.source.span.end_line
    | Value.Table table ->
        sprintf "table rows=%d (lines %d:%d)" (List.length table.rows)
          table.source.span.start_line table.source.span.end_line
    | Value.Document doc -> sprintf "Document(%s)" doc.path
    | Value.List _ -> failf "internal error: expected scalar value for rendering"

  let render_text value =
    match value with
    | Value.List values -> List.map values ~f:render_scalar |> String.concat ~sep:"\n"
    | _ -> render_scalar value

  let run_text ?now ?tz ~doc query =
    match run ?now ?tz ~doc query with
    | Ok value -> Ok (render_text value)
    | Error (Query_parse_error { message; position }) ->
        Error (sprintf "query parse error at %d: %s" position message)
    | Error (Eval_error message) -> Error message
end

module Directory = struct
  type counters = {
    candidate_org : int;
    parsed_ok : int;
    parse_failed : int;
    skipped_hidden : int;
    skipped_symlink : int;
  }

  type parsed_doc = {
    relative_path : string;
    doc : Org.t;
  }

  type parse_failure = {
    relative_path : string;
    reason : Diagnostic.parse_reason;
  }

  type t = {
    root_path : string;
    counters : counters;
    parsed_docs : parsed_doc list;
    parse_failures : parse_failure list;
  }

  exception Scan_io_error of string

  let is_hidden_name name =
    String.length name > 0 && Char.equal name.[0] '.'

  let has_org_extension path =
    let lower = String.lowercase path in
    String.is_suffix lower ~suffix:".org"
    || String.is_suffix lower ~suffix:".org_archive"

  let is_org_file name = has_org_extension name
  let normalize_relative_path = Ordering.normalize_relative_path

  let compare_relative_path left right = String.compare left right

  let sort_parsed_docs (docs : parsed_doc list) : parsed_doc list =
    List.sort docs ~compare:(fun (left : parsed_doc) (right : parsed_doc) ->
        compare_relative_path left.relative_path right.relative_path)

  let sort_parse_failures (failures : parse_failure list) : parse_failure list =
    List.sort failures ~compare:(fun (left : parse_failure) (right : parse_failure) ->
        compare_relative_path left.relative_path right.relative_path)

  let io_errorf fmt =
    Printf.ksprintf (fun message -> raise (Scan_io_error message)) fmt

  let scan root_path =
    try
      let candidate_org = ref 0 in
      let parsed_ok = ref 0 in
      let parse_failed = ref 0 in
      let skipped_hidden = ref 0 in
      let skipped_symlink = ref 0 in
      let parsed_docs_rev = ref [] in
      let parse_failures_rev = ref [] in

      let incr_candidate_org () = candidate_org := !candidate_org + 1 in
      let incr_parsed_ok () = parsed_ok := !parsed_ok + 1 in
      let incr_parse_failed () = parse_failed := !parse_failed + 1 in
      let incr_skipped_hidden () = skipped_hidden := !skipped_hidden + 1 in
      let incr_skipped_symlink () = skipped_symlink := !skipped_symlink + 1 in

      let read_all path =
        try In_channel.read_all path
        with
        | Sys_error message -> io_errorf "failed to read %s: %s" path message
      in

      let rec walk ~absolute_dir ~relative_dir =
        let names =
          try Stdlib.Sys.readdir absolute_dir
          with
          | Sys_error message ->
              io_errorf "failed to read directory %s: %s" absolute_dir message
        in
        Array.sort ~compare:String.compare names;
        Array.iter names ~f:(fun name ->
            if String.equal name "." || String.equal name ".." then ()
            else if is_hidden_name name then incr_skipped_hidden ()
            else
              let absolute_path = Filename.concat absolute_dir name in
              let relative_path =
                if String.is_empty relative_dir then name
                else relative_dir ^ "/" ^ name
              in
              let file_kind =
                try (Caml_unix.lstat absolute_path).st_kind
                with
                | Caml_unix.Unix_error (error, _, _) ->
                    io_errorf "failed to stat %s: %s" absolute_path
                      (Caml_unix.error_message error)
              in
              match file_kind with
              | Caml_unix.S_LNK -> incr_skipped_symlink ()
              | Caml_unix.S_DIR ->
                  walk ~absolute_dir:absolute_path ~relative_dir:relative_path
              | Caml_unix.S_REG when is_org_file name ->
                  incr_candidate_org ();
                  let normalized_relative_path =
                    normalize_relative_path relative_path
                  in
                  let content = read_all absolute_path in
                  (match Org.parse_string ~path:normalized_relative_path content with
                  | Ok doc ->
                      incr_parsed_ok ();
                      parsed_docs_rev :=
                        { relative_path = normalized_relative_path; doc }
                        :: !parsed_docs_rev
                  | Error parse_error ->
                      incr_parse_failed ();
                      parse_failures_rev :=
                        {
                          relative_path = normalized_relative_path;
                          reason = parse_error.reason;
                        }
                        :: !parse_failures_rev)
              | Caml_unix.S_REG -> ()
              | Caml_unix.S_BLK -> ()
              | Caml_unix.S_CHR -> ()
              | Caml_unix.S_FIFO -> ()
              | Caml_unix.S_SOCK -> ())
      in

      walk ~absolute_dir:root_path ~relative_dir:"";

      let parsed_docs = List.rev !parsed_docs_rev |> sort_parsed_docs in
      let parse_failures =
        List.rev !parse_failures_rev |> sort_parse_failures
      in
      let counters =
        {
          candidate_org = !candidate_org;
          parsed_ok = !parsed_ok;
          parse_failed = !parse_failed;
          skipped_hidden = !skipped_hidden;
          skipped_symlink = !skipped_symlink;
        }
      in
      Ok
        {
          root_path;
          counters;
          parsed_docs;
          parse_failures;
        }
    with
    | Scan_io_error message -> Error message
end

module Cli = struct
  type request = {
    input_path : string;
    query : string option;
    strict : bool;
    now : string option;
    tz : string option;
  }

  type outcome = {
    stdout : string option;
    stderr_lines : string list;
    exit_code : Exit_code.t;
  }

  let run () = "oq: ready"

  let make_outcome ?stdout ?(stderr_lines = []) exit_code =
    { stdout; stderr_lines; exit_code }

  let ensure_error_prefix message =
    if String.is_prefix message ~prefix:"Error:" then message
    else Diagnostic.error message

  let has_org_extension path =
    String.is_suffix (String.lowercase path) ~suffix:".org"

  let render_file_summary (doc : Org.t) =
    String.concat ~sep:"\n"
      [
        sprintf "path=%s" doc.path;
        sprintf "headings=%d" (List.length doc.index.headings);
        sprintf "todos=%d" (List.length doc.index.todos);
        sprintf "lines=%d" doc.line_count;
      ]

  let render_directory_summary (counters : Directory.counters) =
    String.concat ~sep:"\n"
      [
        sprintf "candidate_org=%d" counters.candidate_org;
        sprintf "parsed_ok=%d" counters.parsed_ok;
        sprintf "parse_failed=%d" counters.parse_failed;
        sprintf "skipped_hidden=%d" counters.skipped_hidden;
        sprintf "skipped_symlink=%d" counters.skipped_symlink;
      ]

  let indent_block text =
    String.split_lines text |> List.map ~f:(fun line -> "  " ^ line)
    |> String.concat ~sep:"\n"

  let render_group ~relative_path ~text =
    sprintf "%s:\n%s" relative_path (indent_block text)

  let render_directory_output ~counters ~grouped_results =
    let summary = render_directory_summary counters in
    match grouped_results with
    | [] -> summary
    | _ -> summary ^ "\n\n" ^ String.concat ~sep:"\n\n" grouped_results

  let run_query ?now ?tz ~(doc : Org.t) query =
    Eval.run_text ?now ?tz ~doc query

  let parse_single_file path =
    try
      let content = In_channel.read_all path in
      Ok (Org.parse_string ~path:(Ordering.normalize_relative_path path) content)
    with
    | Sys_error message -> Error message

  let run_file_mode request =
    match parse_single_file request.input_path with
    | Error message ->
        make_outcome Exit_code.Io_or_permission_error
          ~stderr_lines:[ Diagnostic.error message ]
    | Ok (Error parse_error) ->
        let message =
          sprintf "failed to parse %s: %s" request.input_path
            (Diagnostic.parse_reason_to_string parse_error.reason)
        in
        make_outcome Exit_code.Parse_coverage_error
          ~stderr_lines:[ Diagnostic.error message ]
    | Ok (Ok doc) -> (
        match request.query with
        | None ->
            make_outcome Exit_code.Success ~stdout:(render_file_summary doc)
        | Some query -> (
            match run_query ?now:request.now ?tz:request.tz ~doc query with
            | Ok text -> make_outcome Exit_code.Success ~stdout:text
            | Error message ->
                make_outcome Exit_code.Query_or_usage_error
                  ~stderr_lines:[ ensure_error_prefix message ]))

  let run_directory_mode request =
    match Directory.scan request.input_path with
    | Error message ->
        make_outcome Exit_code.Io_or_permission_error
          ~stderr_lines:[ Diagnostic.error message ]
    | Ok directory ->
        let warnings =
          List.map directory.parse_failures ~f:(fun failure ->
              Diagnostic.parse_warning ~path:failure.relative_path
                ~reason:failure.reason)
        in
        let summary = render_directory_summary directory.counters in
        if
          directory.counters.candidate_org
          <> directory.counters.parsed_ok + directory.counters.parse_failed
        then
          make_outcome Exit_code.Parse_coverage_error
            ~stdout:summary
            ~stderr_lines:
              (warnings
              @ [ Diagnostic.error "internal invariant failure: candidate_org mismatch" ])
        else if Int.equal directory.counters.candidate_org 0 then
          make_outcome Exit_code.Query_or_usage_error
            ~stdout:summary
            ~stderr_lines:
              (warnings
              @ [
                  Diagnostic.error
                    "no candidate .org files found after hidden/symlink filters";
                ])
        else if request.strict && directory.counters.parse_failed > 0 then
          make_outcome Exit_code.Parse_coverage_error
            ~stdout:summary
            ~stderr_lines:
              (warnings
              @ [ Diagnostic.error "strict mode failed: at least one .org file did not parse" ])
        else if Int.equal directory.counters.parsed_ok 0 then
          make_outcome Exit_code.Parse_coverage_error
            ~stdout:summary
            ~stderr_lines:
              (warnings
              @ [
                  Diagnostic.error
                    "parse coverage failure: zero candidate .org files parsed successfully";
                ])
        else
          match request.query with
          | None -> make_outcome Exit_code.Success ~stdout:summary ~stderr_lines:warnings
          | Some query ->
              let rec eval_docs grouped = function
                | [] -> Ok (List.rev grouped)
                | { Directory.relative_path; doc } :: rest -> (
                    match run_query ?now:request.now ?tz:request.tz ~doc query with
                    | Error message ->
                        Error
                          (sprintf "query failed in %s: %s" relative_path
                             (String.strip message))
                    | Ok text ->
                        if String.is_empty (String.strip text) then
                          eval_docs grouped rest
                        else
                          let rendered_group = render_group ~relative_path ~text in
                          eval_docs (rendered_group :: grouped) rest)
              in
              (match eval_docs [] directory.parsed_docs with
              | Error message ->
                  make_outcome Exit_code.Query_or_usage_error
                    ~stdout:summary
                    ~stderr_lines:(warnings @ [ ensure_error_prefix message ])
              | Ok grouped_results ->
                  let stdout =
                    render_directory_output ~counters:directory.counters
                      ~grouped_results
                  in
                  make_outcome Exit_code.Success ~stdout ~stderr_lines:warnings)

  let execute request =
    let stat_result =
      try Ok (Caml_unix.lstat request.input_path)
      with
      | Caml_unix.Unix_error (error, _, _) ->
          Error
            (sprintf "unable to access %s: %s" request.input_path
               (Caml_unix.error_message error))
    in
    match stat_result with
    | Error message ->
        make_outcome Exit_code.Io_or_permission_error
          ~stderr_lines:[ Diagnostic.error message ]
    | Ok stat -> (
        match stat.st_kind with
        | Caml_unix.S_DIR -> run_directory_mode request
        | Caml_unix.S_REG -> run_file_mode request
        | Caml_unix.S_LNK ->
            let target_stat_result =
              try Ok (Caml_unix.stat request.input_path)
              with
              | Caml_unix.Unix_error (error, _, _) ->
                  Error
                    (sprintf "unable to resolve symlink %s: %s"
                       request.input_path
                       (Caml_unix.error_message error))
            in
            (match target_stat_result with
            | Error message ->
                make_outcome Exit_code.Io_or_permission_error
                  ~stderr_lines:[ Diagnostic.error message ]
            | Ok target_stat -> (
                match target_stat.st_kind with
                | Caml_unix.S_REG -> run_file_mode request
                | _ ->
                    make_outcome Exit_code.Query_or_usage_error
                      ~stderr_lines:
                        [
                          Diagnostic.error
                            (sprintf
                               "input path %S is a symlink and is skipped by \
                                traversal rules"
                               request.input_path);
                        ]))
        | _ ->
            make_outcome Exit_code.Query_or_usage_error
              ~stderr_lines:
                [
                  Diagnostic.error
                    (sprintf "unsupported input path kind for %S" request.input_path);
                ])
end
