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

module Cli = struct
  let run () = "oq: not implemented"
end
