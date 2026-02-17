let () =
  assert (Oq.Exit_code.to_int Oq.Exit_code.Success = 0);
  assert (Oq.Exit_code.to_int Oq.Exit_code.Query_or_usage_error = 1);
  assert (Oq.Exit_code.to_int Oq.Exit_code.Io_or_permission_error = 2);
  assert (Oq.Exit_code.to_int Oq.Exit_code.Parse_coverage_error = 3)

let () = assert (Oq.Diagnostic.error "boom" = "Error: boom")
let () = assert (Oq.Diagnostic.warning "heads up" = "Warning: heads up")

let () =
  assert
    ( Oq.Diagnostic.parse_warning
        ~path:"notes.org"
        ~reason:Oq.Diagnostic.Syntax_error
    = "Warning: failed to parse notes.org: syntax_error" )

let () =
  let open Oq.Ordering in
  let refs =
    [
      { relative_path = "docs\\z.org"; start_line = 2; end_line = 3 };
      { relative_path = "docs/a.org"; start_line = 10; end_line = 11 };
      { relative_path = "docs/a.org"; start_line = 1; end_line = 2 };
      { relative_path = "docs/a.org"; start_line = 1; end_line = 1 };
    ]
  in
  let sorted = sort_result_refs refs in
  let render item =
    Printf.sprintf "%s:%d:%d" (normalize_relative_path item.relative_path)
      item.start_line item.end_line
  in
  assert
    (List.map render sorted
    = [
        "docs/a.org:1:1";
        "docs/a.org:1:2";
        "docs/a.org:10:11";
        "docs/z.org:2:3";
      ])
