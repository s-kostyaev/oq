open Core

let read_test_file relative_path =
  let candidates =
    [
      Filename.concat (Filename.dirname __FILE__) relative_path;
      Filename.concat (Stdlib.Sys.getcwd ()) (Filename.concat "test" relative_path);
      Filename.concat (Stdlib.Sys.getcwd ()) relative_path;
    ]
  in
  match List.find candidates ~f:Stdlib.Sys.file_exists with
  | Some path -> In_channel.read_all path
  | None ->
      failwithf "unable to locate test file %s (checked %s)" relative_path
        (String.concat ~sep:", " candidates) ()

let parse_warning_golden = String.rstrip (read_test_file "golden/parse_warning.txt")
let ambiguity_error_golden = String.rstrip (read_test_file "golden/ambiguity_error.txt")

let () =
  let actual =
    Oq.Diagnostic.parse_warning ~path:"fixtures/corpus/broken.org"
      ~reason:Oq.Diagnostic.Syntax_error
  in
  assert (String.equal actual parse_warning_golden)

let () =
  let candidates : Oq.Diagnostic.section_candidate list =
    [
      { title = "Inbox"; start_line = 6; end_line = 8 };
      { title = "Inbox"; start_line = 9; end_line = 13 };
    ]
  in
  let actual = Oq.Diagnostic.ambiguous_section_error ~title:"Inbox" ~candidates in
  assert (String.equal actual ambiguity_error_golden)
