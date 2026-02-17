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

let duplicate_headings = read_test_file "fixtures/corpus/duplicate_headings.org"
let todo_workflows = read_test_file "fixtures/corpus/todo_workflows.org"
let properties_drawers = read_test_file "fixtures/corpus/properties_drawers.org"
let blocks_links_tables = read_test_file "fixtures/corpus/blocks_links_tables.org"

type scenario = {
  id : string;
  assertions : (unit -> unit) list;
}

let assert_contains text needle =
  assert (String.is_substring text ~substring:needle)

let scenarios =
  [
    {
      id = "find-section-narrow-extract-text";
      assertions =
        [
          (fun () -> assert_contains duplicate_headings "** Inbox");
          (fun () ->
            assert_contains duplicate_headings
              "First inbox content to extract in workflow tests.");
        ];
    };
    {
      id = "directory-search-filter-select-heading";
      assertions =
        [
          (fun () -> assert_contains todo_workflows "Prepare release");
          (fun () -> assert_contains blocks_links_tables "Reference link:");
          (fun () -> assert_contains properties_drawers "Project Alpha");
        ];
    };
    {
      id = "recover-from-ambiguity-error";
      assertions =
        [
          (fun () ->
            let candidates : Oq.Diagnostic.section_candidate list =
              [
                { title = "Inbox"; start_line = 6; end_line = 8 };
                { title = "Inbox"; start_line = 9; end_line = 13 };
              ]
            in
            let output =
              Oq.Diagnostic.ambiguous_section_error ~title:"Inbox" ~candidates
            in
            assert_contains output
              "Error: ambiguous section title \"Inbox\" (2 matches)");
          (fun () ->
            let candidates : Oq.Diagnostic.section_candidate list =
              [
                { title = "Inbox"; start_line = 6; end_line = 8 };
                { title = "Inbox"; start_line = 9; end_line = 13 };
              ]
            in
            let output =
              Oq.Diagnostic.ambiguous_section_error ~title:"Inbox" ~candidates
            in
            assert_contains output "Hint: use .section(\"Inbox\", 6:8)");
        ];
    };
  ]

let () =
  List.iter scenarios ~f:(fun scenario ->
      assert (String.length scenario.id > 0);
      List.iter scenario.assertions ~f:(fun check -> check ()))
