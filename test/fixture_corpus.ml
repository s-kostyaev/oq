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

let assert_contains text needle =
  assert (String.is_substring text ~substring:needle)

let assert_occurrences text needle expected_count =
  let count =
    List.length
      (String.substr_index_all text ~pattern:needle ~may_overlap:false)
  in
  assert (count = expected_count)

let () =
  (* Duplicate heading coverage for ambiguity scenarios. *)
  assert_contains duplicate_headings "* Projects";
  assert_occurrences duplicate_headings "** Inbox" 2;
  assert_contains duplicate_headings "*** Follow-up"

let () =
  (* TODO state + planning line coverage. *)
  assert_contains todo_workflows "#+TODO: TODO NEXT WAIT | DONE CANCELED";
  assert_contains todo_workflows "SCHEDULED:";
  assert_contains todo_workflows "DEADLINE:";
  assert_contains todo_workflows "CLOSED:"

let () =
  (* Drawer/property coverage. *)
  assert_occurrences properties_drawers ":PROPERTIES:" 2;
  assert_contains properties_drawers ":OWNER:";
  assert_contains properties_drawers ":CUSTOM_ID:"

let () =
  (* Block/link/table coverage. *)
  assert_contains blocks_links_tables "#+BEGIN_SRC ocaml";
  assert_contains blocks_links_tables "#+BEGIN_QUOTE";
  assert_contains blocks_links_tables "[[https://example.com/docs][Example Docs]]";
  assert_contains blocks_links_tables "| Name  | State |"
