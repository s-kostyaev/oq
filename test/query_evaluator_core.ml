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

let parse_fixture relative_path =
  let content = read_test_file relative_path in
  match Oq.Org.parse_string ~path:relative_path content with
  | Ok doc -> doc
  | Error err ->
      failwithf "unexpected parse failure for %s: %s (%s)" relative_path
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()

let run_ok doc query =
  match Oq.Eval.run_text ~doc query with
  | Ok value -> value
  | Error message -> failwithf "expected success for %S, got error: %s" query message ()

let run_error doc query =
  match Oq.Eval.run_text ~doc query with
  | Ok value -> failwithf "expected error for %S, got value: %s" query value ()
  | Error message -> message

let render_cases ~title cases =
  let rendered =
    List.map cases ~f:(fun (query, output) ->
        String.concat ~sep:"\n"
          [ "QUERY: " ^ query; output ])
  in
  match rendered with
  | [] -> title
  | _ -> title ^ "\n" ^ String.concat ~sep:"\n---\n" rendered

let () =
  let doc = parse_fixture "fixtures/corpus/duplicate_headings.org" in
  let query_cases =
    [
      ( ".headings | .length",
        run_ok doc ".headings | .length" );
      ( ".tree('full') | .length",
        run_ok doc ".tree('full') | .length" );
      ( ".tree('full')",
        run_ok doc ".tree('full')" );
      ( ".section('*** Follow-up') | .text",
        run_ok doc ".section('*** Follow-up') | .text" );
      ( ".section('* Projects (lines 3:13)') | .text",
        run_ok doc ".section('* Projects (lines 3:13)') | .text" );
      ( ".section('Inbox', 6:8) | .text",
        run_ok doc ".section('Inbox', 6:8) | .text" );
      ( ".section('** Inbox', 6:8) | .text",
        run_ok doc ".section('** Inbox', 6:8) | .text" );
      ( ".section('Inbox (lines 6:8)') | .text",
        run_ok doc ".section('Inbox (lines 6:8)') | .text" );
      ( ".section_contains('Inbox') | .length",
        run_ok doc ".section_contains('Inbox') | .length" );
      ( ".search('second inbox') | .length",
        run_ok doc ".search('second inbox') | .length" );
      ( ".search('/second inbox/i') | .length",
        run_ok doc ".search('/second inbox/i') | .length" );
      ( ".headings | filter(startswith(.title, 'In')) | map(.title)",
        run_ok doc ".headings | filter(startswith(.title, 'In')) | map(.title)" );
      ( ".headings[1:3].title", run_ok doc ".headings[1:3].title" );
    ]
  in
  let actual = render_cases ~title:"# Core Query Output" query_cases in
  let expected = String.rstrip (read_test_file "golden/evaluator_core_queries.txt") in
  assert (String.equal actual expected)

let () =
  let doc = parse_fixture "fixtures/corpus/duplicate_headings.org" in
  let error_cases =
    [
      ( ".section('Inbox')",
        run_error doc ".section('Inbox')" );
      ( ".section('Inbox', 1:2)",
        run_error doc ".section('Inbox', 1:2)" );
      ( ".section('Inbox (lines 1:2)')",
        run_error doc ".section('Inbox (lines 1:2)')" );
      ( ".section('* Follow-up')",
        run_error doc ".section('* Follow-up')" );
      ( ".headings[9]",
        run_error doc ".headings[9]" );
      ( ".section('Projects') | filter(.title == 'Projects')",
        run_error doc ".section('Projects') | filter(.title == 'Projects')" );
    ]
  in
  let actual = render_cases ~title:"# Core Query Errors" error_cases in
  let expected = String.rstrip (read_test_file "golden/evaluator_core_errors.txt") in
  assert (String.equal actual expected)
