open Core

let resolve_test_path relative_path =
  let candidates =
    [
      Filename.concat (Filename.dirname __FILE__) relative_path;
      Filename.concat (Stdlib.Sys.getcwd ()) (Filename.concat "test" relative_path);
      Filename.concat (Stdlib.Sys.getcwd ()) relative_path;
    ]
  in
  match List.find candidates ~f:Stdlib.Sys.file_exists with
  | Some path -> path
  | None ->
      failwithf "unable to locate test file %s (checked %s)" relative_path
        (String.concat ~sep:", " candidates) ()

let assert_contains text needle =
  if String.is_substring text ~substring:needle then ()
  else failwithf "expected output to contain %S, got:\n%s" needle text ()

let assert_exit outcome expected =
  assert
    (Int.equal
       (Oq.Exit_code.to_int outcome.Oq.Cli.exit_code)
       (Oq.Exit_code.to_int expected))

let assert_stable_outcome ~scenario_id ~step_id baseline rerun =
  let baseline_exit = Oq.Exit_code.to_int baseline.Oq.Cli.exit_code in
  let rerun_exit = Oq.Exit_code.to_int rerun.Oq.Cli.exit_code in
  if baseline_exit <> rerun_exit then
    failwithf
      "scenario %s step %s changed exit code between runs (%d vs %d)"
      scenario_id step_id baseline_exit rerun_exit ();
  if not (Option.equal String.equal baseline.stdout rerun.stdout) then
    failwithf "scenario %s step %s changed stdout between runs" scenario_id
      step_id ();
  if not (List.equal String.equal baseline.stderr_lines rerun.stderr_lines) then
    failwithf "scenario %s step %s changed stderr between runs" scenario_id
      step_id ()

let render_stderr lines = String.concat ~sep:"\n" lines

type step = {
  id : string;
  query : string;
  strict : bool;
  now : string option;
  tz : string option;
  expected_exit : Oq.Exit_code.t;
  expected_stdout_substrings : string list;
  expected_stderr_substrings : string list;
}

type scenario = {
  id : string;
  input_path : string;
  steps : step list;
}

let scenarios =
  [
    {
      id = "find-section-narrow-extract-text";
      input_path = resolve_test_path "fixtures/corpus/duplicate_headings.org";
      steps =
        [
          {
            id = "discover-headings";
            query = ".headings | map(.title)";
            strict = false;
            now = None;
            tz = None;
            expected_exit = Oq.Exit_code.Success;
            expected_stdout_substrings = [ "Projects"; "Inbox"; "Follow-up" ];
            expected_stderr_substrings = [];
          };
          {
            id = "trigger-ambiguity";
            query = ".section('Inbox') | .text";
            strict = false;
            now = None;
            tz = None;
            expected_exit = Oq.Exit_code.Query_or_usage_error;
            expected_stdout_substrings = [];
            expected_stderr_substrings =
              [
                "Error: ambiguous section title \"Inbox\" (2 matches)";
                "Hint: use .section(\"Inbox\", 6:8)";
              ];
          };
          {
            id = "extract-disambiguated-section";
            query = ".section('Inbox', 6:8) | .text";
            strict = false;
            now = None;
            tz = None;
            expected_exit = Oq.Exit_code.Success;
            expected_stdout_substrings =
              [ "** Inbox"; "First inbox content to extract in workflow tests." ];
            expected_stderr_substrings = [];
          };
        ];
    };
    {
      id = "directory-search-filter-select-heading";
      input_path = resolve_test_path "fixtures/corpus";
      steps =
        [
          {
            id = "search-release";
            query = ".search('release')";
            strict = false;
            now = None;
            tz = None;
            expected_exit = Oq.Exit_code.Success;
            expected_stdout_substrings =
              [
                "candidate_org=4";
                "parsed_ok=4";
                "todo_workflows.org:";
                "Prepare release (lines 4:9)";
              ];
            expected_stderr_substrings = [];
          };
          {
            id = "filter-open-todos";
            query = ".todos | filter(.state == 'TODO') | map(.title)";
            strict = false;
            now = None;
            tz = None;
            expected_exit = Oq.Exit_code.Success;
            expected_stdout_substrings = [ "todo_workflows.org:"; "Prepare release" ];
            expected_stderr_substrings = [];
          };
          {
            id = "select-release-section";
            query = ".headings | filter(.title == 'Prepare release') | .text";
            strict = false;
            now = None;
            tz = None;
            expected_exit = Oq.Exit_code.Success;
            expected_stdout_substrings =
              [ "todo_workflows.org:"; "* TODO Prepare release :work:release:" ];
            expected_stderr_substrings = [];
          };
        ];
    };
    {
      id = "deterministic-time-window";
      input_path = resolve_test_path "fixtures/corpus/todo_workflows.org";
      steps =
        [
          {
            id = "scheduled-next-7d";
            query = ".scheduled('next_7d') | map(.title)";
            strict = false;
            now = Some "2026-02-17T08:00:00-08:00";
            tz = Some "America/Los_Angeles";
            expected_exit = Oq.Exit_code.Success;
            expected_stdout_substrings = [ "Prepare release"; "Confirm changelog" ];
            expected_stderr_substrings = [];
          };
          {
            id = "deadline-overdue";
            query = ".deadline('overdue') | .length";
            strict = false;
            now = Some "2026-02-17T08:00:00-08:00";
            tz = Some "America/Los_Angeles";
            expected_exit = Oq.Exit_code.Success;
            expected_stdout_substrings = [ "0" ];
            expected_stderr_substrings = [];
          };
        ];
    };
  ]

let () =
  List.iter scenarios ~f:(fun scenario ->
      List.iter scenario.steps ~f:(fun step ->
          let request : Oq.Cli.request =
            {
              input_path = scenario.input_path;
              query = Some step.query;
              strict = step.strict;
              now = step.now;
              tz = step.tz;
            }
          in
          let first = Oq.Cli.execute request in
          let second = Oq.Cli.execute request in
          assert_stable_outcome ~scenario_id:scenario.id ~step_id:step.id first
            second;
          assert_exit first step.expected_exit;
          List.iter step.expected_stdout_substrings ~f:(fun needle ->
              match first.stdout with
              | Some stdout -> assert_contains stdout needle
              | None ->
                  failwithf
                    "scenario %s step %s expected stdout to contain %S but \
                     stdout was empty"
                    scenario.id step.id needle ());
          let stderr_text = render_stderr first.stderr_lines in
          List.iter step.expected_stderr_substrings ~f:(fun needle ->
              assert_contains stderr_text needle)))
