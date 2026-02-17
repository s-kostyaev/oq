open Core
open Cmdliner

let run strict now tz input_path query =
  let request : Oq.Cli.request =
    { input_path; query; strict; now; tz }
  in
  let outcome = Oq.Cli.execute request in
  Option.iter outcome.stdout ~f:Stdio.print_endline;
  List.iter outcome.stderr_lines ~f:(fun line -> Stdio.eprintf "%s\n" line);
  Oq.Exit_code.to_int outcome.exit_code

let strict_arg =
  Arg.(
    value
    & flag
    & info [ "strict" ]
        ~doc:
          "In directory mode, return parse coverage failure (exit 3) when any \
           candidate .org file fails to parse.")

let now_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "now" ] ~docv:"RFC3339"
        ~doc:
          "Reference datetime for deterministic relative date evaluation \
           (requires explicit timezone offset).")

let tz_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "tz" ] ~docv:"IANA_TZ"
        ~doc:
          "Reference timezone for deterministic relative date evaluation \
           (example: America/Los_Angeles).")

let input_path_arg =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"FILE_OR_DIR"
        ~doc:"Input .org file path or directory path.")

let query_arg =
  Arg.(
    value
    & pos 1 (some string) None
    & info [] ~docv:"QUERY"
        ~doc:
          "Optional query pipeline (see QUERY SYNTAX below; example: \
           .headings | .length).")

let man =
  [
    `S Manpage.s_description;
    `P
      "oq is a deterministic, agent-first query CLI for Org files and \
       directories.";
    `S "QUERY SYNTAX";
    `P "General form:";
    `Pre "  QUERY := STAGE (\"|\" STAGE)*";
    `P "Stage kinds:";
    `Pre
      "  SELECTOR := .tree | .headings([max_level]) | .sections\n\
      \            | .section(title[, start:end]) | .section_contains(text)\n\
      \            | .search(text_or_/pattern/flags) | .text | .length\n\
      \            | .todos | .done | .properties | .property(key)\n\
      \            | .tags | .code([lang]) | .links | .tables\n\
      \            | .scheduled([range]) | .deadline([range]) | .closed([range])\n\
      \  FUNCTION := filter(PREDICATE) | map(EXPR)\n\
      \  POSTFIX  := [index] | [start:end] | .field";
    `P "Expression and predicate rules:";
    `Pre
      "  EXPR      := .path | 'string' | \"string\" | int | float | bool\n\
      \            | contains(a,b) | startswith(a,b) | endswith(a,b)\n\
      \  PREDICATE := EXPR (==|!=|<|<=|>|>= EXPR)? (and/or ...)\n\
      \  Precedence: comparison > and > or";
    `P
      "Date ranges for .scheduled/.deadline/.closed: \
       today, tomorrow, yesterday, this_week, next_7d. \
       .scheduled/.deadline also support overdue.";
    `P "Regex search uses /pattern/flags with flags from: i, m, s.";
    `P "Examples:";
    `Pre
      "  oq notes.org \".headings | filter(.state == 'TODO') | map(.title)\"\n\
      \  oq notes.org \".section('Inbox', 6:8) | .text\"\n\
      \  oq notes.org \".search('/release/i') | map(.title)\"";
  ]

let cmd =
  let doc = "agent-first org query CLI" in
  let exits =
    [
      Cmd.Exit.info 0 ~doc:"on success.";
      Cmd.Exit.info 1 ~doc:"on query or usage errors.";
      Cmd.Exit.info 2 ~doc:"on I/O, path, or permission errors.";
      Cmd.Exit.info 3 ~doc:"on parse coverage failures.";
    ]
  in
  let info = Cmd.info "oq" ~doc ~man ~exits in
  let term =
    Term.(
      const run $ strict_arg $ now_arg $ tz_arg $ input_path_arg $ query_arg)
  in
  Cmd.v info term

let () =
  let cli_usage_error = Oq.Exit_code.to_int Oq.Exit_code.Query_or_usage_error in
  let code = Cmd.eval' ~term_err:cli_usage_error cmd in
  let normalized_code =
    if
      Int.equal code Cmd.Exit.cli_error || Int.equal code Cmd.Exit.some_error
      || Int.equal code Cmd.Exit.internal_error
    then cli_usage_error
    else code
  in
  Stdlib.exit normalized_code
