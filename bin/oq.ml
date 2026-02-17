open Core
open Cmdliner

let run strict now tz input_path query =
  let request : Oq.Cli.request =
    { input_path; query; strict; now; tz }
  in
  let outcome = Oq.Cli.execute request in
  Option.iter outcome.stdout ~f:Stdio.print_endline;
  List.iter outcome.stderr_lines ~f:(fun line -> Stdio.eprintf "%s\n" line);
  Stdlib.exit (Oq.Exit_code.to_int outcome.exit_code)

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
        ~doc:"Optional query pipeline (for example: .headings | .length).")

let cmd =
  let doc = "agent-first org query CLI" in
  let info = Cmd.info "oq" ~doc in
  let term =
    Term.(
      const run $ strict_arg $ now_arg $ tz_arg $ input_path_arg $ query_arg)
  in
  Cmd.v info term

let () = Stdlib.exit (Cmd.eval cmd)
