open Core
open Cmdliner

let error_class_conv =
  let parse = function
    | "query" -> Ok Oq.Exit_code.Query_or_usage_error
    | "io" -> Ok Oq.Exit_code.Io_or_permission_error
    | "parse" -> Ok Oq.Exit_code.Parse_coverage_error
    | other ->
        Error
          (`Msg
            (sprintf
               "unsupported error class %S (supported: query|io|parse)"
               other))
  in
  let print formatter code =
    let text =
      match code with
      | Oq.Exit_code.Query_or_usage_error -> "query"
      | Oq.Exit_code.Io_or_permission_error -> "io"
      | Oq.Exit_code.Parse_coverage_error -> "parse"
      | Oq.Exit_code.Success -> "success"
    in
    Format.fprintf formatter "%s" text
  in
  Arg.conv (parse, print)

let run warning error_class message =
  Option.iter warning ~f:(fun text ->
      Stdio.eprintf "%s\n" (Oq.Diagnostic.warning text));
  match error_class with
  | Some code ->
      Stdio.eprintf "%s\n" (Oq.Diagnostic.error message);
      Stdlib.exit (Oq.Exit_code.to_int code)
  | None ->
      Stdio.print_endline (Oq.Cli.run ())

let warning_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "warning" ] ~docv:"MESSAGE"
        ~doc:"Emit a canonical warning line to stderr before normal output.")

let error_class_arg =
  Arg.(
    value
    & opt (some error_class_conv) None
    & info [ "error-class" ] ~docv:"CLASS"
        ~doc:
          "Simulate a classified failure with exit code mapping (query|io|parse).")

let message_arg =
  Arg.(
    value
    & opt string "operation failed"
    & info [ "message" ] ~docv:"TEXT"
        ~doc:"Message used for canonical Error output.")

let cmd =
  let doc = "agent-first org query CLI (bootstrap command contract)" in
  let info = Cmd.info "oq" ~doc in
  let term = Term.(const run $ warning_arg $ error_class_arg $ message_arg) in
  Cmd.v info term

let () = Stdlib.exit (Cmd.eval cmd)
