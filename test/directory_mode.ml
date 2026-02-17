open Core

let assert_contains text needle =
  assert (String.is_substring text ~substring:needle)

let create_temp_dir () =
  let base = Option.value (Sys.getenv "TMPDIR") ~default:"/tmp" in
  let rec loop attempt =
    let path =
      Filename.concat base
        (sprintf "oq-directory-mode-%d-%d" (Caml_unix.getpid ()) attempt)
    in
    if Stdlib.Sys.file_exists path then loop (attempt + 1)
    else (
      Caml_unix.mkdir path 0o755;
      path)
  in
  loop 0

let rec ensure_dir path =
  if String.equal path "" || String.equal path "." || String.equal path "/" then ()
  else if Stdlib.Sys.file_exists path then ()
  else (
    ensure_dir (Filename.dirname path);
    Caml_unix.mkdir path 0o755)

let write_file root relative_path content =
  let path = Filename.concat root relative_path in
  ensure_dir (Filename.dirname path);
  Out_channel.write_all path ~data:content;
  path

let rec remove_path path =
  match Caml_unix.lstat path with
  | exception Caml_unix.Unix_error (Caml_unix.ENOENT, _, _) -> ()
  | stats -> (
      match stats.st_kind with
      | Caml_unix.S_DIR ->
          Stdlib.Sys.readdir path
          |> Array.iter ~f:(fun name ->
                 if String.equal name "." || String.equal name ".." then ()
                 else remove_path (Filename.concat path name));
          Caml_unix.rmdir path
      | _ -> Caml_unix.unlink path)

let with_temp_dir f =
  let root = create_temp_dir () in
  Exn.protect ~f:(fun () -> f root) ~finally:(fun () -> remove_path root)

let assert_exit outcome expected =
  assert
    (Int.equal
       (Oq.Exit_code.to_int outcome.Oq.Cli.exit_code)
       (Oq.Exit_code.to_int expected))

let require_stdout outcome =
  match outcome.Oq.Cli.stdout with
  | Some stdout -> stdout
  | None -> failwith "expected stdout output"

let run_directory ?(strict = false) ?(query = None) root =
  let request : Oq.Cli.request =
    { input_path = root; query; strict; now = None; tz = None }
  in
  Oq.Cli.execute request

let extract_counter stdout key =
  let prefix = key ^ "=" in
  match String.split_lines stdout |> List.find ~f:(String.is_prefix ~prefix) with
  | Some line ->
      Int.of_string
        (String.drop_prefix line (String.length key + String.length "="))
  | None -> failwithf "missing counter %S in stdout:\n%s" key stdout ()

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "notes.txt" "plain text");
      ignore (write_file root ".hidden/ignored.org" "* Hidden\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Query_or_usage_error;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 0);
      assert (extract_counter stdout "parsed_ok" = 0);
      assert (extract_counter stdout "parse_failed" = 0);
      assert (extract_counter stdout "skipped_hidden" = 1);
      assert
        (List.exists outcome.stderr_lines ~f:(fun line ->
             String.is_substring line
               ~substring:"no candidate .org files found after hidden/symlink \
                           filters")))

let () =
  with_temp_dir (fun root ->
      let good = write_file root "good.org" "* Alpha\nBody\n" in
      ignore (write_file root "bad.org" "#+BEGIN_SRC ocaml\nlet x = 1\n");
      ignore (write_file root ".secret.org" "* Hidden\n");
      Caml_unix.symlink good (Filename.concat root "symlink.org");
      let outcome = run_directory ~query:(Some ".headings") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 2);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 1);
      assert (extract_counter stdout "skipped_hidden" = 1);
      assert (extract_counter stdout "skipped_symlink" = 1);
      assert_contains stdout "good.org:";
      assert_contains stdout "* Alpha (lines 1:2)";
      assert
        (List.exists outcome.stderr_lines ~f:(fun line ->
             String.is_substring line
               ~substring:"Warning: failed to parse bad.org: syntax_error")))

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "opaque-valid.org"
           "* Notes\n#+BEGIN_CENTER\nhello\n#+END_CENTER\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "opaque-valid.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "tab-block.org"
           "*\tTask\n#+BEGIN_SRC\tocaml\nlet y = 2\n#+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code('ocaml') | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "tab-block.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-headers.org"
           "* Snippets\n#+BEGIN_SRC emacs-lisp :results output :exports both\n(message \"ok\")\n#+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code('emacs-lisp') | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-headers.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "todo-fast-keys.org"
           "#+TODO: TODO(t) NEXT(n) | DONE(d)\n* TODO Task\n* DONE Finished\n");
      let outcome = run_directory ~query:(Some ".todos | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "todo-fast-keys.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-no-lang.org"
           "* Snippets\n#+BEGIN_SRC :results output\n(message \"ok\")\n#+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code('ocaml') | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-no-lang.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "todo-multi.org"
           "#+TODO: TODO(t) NEXT(n) | DONE(d)\n#+TODO: WAIT(w) HOLD(h) | CANCELED(c)\n* TODO A\n* WAIT B\n* CANCELED C\n");
      let outcome = run_directory ~query:(Some ".todos | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "todo-multi.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "tags-tab.org" "* Task\t:work:docs:\n");
      let outcome = run_directory ~query:(Some ".tags | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "tags-tab.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "tags-only.org" "* :work:docs:\n");
      let outcome = run_directory ~query:(Some ".tags | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "tags-only.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "planning-in-text.org"
           "* Note\nThis sentence mentions SCHEDULED: <2026-02-18 Wed> but is plain text.\n");
      let outcome = run_directory ~query:(Some ".scheduled | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "planning-in-text.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "seq-typ-todo.org"
           "#+SEQ_TODO: NEXT WAIT | DONE\n#+TYP_TODO: IDEA BLOCKED | ARCHIVED\n* NEXT seq item\n* IDEA type item\n* ARCHIVED archived item\n");
      let outcome = run_directory ~query:(Some ".todos | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "seq-typ-todo.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      let bom =
        String.init 3 ~f:(fun index ->
            Char.of_int_exn [| 0xEF; 0xBB; 0xBF |].(index))
      in
      ignore
        (write_file root "bom-todo.org"
           (bom ^ "#+SEQ_TODO: NEXT | DONE\n* NEXT Task\n* DONE Closed\n"));
      let outcome = run_directory ~query:(Some ".todos | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "bom-todo.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-upper-lang.org"
           "* Demo\n#+BEGIN_SRC OCAML\nlet x = 1\n#+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code('ocaml') | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-upper-lang.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "broken-a.org" "#+BEGIN_SRC ocaml\nlet a = 1\n");
      ignore (write_file root "broken-b.org" "#+BEGIN_QUOTE\nunterminated\n");
      let outcome = run_directory ~query:(Some ".headings") root in
      assert_exit outcome Oq.Exit_code.Parse_coverage_error;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 2);
      assert (extract_counter stdout "parsed_ok" = 0);
      assert (extract_counter stdout "parse_failed" = 2);
      let warning_count =
        List.count outcome.stderr_lines ~f:(fun line ->
            String.is_substring line ~substring:"Warning: failed to parse ")
      in
      assert (warning_count = 2);
      assert
        (List.exists outcome.stderr_lines ~f:(fun line ->
             String.is_substring line
               ~substring:
                 "parse coverage failure: zero candidate .org files parsed \
                  successfully")))

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "good.org" "* Alpha\nBody\n");
      ignore (write_file root "broken.org" "#+BEGIN_SRC ocaml\nlet x = 1\n");
      let outcome = run_directory ~strict:true ~query:(Some ".headings") root in
      assert_exit outcome Oq.Exit_code.Parse_coverage_error;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 2);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 1);
      assert
        (List.exists outcome.stderr_lines ~f:(fun line ->
             String.is_substring line
               ~substring:
                 "strict mode failed: at least one .org file did not parse")))

let () =
  with_temp_dir (fun root ->
      for index = 0 to 239 do
        ignore
          (write_file root
             (sprintf "corpus/%03d-note.org" index)
             (sprintf "* Note %03d\nBody %03d\n" index index))
      done;
      let outcome_a = run_directory ~query:(Some ".headings[0].title") root in
      let outcome_b = run_directory ~query:(Some ".headings[0].title") root in
      assert_exit outcome_a Oq.Exit_code.Success;
      assert_exit outcome_b Oq.Exit_code.Success;
      let stdout_a = require_stdout outcome_a in
      let stdout_b = require_stdout outcome_b in
      assert (String.equal stdout_a stdout_b);
      assert (extract_counter stdout_a "candidate_org" = 240);
      assert (extract_counter stdout_a "parsed_ok" = 240);
      assert (extract_counter stdout_a "parse_failed" = 0);
      let group_headers =
        String.split_lines stdout_a
        |> List.filter ~f:(fun line ->
               String.is_suffix line ~suffix:":"
               && not (String.is_substring line ~substring:"="))
      in
      assert (List.length group_headers = 240);
      assert (String.equal (List.hd_exn group_headers) "corpus/000-note.org:");
      assert
        (String.equal (List.last_exn group_headers) "corpus/239-note.org:"))
