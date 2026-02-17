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
      ignore (write_file root "upper.ORG" "* Upper\nBody\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "upper.ORG:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "archive.org_archive" "* Archived\nBody\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "archive.org_archive:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      let input_path = write_file root "single.ORG" "* Task\nBody\n" in
      let request : Oq.Cli.request =
        { input_path; query = Some ".headings | .length"; strict = false; now = None; tz = None }
      in
      let outcome = Oq.Cli.execute request in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert_contains stdout "1")

let () =
  with_temp_dir (fun root ->
      let input_path = write_file root "single-no-ext" "* Task\nBody\n" in
      let request : Oq.Cli.request =
        { input_path; query = Some ".headings | .length"; strict = false; now = None; tz = None }
      in
      let outcome = Oq.Cli.execute request in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert_contains stdout "1")

let () =
  with_temp_dir (fun root ->
      let target = write_file root "real.org" "* Task\nBody\n" in
      let input_path = Filename.concat root "link.org" in
      Caml_unix.symlink target input_path;
      let request : Oq.Cli.request =
        { input_path; query = Some ".headings | .length"; strict = false; now = None; tz = None }
      in
      let outcome = Oq.Cli.execute request in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert_contains stdout "1")

let () =
  with_temp_dir (fun root ->
      let real_dir = Filename.concat root "real-dir" in
      Caml_unix.mkdir real_dir 0o755;
      ignore (write_file root "real-dir/a.org" "* Task\nBody\n");
      let input_path = Filename.concat root "link-dir" in
      Caml_unix.symlink real_dir input_path;
      let request : Oq.Cli.request =
        { input_path; query = Some ".headings | .length"; strict = false; now = None; tz = None }
      in
      let outcome = Oq.Cli.execute request in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "a.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      let input_path = write_file root ".org" "* Hidden file\nBody\n" in
      let request : Oq.Cli.request =
        { input_path; query = Some ".headings | .length"; strict = false; now = None; tz = None }
      in
      let outcome = Oq.Cli.execute request in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert_contains stdout "1")

let () =
  with_temp_dir (fun root ->
      let good = write_file root "good.org" "* Alpha\nBody\n" in
      let invalid = String.make 1 (Char.of_int_exn 0xFF) in
      ignore (write_file root "bad.org" invalid);
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
               ~substring:"Warning: failed to parse bad.org: invalid_utf8")))

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
        (write_file root "dynamic-block.org"
           "* Root\n#+BEGIN: clocktable :scope file\n:UNRELATED:\n#+END:\n** Child\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "dynamic-block.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "dynamic-heading-before-end.org"
           "* Root\n#+BEGIN: clocktable :scope file\n** Child\n#+END:\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "dynamic-heading-before-end.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "dynamic-block-bad-end.org"
           "#+BEGIN: clocktable :scope file\n* Inner\n#+END:foo\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "dynamic-block-bad-end.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "block-unclosed-supported.org"
           "* H\n#+BEGIN_SRC ocaml\n** Child\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "block-unclosed-supported.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "block-unclosed-opaque.org"
           "* H\n#+BEGIN_CENTER\n** Child\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "block-unclosed-opaque.org:";
      assert_contains stdout "  2")

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
        (write_file root "src-no-lang-switch.org"
           "* Snippets\n#+BEGIN_SRC -n :results output\n(message \"ok\")\n#+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code('-n') | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-no-lang-switch.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-no-lang-plus-switch.org"
           "* Snippets\n#+BEGIN_SRC +n :results output\n(message \"ok\")\n#+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code('+n') | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-no-lang-plus-switch.org:";
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
        (write_file root "planning-lower.org"
           "* TODO Combined planning\nscheduled: <2026-02-18 Wed>\n");
      let outcome = run_directory ~query:(Some ".scheduled | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "planning-lower.org:";
      assert_contains stdout "  1")

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
      ignore
        (write_file root "src-end-trailing.org"
           "* Demo\n#+BEGIN_SRC ocaml\nlet x = 1\n#+END_SRC trailing text\n");
      let outcome = run_directory ~query:(Some ".code('ocaml') | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-end-trailing.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-indented-markers.org"
           "* H\n  #+BEGIN_SRC ocaml\n** Child\n  #+END_SRC\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-indented-markers.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-indented-end-marker.org"
           "* H\n#+BEGIN_SRC ocaml\nhttps://example.com\n  #+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-indented-end-marker.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-indented-begin-marker.org"
           "* H\n  #+BEGIN_SRC ocaml\nhttps://example.com\n#+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-indented-begin-marker.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-indented-begin-end-markers.org"
           "* H\n  #+BEGIN_SRC ocaml\nhttps://example.com\n  #+END_SRC\n");
      let outcome = run_directory ~query:(Some ".code | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-indented-begin-end-markers.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "src-heading-before-end.org"
           "* H\n#+BEGIN_SRC ocaml\n** Child\n#+END_SRC\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "src-heading-before-end.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "opaque-heading-before-end.org"
           "* H\n#+BEGIN_CENTER\n** Child\n#+END_CENTER\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "opaque-heading-before-end.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "comment-lines.org"
           "* Note\n# comment with https://example.com\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "comment-lines.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "comment-block-with-heading-like-content.org"
           "* Root\n#+BEGIN_COMMENT\n** Hidden\n#+END_COMMENT\n** Child\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "comment-block-with-heading-like-content.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "fixed-width.org"
           "* Note\n: code:\n: another line\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "fixed-width.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "indented-colon.org" "* Note\n  :foo:\n  literal text\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "indented-colon.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "colon-token.org" "* Note\n:a:b:\ntext\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "colon-token.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "emoji-token.org" "* Note\n:+1:\ntext\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "emoji-token.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "smile-token.org" "* Note\n:smile:\ntext\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "smile-token.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "todo-token.org" "* Note\n:TODO:\ntext\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "todo-token.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "custom-drawer-opaque.org"
           "* Note\n:MYDRAWER:\n#+BEGIN_SRC\nliteral\n:END:\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "custom-drawer-opaque.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "custom-drawer-heading-like.org"
           "* Note\n:MYDRAWER:\n* plain\n#+BEGIN_SRC\n:END:\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "custom-drawer-heading-like.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "drawer-indented-end.org"
           "* Note\n:PROPERTIES:\n:OWNER: Alice\n  :END:\n");
      let outcome = run_directory ~query:(Some ".properties | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "drawer-indented-end.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "drawer-tabbed-end.org"
           "* Note\n:PROPERTIES:\n:OWNER: Alice\n\t:END:\n");
      let outcome = run_directory ~query:(Some ".properties | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "drawer-tabbed-end.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "drawer-unclosed-known.org"
           "* Note\n:PROPERTIES:\n:OWNER: Alice\n** Child\n");
      let outcome = run_directory ~query:(Some ".headings | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "drawer-unclosed-known.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-tab.org"
           "* Note\nword\thttps://example.com/docs\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-tab.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-uppercase-scheme.org"
           "* Note\nHTTPS://example.com/docs\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-uppercase-scheme.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-generic-scheme.org"
           "* Note\nftp://example.com/pub\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-generic-scheme.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-file-mailto.org"
           "* Note\nfile:notes.org\nmailto:user@example.com\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-file-mailto.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-id-custom-id.org"
           "* Note\nid:task-123\ncustom-id:release-notes\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-id-custom-id.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-news-shell.org"
           "* Note\nnews:comp.emacs\nshell:echo hello\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-news-shell.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-man-woman.org"
           "* Note\nman:printf\nwoman:printf\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-man-woman.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "links-calc.org" "* Note\ncalc:2+2\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-calc.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-mobile-geo.org"
           "* Note\ntel:+123456789\nsms:+123456789\ngeo:37.786971,-122.399677\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-mobile-geo.org:";
      assert_contains stdout "  3")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "links-doi.org" "* Note\ndoi:10.1000/182\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-doi.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "links-attachment.org" "* Note\nattachment:report.pdf\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-attachment.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore (write_file root "links-coderef.org" "* Note\ncoderef:label\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-coderef.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-file-plus.org"
           "* Note\nfile+sys:/tmp/report.txt\nfile+emacs:/tmp/report.txt\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-file-plus.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-file-no-scheme.org"
           "* Note\n./notes.org\n../docs/spec.org\n/ssh:me@host:/tmp/notes.org\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-file-no-scheme.org:";
      assert_contains stdout "  3")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-file-tilde.org"
           "* Note\n~/notes.org\n~alice/docs/spec.org\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-file-tilde.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-file-windows.org"
           "* Note\nC:/work/notes.org\nD:\\docs\\spec.org\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-file-windows.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-file-windows-unc.org"
           "* Note\n\\\\server\\share\\notes.org\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-file-windows-unc.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-more-schemes.org"
           "* Note\nirc:#emacs\ngnus:group\ndocview:/tmp/a.pdf::5\nrmail:Inbox\nbbdb:John_Doe\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-more-schemes.org:";
      assert_contains stdout "  5")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-mail-schemes.org"
           "* Note\nmhe:inbox\nwl:folder\nvm:inbox\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-mail-schemes.org:";
      assert_contains stdout "  3")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-vm-imap.org"
           "* Note\nvm-imap:work:Inbox\nvm-imap:work:Inbox#123\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-vm-imap.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-custom-abbrev.org"
           "#+LINK: gh https://github.com/%s\n* Note\ngh:ocaml/dune\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-custom-abbrev.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-custom-abbrev-late.org"
           "* Note\ngh:ocaml/dune\n#+LINK: gh https://github.com/%s\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-custom-abbrev-late.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-custom-abbrev-in-src.org"
           "* Note\n#+BEGIN_SRC text\n#+LINK: gh https://github.com/%s\n#+END_SRC\ngh:ocaml/dune\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-custom-abbrev-in-src.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-custom-abbrev-in-drawer.org"
           "* Note\n:MYDRAWER:\n#+LINK: gh https://github.com/%s\n:END:\ngh:ocaml/dune\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-custom-abbrev-in-drawer.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-in-custom-drawer.org"
           "* Note\n:MYDRAWER:\nhttps://example.com\n:END:\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-in-custom-drawer.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-in-center-block.org"
           "* Note\n#+BEGIN_CENTER\nhttps://example.com\n#+END_CENTER\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-in-center-block.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-in-dynamic-block.org"
           "* Note\n#+BEGIN: clocktable :scope file\nhttps://example.com\n#+END:\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-in-dynamic-block.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-in-quote-block.org"
           "* Note\n#+BEGIN_QUOTE\nhttps://example.com\n#+END_QUOTE\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-in-quote-block.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-in-comment-block.org"
           "* Note\n#+BEGIN_COMMENT\nhttps://example.com\n#+END_COMMENT\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-in-comment-block.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-in-table.org"
           "* Note\n| Name | URL |\n|------+-----|\n| x | https://example.com |\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-in-table.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-in-compact-table-cell.org"
           "* Note\n|https://example.com|\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-in-compact-table-cell.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-custom-abbrev-indented-keyword.org"
           "* Note\n  #+LINK: gh https://github.com/%s\ngh:ocaml/dune\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-custom-abbrev-indented-keyword.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-custom-abbrev-in-indented-drawer.org"
           "* Note\n  :MYDRAWER:\n  #+LINK: gh https://github.com/%s\n  :END:\ngh:ocaml/dune\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-custom-abbrev-in-indented-drawer.org:";
      assert_contains stdout "  0")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-bracket.org"
           "* Links\n[[https://example.com/docs][Example Docs]]\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-bracket.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-bracket-after-broken-open.org"
           "* Links\nbroken [[ token then [[https://example.com/docs][ok]]\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-bracket-after-broken-open.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-parentheses.org"
           "* Links\nhttps://en.wikipedia.org/wiki/Function_(mathematics)\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-parentheses.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-trailing-punctuation.org"
           "* Links\nVisit https://example.com/docs! and then https://example.com/help?\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-trailing-punctuation.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-leading-punctuation.org"
           "* Links\n...https://example.com/docs\n,https://example.com/help\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-leading-punctuation.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-angle-spaces.org"
           "* Links\n<https://example.com/path with spaces>\n<bbdb:R.* Stallman>\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-angle-spaces.org:";
      assert_contains stdout "  2")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-angle-after-less-than.org"
           "* Links\nif x < y then <https://example.com/docs>\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-angle-after-less-than.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "links-duplicate-occurrences.org"
           "* Links\nhttps://example.com https://example.com\n<https://example.org> https://example.org\n");
      let outcome = run_directory ~query:(Some ".links | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "links-duplicate-occurrences.org:";
      assert_contains stdout "  4")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "planning-diary-sexp.org"
           "* TODO Recurring\nSCHEDULED: <%%(diary-float t 5 2)>\n");
      let outcome = run_directory ~query:(Some ".scheduled | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "planning-diary-sexp.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      ignore
        (write_file root "properties-lowercase.org"
           "* Task\n:PROPERTIES:\n:owner: Alice\n:END:\n");
      let outcome = run_directory ~query:(Some ".property('OWNER') | .length") root in
      assert_exit outcome Oq.Exit_code.Success;
      let stdout = require_stdout outcome in
      assert (extract_counter stdout "candidate_org" = 1);
      assert (extract_counter stdout "parsed_ok" = 1);
      assert (extract_counter stdout "parse_failed" = 0);
      assert_contains stdout "properties-lowercase.org:";
      assert_contains stdout "  1")

let () =
  with_temp_dir (fun root ->
      let invalid_a = String.make 1 (Char.of_int_exn 0xFF) in
      let invalid_b = String.make 1 (Char.of_int_exn 0xFE) in
      ignore (write_file root "broken-a.org" invalid_a);
      ignore (write_file root "broken-b.org" invalid_b);
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
      let invalid = String.make 1 (Char.of_int_exn 0xFF) in
      ignore (write_file root "broken.org" invalid);
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
