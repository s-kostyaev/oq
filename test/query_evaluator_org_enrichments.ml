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

let parse_inline path content =
  match Oq.Org.parse_string ~path content with
  | Ok doc -> doc
  | Error err ->
      failwithf "unexpected parse failure for %s: %s (%s)" path
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()

let run_ok ?now ?tz doc query =
  match Oq.Eval.run_text ?now ?tz ~doc query with
  | Ok value -> value
  | Error message ->
      failwithf "expected success for %S, got error: %s" query message ()

let run_error ?now ?tz doc query =
  match Oq.Eval.run_text ?now ?tz ~doc query with
  | Ok value -> failwithf "expected error for %S, got value: %s" query value ()
  | Error message -> message

let assert_contains text needle =
  assert (String.is_substring text ~substring:needle)

let () =
  let duplicate = parse_fixture "fixtures/corpus/duplicate_headings.org" in
  let todo = parse_fixture "fixtures/corpus/todo_workflows.org" in
  let props = parse_fixture "fixtures/corpus/properties_drawers.org" in
  let blocks = parse_fixture "fixtures/corpus/blocks_links_tables.org" in
  assert_contains (run_ok duplicate ".sections[0]") "* Projects (lines 3:13)";
  assert_contains (run_ok duplicate ".section('Inbox', 6:8)") "** Inbox (lines 6:8)";
  assert_contains (run_ok duplicate ".section_contains('Inbox')") "** Inbox (lines 6:8)";
  assert_contains (run_ok duplicate ".search('second inbox')") "* Projects (lines 3:13)";
  assert (String.equal (run_ok todo ".todos | .length") "2");
  assert (String.equal (run_ok todo ".done | .length") "1");
  assert (String.equal (run_ok todo ".scheduled | .length") "2");
  assert (String.equal (run_ok todo ".deadline | .length") "1");
  assert (String.equal (run_ok todo ".closed | .length") "1");
  assert_contains (run_ok todo ".todos") "* Prepare release (lines 4:9)";
  assert_contains (run_ok todo ".done") "* Submit weekly report (lines 13:14)";
  assert_contains (run_ok todo ".scheduled") "* Confirm changelog (lines 10:12)";
  assert_contains (run_ok todo ".deadline") "* Prepare release (lines 4:9)";
  assert_contains (run_ok todo ".closed") "* Submit weekly report (lines 13:14)";
  assert (String.equal (run_ok todo ".tags | .length") "3");
  assert (String.equal (run_ok props ".properties | .length") "4");
  assert
    (String.equal
       (run_ok props ".property('OWNER') | map(.value)")
       "Sergey\nTeam");
  let props_lower =
    parse_inline "properties-lowercase.org"
      {|
* Task
:PROPERTIES:
:owner: Alice
:END:
|}
  in
  assert (String.equal (run_ok props_lower ".property('OWNER') | .length") "1");
  assert (String.equal (run_ok props_lower ".property('owner') | .length") "1");
  assert (String.equal (run_ok blocks ".code('ocaml') | .length") "1");
  assert_contains (run_ok blocks ".code('ocaml') | .text") "let greeting = \"hello\"";
  assert_contains (run_ok blocks ".links | map(.target)") "https://example.com/docs";
  assert (String.equal (run_ok blocks ".tables | map(.row_count)") "4")

let () =
  let inline_doc =
    parse_inline "todo-fast-keys.org"
      {|
#+TODO: TODO(t) NEXT(n) WAIT(w@/!) | DONE(d!) CANCELED(c@)
* TODO Open item
* DONE Closed item
|}
  in
  assert (String.equal (run_ok inline_doc ".todos | .length") "1");
  assert (String.equal (run_ok inline_doc ".done | .length") "1");
  assert_contains (run_ok inline_doc ".todos") "* Open item (lines 3:3)";
  assert_contains (run_ok inline_doc ".done") "* Closed item (lines 4:4)"

let () =
  let inline_doc =
    parse_inline "src-no-language.org"
      {|
* Snippets
#+BEGIN_SRC :results output :exports both
(message "ok")
#+END_SRC
|}
  in
  assert (String.equal (run_ok inline_doc ".code | .length") "1");
  assert (String.equal (run_ok inline_doc ".code('ocaml') | .length") "0")

let () =
  let inline_doc =
    parse_inline "todo-multiple-lines.org"
      {|
#+TODO: TODO(t) NEXT(n) | DONE(d)
#+TODO: WAIT(w) HOLD(h) | CANCELED(c)
* TODO A
* WAIT B
* CANCELED C
|}
  in
  assert (String.equal (run_ok inline_doc ".todos | .length") "2");
  assert (String.equal (run_ok inline_doc ".done | .length") "1");
  assert_contains (run_ok inline_doc ".todos | map(.title)") "A";
  assert_contains (run_ok inline_doc ".todos | map(.title)") "B";
  assert_contains (run_ok inline_doc ".done | map(.title)") "C"

let () =
  let inline_doc =
    parse_inline "tags-tab.org"
      {|
* Task	:work:docs:
|}
  in
  assert (String.equal (run_ok inline_doc ".tags | .length") "2");
  assert_contains (run_ok inline_doc ".tags") "work";
  assert_contains (run_ok inline_doc ".tags") "docs"

let () =
  let inline_doc =
    parse_inline "tags-only.org"
      {|
* :work:docs:
|}
  in
  assert (String.equal (run_ok inline_doc ".tags | .length") "2");
  assert (String.equal (run_ok inline_doc ".headings[0].title") "");
  assert_contains (run_ok inline_doc ".tags") "work";
  assert_contains (run_ok inline_doc ".tags") "docs"

let () =
  let inline_doc =
    parse_inline "seq-typ-todo.org"
      {|
#+SEQ_TODO: NEXT WAIT | DONE
#+TYP_TODO: IDEA BLOCKED | ARCHIVED
* NEXT seq item
* IDEA type item
* ARCHIVED archived item
|}
  in
  assert (String.equal (run_ok inline_doc ".todos | .length") "2");
  assert (String.equal (run_ok inline_doc ".done | .length") "1");
  assert_contains (run_ok inline_doc ".todos | map(.title)") "seq item";
  assert_contains (run_ok inline_doc ".todos | map(.title)") "type item";
  assert_contains (run_ok inline_doc ".done | map(.title)") "archived item"

let () =
  let bom =
    String.init 3 ~f:(fun index ->
        Char.of_int_exn [| 0xEF; 0xBB; 0xBF |].(index))
  in
  let inline_doc =
    parse_inline "bom-todo.org"
      (bom ^ "#+SEQ_TODO: NEXT | DONE\n* NEXT Task\n* DONE Closed\n")
  in
  assert (String.equal (run_ok inline_doc ".todos | .length") "1");
  assert (String.equal (run_ok inline_doc ".done | .length") "1")

let () =
  let inline_doc =
    parse_inline "src-upper-lang.org"
      {|
* Demo
#+BEGIN_SRC OCAML
let x = 1
#+END_SRC
|}
  in
  assert (String.equal (run_ok inline_doc ".code('ocaml') | .length") "1");
  assert (String.equal (run_ok inline_doc ".code('OCAML') | .length") "1")

let () =
  let inline_doc =
    parse_inline "comment-lines.org"
      {|
* Note
# this is a comment with https://example.com
# SCHEDULED: <2026-02-18 Wed>
|}
  in
  assert (String.equal (run_ok inline_doc ".links | .length") "0");
  assert (String.equal (run_ok inline_doc ".scheduled | .length") "0")

let () =
  let inline_doc =
    parse_inline "fixed-width.org"
      {|
* Note
: code:
: another line
|}
  in
  assert (String.equal (run_ok inline_doc ".headings | .length") "1");
  assert (String.equal (run_ok inline_doc ".properties | .length") "0")

let () =
  let inline_doc =
    parse_inline "links-tab.org"
      {|
* Note
word	https://example.com/docs
|}
  in
  assert (String.equal (run_ok inline_doc ".links | .length") "1");
  assert_contains (run_ok inline_doc ".links | map(.target)") "https://example.com/docs"

let () =
  let inline_doc =
    parse_inline "links-bracket.org"
      {|
* Links
[[https://example.com/docs][Example Docs]]
|}
  in
  assert (String.equal (run_ok inline_doc ".links | .length") "1");
  assert (String.equal (run_ok inline_doc ".links | map(.kind)") "bracket")

let () =
  let inline_doc =
    parse_inline "links-parentheses.org"
      {|
* Links
https://en.wikipedia.org/wiki/Function_(mathematics)
|}
  in
  assert (String.equal (run_ok inline_doc ".links | .length") "1");
  assert_contains
    (run_ok inline_doc ".links | map(.target)")
    "https://en.wikipedia.org/wiki/Function_(mathematics)"

let () =
  let todo = parse_fixture "fixtures/corpus/todo_workflows.org" in
  let now = "2026-02-17T10:30:00-08:00" in
  let tz = "America/Los_Angeles" in
  assert
    (String.equal
       (run_ok ~now ~tz todo ".scheduled('next_7d') | map(.title)")
       "Prepare release\nConfirm changelog");
  assert (String.equal (run_ok ~now ~tz todo ".scheduled('today') | .length") "0");
  assert (String.equal (run_ok ~now ~tz todo ".scheduled('tomorrow') | .length") "0");
  assert (String.equal (run_ok ~now ~tz todo ".scheduled('next_7d') | .length") "2");
  assert (String.equal (run_ok ~now ~tz todo ".deadline('this_week') | .length") "1");
  assert (String.equal (run_ok ~now ~tz todo ".closed('yesterday') | .length") "1");
  let first = run_ok ~now ~tz todo ".scheduled('next_7d') | map(.title)" in
  let second = run_ok ~now ~tz todo ".scheduled('next_7d') | map(.title)" in
  assert (String.equal first second);
  assert
    (String.equal
       (run_ok ~now:"2026-02-22T08:00:00-08:00" ~tz todo
          ".scheduled('this_week') | .length")
       "2")

let () =
  let inline_doc =
    parse_inline "overdue.org"
      {|
#+TODO: TODO | DONE

* DONE Finished item
SCHEDULED: <2026-02-10 Tue>
DEADLINE: <2026-02-10 Tue>

* TODO Pending item
SCHEDULED: <2026-02-10 Tue>
DEADLINE: <2026-02-10 Tue>
|}
  in
  let now = "2026-02-11T08:00:00-08:00" in
  let tz = "America/Los_Angeles" in
  assert
    (String.equal
       (run_ok ~now ~tz inline_doc ".scheduled('overdue') | map(.title)")
       "Pending item");
  assert
    (String.equal
       (run_ok ~now ~tz inline_doc ".deadline('overdue') | map(.title)")
       "Pending item");
  let error = run_error ~now ~tz inline_doc ".closed('overdue')" in
  assert_contains error "supported: today|tomorrow|yesterday|this_week|next_7d"

let () =
  let inline_doc =
    parse_inline "combined-planning.org"
      {|
* TODO Combined planning
SCHEDULED: <2026-02-18 Wed> DEADLINE: <2026-02-20 Fri>
|}
  in
  let now = "2026-02-17T08:00:00-08:00" in
  let tz = "America/Los_Angeles" in
  assert (String.equal (run_ok inline_doc ".scheduled | .length") "1");
  assert (String.equal (run_ok inline_doc ".deadline | .length") "1");
  assert
    (String.equal
       (run_ok ~now ~tz inline_doc ".scheduled('next_7d') | .length")
       "1");
  assert
    (String.equal
       (run_ok ~now ~tz inline_doc ".deadline('next_7d') | .length")
       "1")

let () =
  let inline_doc =
    parse_inline "planning-in-text.org"
      {|
* Note
This sentence mentions SCHEDULED: <2026-02-18 Wed> but is plain text.
|}
  in
  assert (String.equal (run_ok inline_doc ".scheduled | .length") "0")

let () =
  let inline_doc =
    parse_inline "planning-diary-sexp.org"
      {|
* TODO Recurring
SCHEDULED: <%%(diary-float t 5 2)>
|}
  in
  let now = "2026-02-17T08:00:00-08:00" in
  let tz = "America/Los_Angeles" in
  assert (String.equal (run_ok inline_doc ".scheduled | .length") "1");
  assert
    (String.equal
       (run_ok ~now ~tz inline_doc ".scheduled('next_7d') | .length")
       "0")

let () =
  let todo = parse_fixture "fixtures/corpus/todo_workflows.org" in
  assert_contains
    (run_error ~now:"2026-02-17" todo ".headings | .length")
    "invalid --now value";
  assert_contains
    (run_error ~now:"2026-02-17T10:30:00" todo ".headings | .length")
    "invalid --now value";
  assert_contains
    (run_error ~now:"2026-02-17 10:30:00-08:00" todo ".headings | .length")
    "invalid --now value";
  assert_contains
    (run_error ~tz:"Mars/Phobos" todo ".headings | .length")
    "invalid --tz value";
  assert_contains
    (run_error ~now:"2026-02-17T10:30:00+00:00" ~tz:"America/Los_Angeles" todo
       ".headings | .length")
    "offset mismatch between --now"
