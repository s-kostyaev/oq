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

let find_heading_by_title (headings : Oq.Org.heading list) title =
  List.find headings ~f:(fun heading -> String.equal heading.Oq.Org.title title)

let () =
  let duplicate = parse_fixture "fixtures/corpus/duplicate_headings.org" in
  let headings = duplicate.index.headings in
  assert (List.length headings = 4);
  match find_heading_by_title headings "Projects" with
  | Some heading ->
      assert (heading.source.span.start_line = 3);
      assert (heading.section_source.span.start_line = 3);
      assert (heading.section_source.span.end_line = 13)
  | None -> failwith "missing Projects heading"

let () =
  let duplicate = parse_fixture "fixtures/corpus/duplicate_headings.org" in
  let headings = duplicate.index.headings in
  let inbox_spans =
    List.filter headings ~f:(fun heading -> String.equal heading.title "Inbox")
    |> List.map ~f:(fun heading ->
           (heading.section_source.span.start_line, heading.section_source.span.end_line))
  in
  assert (Poly.equal inbox_spans [ (6, 8); (9, 13) ])

let () =
  let todo = parse_fixture "fixtures/corpus/todo_workflows.org" in
  assert (Poly.equal todo.todo_config.open_states [ "TODO"; "NEXT"; "WAIT" ]);
  assert (Poly.equal todo.todo_config.done_states [ "DONE"; "CANCELED" ]);
  assert (List.length todo.index.todos = 3);
  assert (List.length todo.index.planning = 4)

let () =
  match Oq.Org.parse_string ~path:"tags-tab.org" "* Task\t:work:docs:\n" with
  | Error err ->
      failwithf "expected tab-delimited tags parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.headings with
      | [ heading ] -> assert (Poly.equal heading.tags [ "work"; "docs" ])
      | _ -> failwith "expected one heading with tags")

let () =
  match Oq.Org.parse_string ~path:"tags-only.org" "* :work:docs:\n" with
  | Error err ->
      failwithf "expected tags-only heading parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.headings with
      | [ heading ] ->
          assert (String.is_empty heading.title);
          assert (Poly.equal heading.tags [ "work"; "docs" ])
      | _ -> failwith "expected one tags-only heading")

let () =
  match
    Oq.Org.parse_string ~path:"todo-fast-keys.org"
      "#+TODO: TODO(t) NEXT(n) WAIT(w@/!) | DONE(d!) CANCELED(c@)\n* TODO task one\n* DONE task two\n"
  with
  | Error err ->
      failwithf "expected todo fast-keys parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (Poly.equal doc.todo_config.open_states [ "TODO"; "NEXT"; "WAIT" ]);
      assert (Poly.equal doc.todo_config.done_states [ "DONE"; "CANCELED" ]);
      assert (List.length doc.index.todos = 2)

let () =
  match
    Oq.Org.parse_string ~path:"todo-multiple-lines.org"
      "#+TODO: TODO(t) NEXT(n) | DONE(d)\n#+TODO: WAIT(w) HOLD(h) | CANCELED(c)\n* TODO A\n* WAIT B\n* CANCELED C\n"
  with
  | Error err ->
      failwithf "expected multiple TODO lines parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert
        (Poly.equal doc.todo_config.open_states [ "TODO"; "NEXT"; "WAIT"; "HOLD" ]);
      assert (Poly.equal doc.todo_config.done_states [ "DONE"; "CANCELED" ]);
      assert (List.length doc.index.todos = 3)

let () =
  let props = parse_fixture "fixtures/corpus/properties_drawers.org" in
  assert (List.length props.index.properties = 4);
  assert (List.length props.index.drawers = 2);
  let keys = List.map props.index.properties ~f:(fun property -> property.key) in
  assert (List.mem keys "OWNER" ~equal:String.equal);
  assert (List.mem keys "EFFORT" ~equal:String.equal);
  assert (List.mem keys "CUSTOM_ID" ~equal:String.equal)

let () =
  let blocks = parse_fixture "fixtures/corpus/blocks_links_tables.org" in
  assert (List.length blocks.index.blocks = 2);
  assert (List.length blocks.index.tables = 1);
  assert (List.length blocks.index.links >= 1);
  match blocks.index.tables with
  | [ table ] ->
      assert (table.source.span.start_line = 14);
      assert (table.source.span.end_line = 17);
      assert (List.length table.rows = 4)
  | _ -> failwith "expected a single table"

let () =
  match Oq.Org.parse_string ~path:"broken.org" "#+BEGIN_SRC ocaml\nlet x = 1\n" with
  | Error { reason = Oq.Diagnostic.Syntax_error; line = Some 1; _ } -> ()
  | Error err ->
      failwithf "expected syntax_error, got %s" 
        (Oq.Diagnostic.parse_reason_to_string err.reason) ()
  | Ok _ -> failwith "expected parse failure for unterminated block"

let () =
  match
    Oq.Org.parse_string ~path:"opaque-blocks.org"
      {|
* Root
#+BEGIN_CENTER
Centered text.
#+END_CENTER

#+BEGIN_COMMENT
#+END_SRC
* Not a heading inside comment block
#+END_COMMENT

#+BEGIN_VERSE
Line one.
Line two.
#+END_VERSE

#+BEGIN_SRC ocaml
let x = 1
#+END_SRC

** Child
SCHEDULED: <2026-02-18 Wed>
|}
  with
  | Error err ->
      failwithf "expected opaque block parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 2);
      assert (List.length doc.index.blocks = 1);
      assert (List.length doc.index.planning = 1)

let () =
  match Oq.Org.parse_string ~path:"broken-opaque.org" "#+BEGIN_CENTER\ntext\n" with
  | Error { reason = Oq.Diagnostic.Syntax_error; line = Some 1; _ } -> ()
  | Error err ->
      failwithf "expected syntax_error for unterminated opaque block, got %s"
        (Oq.Diagnostic.parse_reason_to_string err.reason) ()
  | Ok _ -> failwith "expected parse failure for unterminated opaque block"

let () =
  match
    Oq.Org.parse_string ~path:"export.org"
      "#+begin_export html\n<div>ok</div>\n#+end_export\n"
  with
  | Error err ->
      failwithf "expected export block parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.blocks with
      | [ block ] ->
          assert (Poly.equal block.kind Oq.Org.Export);
          assert (String.equal (Option.value block.language ~default:"") "html")
      | _ -> failwith "expected one export block")

let () =
  match
    Oq.Org.parse_string ~path:"tab-whitespace.org"
      "*\tTODO [#A] Tab heading :tag:\n#+BEGIN_SRC\tocaml\nlet x = 1\n#+END_SRC\n"
  with
  | Error err ->
      failwithf "expected tab-whitespace parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match (doc.index.headings, doc.index.blocks) with
      | [ heading ], [ block ] ->
          assert (heading.level = 1);
          assert (String.equal heading.title "Tab heading");
          assert
            (String.equal
               (Option.value heading.todo_keyword ~default:"")
               "TODO");
          assert (Poly.equal block.kind Oq.Org.Src);
          assert (String.equal (Option.value block.language ~default:"") "ocaml")
      | _ -> failwith "expected one heading and one src block")

let () =
  match
    Oq.Org.parse_string ~path:"src-header-args.org"
      "* Snippets\n#+BEGIN_SRC emacs-lisp :results output :exports both\n(message \"ok\")\n#+END_SRC\n"
  with
  | Error err ->
      failwithf "expected src header args parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.blocks with
      | [ block ] ->
          assert (Poly.equal block.kind Oq.Org.Src);
          assert
            (String.equal
               (Option.value block.language ~default:"")
               "emacs-lisp")
      | _ -> failwith "expected one src block with header args")

let () =
  match
    Oq.Org.parse_string ~path:"src-no-language.org"
      "* Snippets\n#+BEGIN_SRC :results output :exports both\n(message \"ok\")\n#+END_SRC\n"
  with
  | Error err ->
      failwithf "expected src no-language parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.blocks with
      | [ block ] ->
          assert (Poly.equal block.kind Oq.Org.Src);
          assert (Option.is_none block.language)
      | _ -> failwith "expected one src block without language")

let () =
  match
    Oq.Org.parse_string ~path:"planning-combined.org"
      "* TODO Combined planning\nSCHEDULED: <2026-02-18 Wed> DEADLINE: <2026-02-20 Fri> CLOSED: [2026-02-21 Sat]\n"
  with
  | Error err ->
      failwithf "expected combined planning parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      let by_kind kind =
        List.find doc.index.planning ~f:(fun item -> Poly.equal item.kind kind)
      in
      assert (List.length doc.index.planning = 3);
      assert
        (String.equal
           (Option.value_map (by_kind Oq.Org.Scheduled) ~default:""
              ~f:(fun item -> item.raw_value))
           "<2026-02-18 Wed>");
      assert
        (String.equal
           (Option.value_map (by_kind Oq.Org.Deadline) ~default:""
              ~f:(fun item -> item.raw_value))
           "<2026-02-20 Fri>");
      assert
        (String.equal
           (Option.value_map (by_kind Oq.Org.Closed) ~default:""
              ~f:(fun item -> item.raw_value))
           "[2026-02-21 Sat]")

let () =
  let bytes = Bytes.create 1 in
  Bytes.set bytes 0 (Char.of_int_exn 0xFF);
  let invalid = Bytes.to_string bytes in
  match Oq.Org.parse_string ~path:"invalid.org" invalid with
  | Error { reason = Oq.Diagnostic.Invalid_utf8; _ } -> ()
  | Error err ->
      failwithf "expected invalid_utf8, got %s"
        (Oq.Diagnostic.parse_reason_to_string err.reason) ()
  | Ok _ -> failwith "expected parse failure for invalid utf8"

let () =
  assert
    (String.equal
       (Oq.Diagnostic.parse_reason_to_string Oq.Diagnostic.Syntax_error)
       "syntax_error");
  assert
    (String.equal
       (Oq.Diagnostic.parse_reason_to_string Oq.Diagnostic.Unsupported_construct)
       "unsupported_construct");
  assert
    (String.equal
       (Oq.Diagnostic.parse_reason_to_string Oq.Diagnostic.Invalid_utf8)
       "invalid_utf8");
  assert
    (String.equal
       (Oq.Diagnostic.parse_reason_to_string Oq.Diagnostic.Internal_parser_error)
       "internal_parser_error")
