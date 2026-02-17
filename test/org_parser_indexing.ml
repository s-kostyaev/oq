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
    Oq.Org.parse_string ~path:"seq-typ-todo.org"
      "#+SEQ_TODO: NEXT WAIT | DONE\n#+TYP_TODO: IDEA BLOCKED | ARCHIVED\n* NEXT seq item\n* IDEA type item\n* ARCHIVED archived item\n"
  with
  | Error err ->
      failwithf "expected SEQ_TODO/TYP_TODO parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert
        (Poly.equal doc.todo_config.open_states [ "NEXT"; "WAIT"; "IDEA"; "BLOCKED" ]);
      assert (Poly.equal doc.todo_config.done_states [ "DONE"; "ARCHIVED" ]);
      assert (List.length doc.index.todos = 3)

let () =
  let bom =
    String.init 3 ~f:(fun index ->
        Char.of_int_exn [| 0xEF; 0xBB; 0xBF |].(index))
  in
  match
    Oq.Org.parse_string ~path:"bom-todo.org"
      (bom ^ "#+SEQ_TODO: NEXT | DONE\n* NEXT Task\n")
  with
  | Error err ->
      failwithf "expected UTF-8 BOM parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (Poly.equal doc.todo_config.open_states [ "NEXT" ]);
      assert (Poly.equal doc.todo_config.done_states [ "DONE" ]);
      assert (List.length doc.index.todos = 1)

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
  match
    Oq.Org.parse_string ~path:"properties-lowercase.org"
      "* Task\n:PROPERTIES:\n:owner: Alice\n:END:\n"
  with
  | Error err ->
      failwithf "expected lowercase property parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.properties = 1);
      assert
        (String.Caseless.equal
           (List.hd_exn doc.index.properties).key
           "OWNER")

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
    Oq.Org.parse_string ~path:"dynamic-block.org"
      "* Root\n#+BEGIN: clocktable :scope file\n:UNRELATED:\n#+END:\n** Child\n"
  with
  | Error err ->
      failwithf "expected dynamic block parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 2);
      assert (List.is_empty doc.index.drawers)

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
    Oq.Org.parse_string ~path:"src-no-language-switch.org"
      "* Snippets\n#+BEGIN_SRC -n :results output\n(message \"ok\")\n#+END_SRC\n"
  with
  | Error err ->
      failwithf "expected src no-language-with-switch parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.blocks with
      | [ block ] ->
          assert (Poly.equal block.kind Oq.Org.Src);
          assert (Option.is_none block.language)
      | _ ->
          failwith
            "expected one src block without language when only switches are \
             provided")

let () =
  match
    Oq.Org.parse_string ~path:"src-no-language-plus-switch.org"
      "* Snippets\n#+BEGIN_SRC +n :results output\n(message \"ok\")\n#+END_SRC\n"
  with
  | Error err ->
      failwithf
        "expected src no-language-with-plus-switch parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.blocks with
      | [ block ] ->
          assert (Poly.equal block.kind Oq.Org.Src);
          assert (Option.is_none block.language)
      | _ ->
          failwith
            "expected one src block without language when only plus-switches \
             are provided")

let () =
  match
    Oq.Org.parse_string ~path:"src-upper-lang.org"
      "* Demo\n#+BEGIN_SRC OCAML\nlet x = 1\n#+END_SRC\n"
  with
  | Error err ->
      failwithf "expected uppercase-language src parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.blocks with
      | [ block ] ->
          assert (Poly.equal block.kind Oq.Org.Src);
          assert (String.equal (Option.value block.language ~default:"") "OCAML")
      | _ -> failwith "expected one src block with uppercase language")

let () =
  match
    Oq.Org.parse_string ~path:"src-end-trailing.org"
      "* Demo\n#+BEGIN_SRC ocaml\nlet x = 1\n#+END_SRC trailing text\n"
  with
  | Error err ->
      failwithf "expected src end-trailing parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.blocks with
      | [ block ] ->
          assert (Poly.equal block.kind Oq.Org.Src);
          assert (String.equal (Option.value block.language ~default:"") "ocaml")
      | _ -> failwith "expected one src block with trailing end marker text")

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
  match
    Oq.Org.parse_string ~path:"planning-combined-lower.org"
      "* TODO Combined planning\nscheduled: <2026-02-18 Wed> deadline: <2026-02-20 Fri> closed: [2026-02-21 Sat]\n"
  with
  | Error err ->
      failwithf "expected lowercase combined planning parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> assert (List.length doc.index.planning = 3)

let () =
  match
    Oq.Org.parse_string ~path:"planning-in-text.org"
      "* Note\nThis sentence mentions SCHEDULED: <2026-02-18 Wed> but is plain text.\n"
  with
  | Error err ->
      failwithf "expected planning-in-text parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> assert (List.is_empty doc.index.planning)

let () =
  match
    Oq.Org.parse_string ~path:"comment-lines.org"
      "* Note\n# this is a comment with https://example.com\n# SCHEDULED: <2026-02-18 Wed>\n"
  with
  | Error err ->
      failwithf "expected comment-lines parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.is_empty doc.index.links);
      assert (List.is_empty doc.index.planning)

let () =
  match
    Oq.Org.parse_string ~path:"planning-diary-sexp.org"
      "* TODO Recurring\nSCHEDULED: <%%(diary-float t 5 2)>\n"
  with
  | Error err ->
      failwithf "expected diary-sexp planning parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.planning = 1);
      assert
        (String.equal
           (List.hd_exn doc.index.planning).raw_value
           "<%%(diary-float t 5 2)>")

let () =
  match
    Oq.Org.parse_string ~path:"fixed-width.org"
      "* Note\n: code:\n: another line\n"
  with
  | Error err ->
      failwithf "expected fixed-width parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.is_empty doc.index.drawers)

let () =
  match Oq.Org.parse_string ~path:"colon-token.org" "* Note\n:a:b:\ntext\n" with
  | Error err ->
      failwithf "expected colon-token parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.is_empty doc.index.drawers)

let () =
  match Oq.Org.parse_string ~path:"emoji-token.org" "* Note\n:+1:\ntext\n" with
  | Error err ->
      failwithf "expected emoji-token parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.is_empty doc.index.drawers)

let () =
  match Oq.Org.parse_string ~path:"smile-token.org" "* Note\n:smile:\ntext\n" with
  | Error err ->
      failwithf "expected smile-token parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.is_empty doc.index.drawers)

let () =
  match Oq.Org.parse_string ~path:"todo-token.org" "* Note\n:TODO:\ntext\n" with
  | Error err ->
      failwithf "expected todo-token parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.is_empty doc.index.drawers)

let () =
  match
    Oq.Org.parse_string ~path:"custom-drawer-opaque.org"
      "* Note\n:MYDRAWER:\n#+BEGIN_SRC\nliteral\n:END:\n"
  with
  | Error err ->
      failwithf "expected custom drawer-opaque parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.length doc.index.drawers = 1);
      assert
        (String.equal
           (List.hd_exn doc.index.drawers).name
           "MYDRAWER")

let () =
  match
    Oq.Org.parse_string ~path:"indented-colon.org"
      "* Note\n  :foo:\n  literal text\n"
  with
  | Error err ->
      failwithf "expected indented colon parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.is_empty doc.index.drawers)

let () =
  match
    Oq.Org.parse_string ~path:"drawer-indented-end.org"
      "* Note\n:PROPERTIES:\n:OWNER: Alice\n  :END:\n"
  with
  | Error err ->
      failwithf "expected drawer with indented end parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.length doc.index.drawers = 1);
      assert (List.length doc.index.properties = 1)

let () =
  match
    Oq.Org.parse_string ~path:"drawer-tabbed-end.org"
      "* Note\n:PROPERTIES:\n:OWNER: Alice\n\t:END:\n"
  with
  | Error err ->
      failwithf "expected drawer with tabbed end parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.headings = 1);
      assert (List.length doc.index.drawers = 1);
      assert (List.length doc.index.properties = 1)

let () =
  match
    Oq.Org.parse_string ~path:"links-tab.org"
      "* Note\nword\thttps://example.com/docs\n"
  with
  | Error err ->
      failwithf "expected tab-separated plain link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert
        (String.equal
           (List.hd_exn doc.index.links).target
           "https://example.com/docs")

let () =
  match
    Oq.Org.parse_string ~path:"links-uppercase-scheme.org"
      "* Note\nHTTPS://example.com/docs\n"
  with
  | Error err ->
      failwithf
        "expected uppercase-scheme plain link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert
        (String.equal
           (List.hd_exn doc.index.links).target
           "HTTPS://example.com/docs")

let () =
  match
    Oq.Org.parse_string ~path:"links-generic-scheme.org"
      "* Note\nftp://example.com/pub\n"
  with
  | Error err ->
      failwithf "expected generic-scheme plain link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert
        (String.equal
           (List.hd_exn doc.index.links).target
           "ftp://example.com/pub")

let () =
  match
    Oq.Org.parse_string ~path:"links-file-mailto.org"
      "* Note\nfile:notes.org\nmailto:user@example.com\n"
  with
  | Error err ->
      failwithf "expected file/mailto plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "file:notes.org"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "mailto:user@example.com"))

let () =
  match
    Oq.Org.parse_string ~path:"links-id-custom-id.org"
      "* Note\nid:task-123\ncustom-id:release-notes\n"
  with
  | Error err ->
      failwithf "expected id/custom-id plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "id:task-123"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "custom-id:release-notes"))

let () =
  match
    Oq.Org.parse_string ~path:"links-news-shell.org"
      "* Note\nnews:comp.emacs\nshell:echo hello\n"
  with
  | Error err ->
      failwithf "expected news/shell plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "news:comp.emacs"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "shell:echo"))

let () =
  match
    Oq.Org.parse_string ~path:"links-man-woman.org"
      "* Note\nman:printf\nwoman:printf\n"
  with
  | Error err ->
      failwithf "expected man/woman plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "man:printf"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "woman:printf"))

let () =
  match Oq.Org.parse_string ~path:"links-calc.org" "* Note\ncalc:2+2\n" with
  | Error err ->
      failwithf "expected calc plain link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert (String.equal (List.hd_exn doc.index.links).target "calc:2+2")

let () =
  match
    Oq.Org.parse_string ~path:"links-mobile-geo.org"
      "* Note\ntel:+123456789\nsms:+123456789\ngeo:37.786971,-122.399677\n"
  with
  | Error err ->
      failwithf "expected tel/sms/geo plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 3);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "tel:+123456789"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "sms:+123456789"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "geo:37.786971,-122.399677"))

let () =
  match
    Oq.Org.parse_string ~path:"links-doi.org"
      "* Note\ndoi:10.1000/182\n"
  with
  | Error err ->
      failwithf "expected doi plain link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert
        (String.equal
           (List.hd_exn doc.index.links).target
           "doi:10.1000/182")

let () =
  match
    Oq.Org.parse_string ~path:"links-attachment.org"
      "* Note\nattachment:report.pdf\n"
  with
  | Error err ->
      failwithf "expected attachment plain link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert
        (String.equal
           (List.hd_exn doc.index.links).target
           "attachment:report.pdf")

let () =
  match
    Oq.Org.parse_string ~path:"links-coderef.org"
      "* Note\ncoderef:label\n"
  with
  | Error err ->
      failwithf "expected coderef plain link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert
        (String.equal
           (List.hd_exn doc.index.links).target
           "coderef:label")

let () =
  match
    Oq.Org.parse_string ~path:"links-file-plus.org"
      "* Note\nfile+sys:/tmp/report.txt\nfile+emacs:/tmp/report.txt\n"
  with
  | Error err ->
      failwithf "expected file+ plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "file+sys:/tmp/report.txt"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "file+emacs:/tmp/report.txt"))

let () =
  match
    Oq.Org.parse_string ~path:"links-file-no-scheme.org"
      "* Note\n./notes.org\n../docs/spec.org\n/ssh:me@host:/tmp/notes.org\n"
  with
  | Error err ->
      failwithf "expected plain file path links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 3);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "./notes.org"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "../docs/spec.org"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "/ssh:me@host:/tmp/notes.org"))

let () =
  match
    Oq.Org.parse_string ~path:"links-file-tilde.org"
      "* Note\n~/notes.org\n~alice/docs/spec.org\n"
  with
  | Error err ->
      failwithf "expected tilde file path links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "~/notes.org"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "~alice/docs/spec.org"))

let () =
  match
    Oq.Org.parse_string ~path:"links-file-windows.org"
      "* Note\nC:/work/notes.org\nD:\\docs\\spec.org\n"
  with
  | Error err ->
      failwithf "expected windows file path links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "C:/work/notes.org"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "D:\\docs\\spec.org"))

let () =
  match
    Oq.Org.parse_string ~path:"links-more-schemes.org"
      "* Note\nirc:#emacs\ngnus:group\ndocview:/tmp/a.pdf::5\nrmail:Inbox\nbbdb:John_Doe\n"
  with
  | Error err ->
      failwithf "expected additional plain schemes parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 5);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "irc:#emacs"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "gnus:group"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "docview:/tmp/a.pdf::5"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "rmail:Inbox"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "bbdb:John_Doe"))

let () =
  match
    Oq.Org.parse_string ~path:"links-mail-schemes.org"
      "* Note\nmhe:inbox\nwl:folder\nvm:inbox\n"
  with
  | Error err ->
      failwithf "expected additional mail plain schemes parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 3);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "mhe:inbox"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "wl:folder"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "vm:inbox"))

let () =
  match
    Oq.Org.parse_string ~path:"links-vm-imap.org"
      "* Note\nvm-imap:work:Inbox\nvm-imap:work:Inbox#123\n"
  with
  | Error err ->
      failwithf "expected vm-imap plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "vm-imap:work:Inbox"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "vm-imap:work:Inbox#123"))

let () =
  match
    Oq.Org.parse_string ~path:"links-custom-abbrev.org"
      "#+LINK: gh https://github.com/%s\n* Note\ngh:ocaml/dune\n"
  with
  | Error err ->
      failwithf "expected LINK abbrev plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert
        (String.equal (List.hd_exn doc.index.links).target "gh:ocaml/dune")

let () =
  match
    Oq.Org.parse_string ~path:"links-custom-abbrev-late.org"
      "* Note\ngh:ocaml/dune\n#+LINK: gh https://github.com/%s\n"
  with
  | Error err ->
      failwithf "expected late LINK abbrev plain links parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 1);
      assert
        (String.equal (List.hd_exn doc.index.links).target "gh:ocaml/dune")

let () =
  match
    Oq.Org.parse_string ~path:"links-custom-abbrev-in-src.org"
      "* Note\n#+BEGIN_SRC text\n#+LINK: gh https://github.com/%s\n#+END_SRC\ngh:ocaml/dune\n"
  with
  | Error err ->
      failwithf "expected LINK abbrev in src to stay inactive, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> assert (List.length doc.index.links = 0)

let () =
  match
    Oq.Org.parse_string ~path:"links-custom-abbrev-in-drawer.org"
      "* Note\n:MYDRAWER:\n#+LINK: gh https://github.com/%s\n:END:\ngh:ocaml/dune\n"
  with
  | Error err ->
      failwithf "expected LINK abbrev in custom drawer to stay inactive, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> assert (List.length doc.index.links = 0)

let () =
  match
    Oq.Org.parse_string ~path:"links-bracket.org"
      "* Links\n[[https://example.com/docs][Example Docs]]\n"
  with
  | Error err ->
      failwithf "expected bracket link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.links with
      | [ link ] ->
          assert (Poly.equal link.kind Oq.Org.Bracket);
          assert (String.equal link.target "https://example.com/docs")
      | _ -> failwith "expected a single bracket link without plain duplicate")

let () =
  match
    Oq.Org.parse_string ~path:"links-parentheses.org"
      "* Links\nhttps://en.wikipedia.org/wiki/Function_(mathematics)\n"
  with
  | Error err ->
      failwithf "expected parenthesized URL parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc -> (
      match doc.index.links with
      | [ link ] ->
          assert (Poly.equal link.kind Oq.Org.Plain);
          assert
            (String.equal
               link.target
               "https://en.wikipedia.org/wiki/Function_(mathematics)")
      | _ -> failwith "expected one plain link with balanced parentheses")

let () =
  match
    Oq.Org.parse_string ~path:"links-trailing-punctuation.org"
      "* Links\nVisit https://example.com/docs! and then https://example.com/help?\n"
  with
  | Error err ->
      failwithf "expected trailing punctuation plain link parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "https://example.com/docs"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "https://example.com/help"))

let () =
  match
    Oq.Org.parse_string ~path:"links-angle-spaces.org"
      "* Links\n<https://example.com/path with spaces>\n<bbdb:R.* Stallman>\n"
  with
  | Error err ->
      failwithf "expected angle links with spaces parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 2);
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "https://example.com/path with spaces"));
      assert
        (List.exists doc.index.links ~f:(fun link ->
             String.equal link.target "bbdb:R.* Stallman"))

let () =
  match
    Oq.Org.parse_string ~path:"links-duplicate-occurrences.org"
      "* Links\nhttps://example.com https://example.com\n<https://example.org> https://example.org\n"
  with
  | Error err ->
      failwithf "expected duplicate link occurrences parse success, got %s (%s)"
        (Oq.Diagnostic.parse_reason_to_string err.reason)
        err.detail ()
  | Ok doc ->
      assert (List.length doc.index.links = 4)

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
