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
  match Oq.Org.parse_string ~path:"unsupported.org" "#+BEGIN_CENTER\ntext\n#+END_CENTER\n" with
  | Error { reason = Oq.Diagnostic.Unsupported_construct; line = Some 1; _ } -> ()
  | Error err ->
      failwithf "expected unsupported_construct, got %s" 
        (Oq.Diagnostic.parse_reason_to_string err.reason) ()
  | Ok _ -> failwith "expected parse failure for unsupported construct"

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
