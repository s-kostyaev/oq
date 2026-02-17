open Core

let parse_ok query =
  match Oq.Query.parse query with
  | Ok ast -> ast
  | Error { message; position } ->
      failwithf "expected parse success for %S, got error at %d: %s" query
        position message ()

let parse_error query =
  match Oq.Query.parse query with
  | Ok _ -> failwithf "expected parse error for %S" query ()
  | Error err -> err

let assert_error_contains query needle =
  let { Oq.Query.message; _ } = parse_error query in
  assert (String.is_substring message ~substring:needle)

let rec expr_shape = function
  | Oq.Query.Ast.Path fields -> "path(" ^ String.concat ~sep:"." fields ^ ")"
  | Oq.Query.Ast.Literal (String value) -> "str(" ^ value ^ ")"
  | Oq.Query.Ast.Literal (Int value) -> "int(" ^ Int.to_string value ^ ")"
  | Oq.Query.Ast.Literal (Float value) ->
      "float(" ^ Float.to_string value ^ ")"
  | Oq.Query.Ast.Literal (Bool value) ->
      "bool(" ^ Bool.to_string value ^ ")"
  | Oq.Query.Ast.Call { name; args } ->
      "call(" ^ name ^ "," ^ String.concat ~sep:"," (List.map args ~f:expr_shape)
      ^ ")"
  | Oq.Query.Ast.Parenthesized predicate -> "group(" ^ predicate_shape predicate ^ ")"

and predicate_shape = function
  | Oq.Query.Ast.Predicate_value expr -> "value(" ^ expr_shape expr ^ ")"
  | Oq.Query.Ast.Predicate_compare { left; op; right } ->
      let op_text =
        match op with
        | Eq -> "=="
        | Ne -> "!="
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
      in
      "cmp(" ^ expr_shape left ^ op_text ^ expr_shape right ^ ")"
  | Oq.Query.Ast.Predicate_and (left, right) ->
      "and(" ^ predicate_shape left ^ "," ^ predicate_shape right ^ ")"
  | Oq.Query.Ast.Predicate_or (left, right) ->
      "or(" ^ predicate_shape left ^ "," ^ predicate_shape right ^ ")"

let () =
  let parsed =
    parse_ok
      ".todos | filter(.state == 'TODO' or .priority == 1 and .title == \
       'Release')"
  in
  match List.last parsed with
  | Some { atom_stage = Function (Filter predicate); _ } ->
      assert
        (String.equal
           (predicate_shape predicate)
           "or(cmp(path(state)==str(TODO)),and(cmp(path(priority)==int(1)),cmp(path(title)==str(Release))))")
  | _ -> failwith "expected trailing filter stage"

let () =
  let parsed =
    parse_ok
      ".todos | filter((.state == 'TODO' or .priority == 1) and .title == \
       'Release')"
  in
  match List.last parsed with
  | Some { atom_stage = Function (Filter predicate); _ } ->
      assert
        (String.equal
           (predicate_shape predicate)
           "and(value(group(or(cmp(path(state)==str(TODO)),cmp(path(priority)==int(1))))),cmp(path(title)==str(Release)))")
  | _ -> failwith "expected trailing filter stage"

let () =
  let parsed = parse_ok ".headings[0].title" in
  match parsed with
  | [
   {
     atom_stage = Selector { name = "headings"; _ };
     postfixes = [ Index 0; Field "title" ];
   };
  ] ->
      ()
  | _ -> failwith "expected .headings[0].title postfix binding"

let () =
  let parsed = parse_ok ".todos[1:3].state" in
  match parsed with
  | [
   {
     atom_stage = Selector { name = "todos"; _ };
     postfixes = [ Slice (Some 1, Some 3); Field "state" ];
   };
  ] ->
      ()
  | _ -> failwith "expected .todos[1:3].state postfix binding"

let () =
  let parsed = parse_ok ".headings | .length" in
  assert (List.length parsed = 2)

let () = assert_error_contains ".headings | [0]" "bare indexing stage is invalid"
let () = assert_error_contains ".headings | length" "use `.length`"
let () = assert_error_contains ".unknown_selector" "unknown selector"
let () = assert_error_contains ".tree('full')" ".tree does not take arguments"
let () = assert_error_contains ".headings('one')" "expected integer"
let () = assert_error_contains ".search('/foo/x')" "unsupported regex flags"
let () = assert_error_contains ".search('/foo')" "malformed regex pattern-string"
