open Core

let assert_contains text needle =
  if String.is_substring text ~substring:needle then ()
  else failwithf "expected output to contain %S, got:\n%s" needle text ()

let assert_exit outcome expected =
  assert
    (Int.equal
       (Oq.Exit_code.to_int outcome.Oq.Cli.exit_code)
       (Oq.Exit_code.to_int expected))

let require_stdout outcome =
  match outcome.Oq.Cli.stdout with
  | Some stdout -> stdout
  | None -> failwith "expected stdout output"

let create_temp_dir () =
  let base = Option.value (Sys.getenv "TMPDIR") ~default:"/tmp" in
  let rec loop attempt =
    let path =
      Filename.concat base
        (sprintf "oq-benchmark-check-%d-%d" (Caml_unix.getpid ()) attempt)
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

let run_directory_query root query =
  let request : Oq.Cli.request =
    { input_path = root; query = Some query; strict = false; now = None; tz = None }
  in
  Oq.Cli.execute request

let extract_counter stdout key =
  let prefix = key ^ "=" in
  match String.split_lines stdout |> List.find ~f:(String.is_prefix ~prefix) with
  | Some line ->
      Int.of_string
        (String.drop_prefix line (String.length key + String.length "="))
  | None -> failwithf "missing counter %S in stdout:\n%s" key stdout ()

let measure_seconds f =
  let start = Time_float.now () in
  let result = f () in
  let stop = Time_float.now () in
  (result, Time_float.Span.to_sec (Time_float.diff stop start))

let count_result_groups stdout =
  String.split_lines stdout
  |> List.filter ~f:(fun line ->
         String.is_suffix line ~suffix:":"
         && not (String.is_substring line ~substring:"="))
  |> List.length

let () =
  with_temp_dir (fun root ->
      for index = 0 to 499 do
        let marker_line =
          if index % 10 = 0 then "marker alpha beta for benchmark gate"
          else "noise body line"
        in
        ignore
          (write_file root
             (sprintf "corpus/%03d.org" index)
             (sprintf "* Note %03d\n%s\n" index marker_line))
      done;
      let query = ".search('marker alpha beta') | map(.title)" in
      let first, first_seconds =
        measure_seconds (fun () -> run_directory_query root query)
      in
      let second, second_seconds =
        measure_seconds (fun () -> run_directory_query root query)
      in
      assert_exit first Oq.Exit_code.Success;
      assert_exit second Oq.Exit_code.Success;
      let first_stdout = require_stdout first in
      let second_stdout = require_stdout second in
      if not (String.equal first_stdout second_stdout) then
        failwith "benchmark gate query output changed between reruns";
      assert (extract_counter first_stdout "candidate_org" = 500);
      assert (extract_counter first_stdout "parsed_ok" = 500);
      assert (extract_counter first_stdout "parse_failed" = 0);
      assert (count_result_groups first_stdout = 50);
      assert_contains first_stdout "corpus/000.org:";
      assert_contains first_stdout "corpus/490.org:";
      if Float.(second_seconds > 20.0) then
        failwithf
          "benchmark gate exceeded threshold: second run %.3fs > 20.0s"
          second_seconds ();
      Stdio.printf "benchmark_search_first_run_sec=%.3f\n" first_seconds;
      Stdio.printf "benchmark_search_second_run_sec=%.3f\n" second_seconds)
