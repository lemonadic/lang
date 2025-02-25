open Alcotest

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let golden_test process file () =
  let expect_file = Filename.chop_extension file ^ ".expect" in
  let actual_output = process file in
  if Sys.file_exists expect_file then
    let expected_output = read_file expect_file in
    Alcotest.(check string) ("Golden test for " ^ file) expected_output actual_output
  else
    write_file expect_file actual_output

let get_test_files dir =
  Array.to_list (Sys.readdir dir)
  |> List.filter (fun f -> Filename.check_suffix f ".lng")
  |> List.map (fun f -> Filename.concat dir f)

(* -------- *)

let parse source =
  match Lang.Parser.parse_from_source "test" source with
  | Ok(x) -> String.concat "ok: " [Lang.Ast.show_program x]
  | Error x -> String.concat "error: " [x.message]

let () =
  let test_dir = "../../../suite/parser" in
  let test_cases = get_test_files test_dir |> List.map (fun file -> test_case file `Quick (golden_test parse file)) in
  Alcotest.run "Parser Golden Tests" [ "Parser output", test_cases ]