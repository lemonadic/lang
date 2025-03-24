open Alcotest
open Lang

(* Helper function to parse and type check a string *)
let type_check_string str =
  match Parser.parse_from_source "test" str with
  | Ok program -> 
      (match Type_checker.type_check_program program with
       | Ok _ -> true
       | Error _ -> false)
  | Error _ -> false

(* Test cases *)
let test_simple_let () =
  check bool "Simple let declaration should type check" true
    (type_check_string "let x : int = 5")

let test_function_declaration () =
  check bool "Function declaration should type check" true
    (type_check_string "let add (x: int, y: int) : int = x")

let test_type_mismatch () =
  check bool "Type mismatch should fail" false
    (type_check_string "let x : string = 5")

let test_record_type () =
  check bool "Record type should type check" true
    (type_check_string "type User = { name: string, age: int }")

let test_variant_type () =
  check bool "Variant type should type check" true
    (type_check_string "type Option = | Some(int) | None")

(* Test suite *)
let () =
  Alcotest.run "Type Checker Tests" [
    "type checking", [
      test_case "Simple let" `Quick test_simple_let;
      test_case "Function declaration" `Quick test_function_declaration;
      test_case "Type mismatch" `Quick test_type_mismatch;
      test_case "Record type" `Quick test_record_type;
      test_case "Variant type" `Quick test_variant_type;
    ]
  ]
