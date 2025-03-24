(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)
open Lang

type compiler_options = {
  input_files : string list;
  output : string;
  backend : string;
  root_dir : string;
  from_stdin : bool;
  json_output : bool;
  module_tree_json : bool;
  threads : int;
  dependencies : bool;
  features : bool;
}

let print_features ()  =
  print_endline "Supported features:";
  print_endline "- Compilation from files or stdin";
  print_endline "- Multiple backends (e.g., LLVM)";
  print_endline "- JSON output options";
  `Ok ()

let read_until_eof () =
  let buffer = Buffer.create 2048 in
  let rec read_loop() =
    let line = try Some(read_line()) with End_of_file -> None in
    match line with
    | None -> Buffer.contents buffer
    | Some(line) -> (
      Buffer.add_string buffer (line ^ "\n");
      read_loop()
    )
  in
  read_loop()

let parse_from_stdin json_output =
  let source_code = read_until_eof () in
  let parsed = Parser.parse_from_source "stdin" source_code in

  match parsed with
  | Ok ast ->
    if json_output then
      failwith "TODO implement json output"
    else
      print_endline (Ast.show_program ast);
      `Ok()
  | Error error ->
    Errors.print_compiler_error Format.err_formatter error source_code;
    exit 1

let compile options () =
  Printf.printf "Compiling files: %s\n" (String.concat ", " options.input_files);
  
  (* Parse the input files *)
  let parse_file filename =
    let channel = open_in filename in
    let result = Parser.parse_from_channel filename channel in
    close_in channel;
    result
  in
  
  (* Parse all input files and combine into a single program *)
  let parsed_programs = 
    List.map parse_file options.input_files
    |> List.fold_left 
         (fun acc result -> 
           match acc, result with
           | Ok acc_prog, Ok file_prog -> Ok (acc_prog @ file_prog)
           | Error e, _ -> Error e
           | _, Error e -> Error e)
         (Ok [])
  in
  
  match parsed_programs with
  | Ok program ->
      (* Type check the program *)
      (match Type_checker.type_check_program program with
       | Ok _ -> 
           Printf.printf "Type checking successful!\n";
           (* Continue with code generation *)
           `Ok ()
       | Error error ->
           Errors.print_compiler_error Format.err_formatter error "";
           exit 1)
  | Error error ->
      Errors.print_compiler_error Format.err_formatter error "";
      exit 1

let process options =
  match options with
  | { features = true; _ } -> print_features ()
  | { from_stdin = true; json_output; _ } -> parse_from_stdin json_output
  | { input_files = []; _ } ->
      prerr_endline "Error: No input files provided.";
      `Error "No input files provided."
  | options -> compile options ()