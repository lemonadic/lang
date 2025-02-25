(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Sofia Rodrigues
*)
open Errors

let handle_error file buf id message hints =
  {
    id;
    message;
    file;
    location = Location.mk_ast_position (Sedlexing.lexing_positions buf);
    hints;
    additional_info = [];
  }

let parse file (buf : Sedlexing.lexbuf) =
  try
    let ast = MenhirLib.Convert.Simplified.traditional2revised Grammar.program (Lexer.provider buf) in
    Ok ast
  with
    | Lexer.Invalid_token msg -> Error (handle_error file buf 1 ("Lexical error: " ^ msg) [])
    | Grammar.Error -> Error (handle_error file buf 2 "Parse error" [])

(* Parses a source string from the given string. *)
let parse_from_source file source =
  parse file (Sedlexing.Utf8.from_string source)

(* Parses input from a channel. *)
let parse_from_channel file channel =
  parse file (Sedlexing.Utf8.from_channel channel)
