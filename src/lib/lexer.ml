(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Davi William, Sofia Rodrigues
*)
open Grammar
open Sedlexing.Utf8

exception Invalid_token of string

(* Regular expressions for tokens *)

let whitespace = [%sedlex.regexp? Plus (' ' | '\n' | '\t' | '\r')]
let digit = [%sedlex.regexp? '0' .. '9']
let lower = [%sedlex.regexp? 'a' .. 'z']
let upper = [%sedlex.regexp? 'A' .. 'Z']
let alpha = [%sedlex.regexp? lower | upper]
let wildcard = [%sedlex.regexp? '_', Star (alpha | digit | '_')]
let identifier = [%sedlex.regexp? (alpha | '_'), Star (alpha | digit | '_')]
let integer = [%sedlex.regexp? Plus digit]
let string_literal = [%sedlex.regexp? '"', Star (Compl '"'), '"']

(* Main tokenizer function *)
let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | "type" -> TYPE
  | "fn" -> FN
  | "pub" -> PUB
  | "let" -> LET
  | "match" -> MATCH
  | "=>" -> FATARROW
  | "->" -> ARROW
  | "=" -> EQUAL
  | "." -> DOT
  | "," -> COMMA
  | ":" -> COLON
  | "{" -> LBRACE
  | "}" -> RBRACE
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "|" -> BAR
  | eof -> EOF
  | string_literal -> STRING (lexeme buf)
  | wildcard -> WILDCARD
  | identifier -> IDENT (lexeme buf)
  | integer -> INT (int_of_string (lexeme buf))
  | _ -> raise (Invalid_token (lexeme buf))

(* `provider` is a lexer interface function that provides the next token from the input buffer. *)
let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

(* `from_string` converts an input string into a sequence of tokens using the provided lexer. *)
let from_string f string =
  provider (from_string string)
  |> MenhirLib.Convert.Simplified.traditional2revised f
