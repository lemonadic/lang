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

let digit = [%sedlex.regexp? '0'..'9']

let lower = [%sedlex.regexp? 'a'..'z']

let upper = [%sedlex.regexp? 'A'..'Z']

let hex = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'Z']

let alpha = [%sedlex.regexp? lower | upper]

let wildcard = [%sedlex.regexp? '_', Star (alpha | digit | '_')]

let identifier = [%sedlex.regexp? (alpha | '_'), Star (alpha | digit | '_')]

let integer = [%sedlex.regexp? Plus digit]

let escape_sequence = [%sedlex.regexp? '\\', ('"' | '\\' | 'n' | 't' | 'r' | ('x', Rep (hex, 2)) | Rep (digit, 1 .. 3))]

let string_content = [%sedlex.regexp? Star (Compl ('"' | '\\') | escape_sequence)]

let string_literal = [%sedlex.regexp? '"', string_content, '"' ]

let hex_digit_value c =
  if '0' <= c && c <= '9' then Some (Char.code c - Char.code '0')
  else if 'a' <= c && c <= 'f' then Some (10 + Char.code c - Char.code 'a')
  else if 'A' <= c && c <= 'F' then Some (10 + Char.code c - Char.code 'A')
  else None

let parse_hex str i buffer =
  if i + 3 < String.length str then
    match (hex_digit_value str.[i + 2], hex_digit_value str.[i + 3]) with
    | Some high, Some low -> Buffer.add_char buffer (Char.chr ((high lsl 4) lor low)); i + 4
    | _ -> i + 2
  else i + 2

let parse_octal str i buffer =
  let rec read_octal j value count =
    if count = 3 || j >= String.length str || str.[j] < '0' || str.[j] > '7'
      then (value, j)
    else
      let digit = Char.code str.[j] - Char.code '0' in
      read_octal (j + 1) ((value lsl 3) lor digit) (count + 1)
  in
  let value, j = read_octal (i + 1) 0 0 in
  if j > i + 1 then (Buffer.add_char buffer (Char.chr value); j) else i + 2

let unescape_char buffer str i =
  match str.[i] with
  | '\\' when i + 1 < String.length str -> (
      match str.[i + 1] with
      | '"' -> Buffer.add_char buffer '"'; i + 2
      | '\\' -> Buffer.add_char buffer '\\'; i + 2
      | 'n' -> Buffer.add_char buffer '\n'; i + 2
      | 't' -> Buffer.add_char buffer '\t'; i + 2
      | 'r' -> Buffer.add_char buffer '\r'; i + 2
      | 'x' when i + 3 < String.length str -> parse_hex str i buffer
      | '0' -> parse_octal str (i + 1) buffer
      | _ -> Buffer.add_char buffer '\\'; Buffer.add_char buffer str.[i + 1]; i + 2
    )
  | c -> Buffer.add_char buffer c; i + 1

let remove_first_last s =
  let len = String.length s in
  if len <= 2 then ""
  else String.sub s 1 (len - 2)

let unescape str =
  let str = remove_first_last str in
  let buffer = Buffer.create (String.length str) in
  let rec aux i = if i >= String.length str then () else aux (unescape_char buffer str i) in
  aux 0;
  Buffer.contents buffer

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
  | string_literal -> STRING (unescape (lexeme buf))
  | wildcard -> WILDCARD
  | identifier -> IDENT (lexeme buf)
  | integer -> INT (int_of_string (lexeme buf))
  | _ -> raise (Invalid_token (lexeme buf))

(* `provider` is a lexer interface function that provides the next token from the input buffer. *)
let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  token, start, stop

(* `from_string` converts an input string into a sequence of tokens using the provided lexer. *)
let from_string f string =
  provider (from_string string)
  |> MenhirLib.Convert.Simplified.traditional2revised f
