(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Sofia Rodrigues
*)
open Grammar

exception Invalid_token of string

(* `provider` is a lexer interface function that provides the next token from the input buffer. *)
val provider :
  Sedlexing.lexbuf -> unit -> token * Lexing.position * Lexing.position

(* `from_string` converts an input string into a sequence of tokens using the provided lexer. *)
val from_string : (token, 'a) MenhirLib.Convert.traditional -> string -> 'a
