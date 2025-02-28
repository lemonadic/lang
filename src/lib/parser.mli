(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Sofia Rodrigues
*)
open Errors

(* Parses a source string from the given string. *)
val parse_from_channel : string -> in_channel -> (Ast.program, compiler_error) result

(* Parses input from a channel. *)
val parse_from_source : string -> string -> (Ast.program, compiler_error) result