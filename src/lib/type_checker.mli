(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

(** Type check an entire program (list of declarations).
    Returns [Ok ()] if the program is well-typed, or [Error] with a
    compiler error describing the first type error found. *)
val type_check_program :
  Ast.program -> (unit, Errors.compiler_error) result
