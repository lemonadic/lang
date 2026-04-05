(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

(** Evaluate an expression to a value *)
val eval : Value.env -> Ast.expr -> Value.value

(** Apply a function value to an argument *)
val apply : Value.value -> Value.value -> Value.value

(** Apply a closure to an argument *)
val apply_closure : Value.closure -> Value.value -> Value.value

(** Convert a value to a human-readable string (for error messages) *)
val show_value : int -> Value.value -> string

(** Check if two values are convertible (definitionally equal) *)
val conv : int -> Value.value -> Value.value -> bool
