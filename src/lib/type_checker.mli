(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

open Ast
open Type_expr
open Type_env
open Errors

(** Result type for type checking operations *)
type 'a type_check_result = ('a, compiler_error) result

(** Type checks an expression in the given environment *)
val type_check_expr : 
  Type_env.t -> Ast.expr -> type_expr option -> type_expr type_check_result

(** Type checks a declaration in the given environment *)
val type_check_decl : 
  Type_env.t -> Ast.declaration -> Type_env.t type_check_result

(** Type checks a program (list of declarations) *)
val type_check_program : 
  Ast.program -> Type_env.t type_check_result