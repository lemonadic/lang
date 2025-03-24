(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

(** Represents a type expression in the language *)
type type_expr =
  | TypeVar of string                     (* Type variables like 'a *)
  | TypeConst of string                   (* Type constants like int, string *)
  | TypeApp of string * type_expr list    (* Type applications like List(int) *)
  | TypeArrow of type_expr * type_expr    (* Function types like int -> string *)
  | TypeRecord of (string * type_expr) list (* Record types like {name: string, age: int} *)
[@@deriving show]

(** Creates a simple type constant *)
let mk_const name = TypeConst name

(** Creates a function type *)
let mk_arrow param_type return_type = TypeArrow (param_type, return_type)

(** Creates a type application *)
let mk_app name args = TypeApp (name, args)

(** Creates a record type *)
let mk_record fields = TypeRecord fields

(** Creates a type variable *)
let mk_var name = TypeVar name

(** Primitive types *)
let type_int = mk_const "int"
let type_string = mk_const "string"
let type_bool = mk_const "bool"
let type_unit = mk_const "unit"