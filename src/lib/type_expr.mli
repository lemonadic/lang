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
val mk_const : string -> type_expr

(** Creates a function type *)
val mk_arrow : type_expr -> type_expr -> type_expr

(** Creates a type application *)
val mk_app : string -> type_expr list -> type_expr

(** Creates a record type *)
val mk_record : (string * type_expr) list -> type_expr

(** Creates a type variable *)
val mk_var : string -> type_expr

(** Primitive types *)
val type_int : type_expr
val type_string : type_expr
val type_bool : type_expr
val type_unit : type_expr
