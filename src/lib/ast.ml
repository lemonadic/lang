(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Davi William, Sofia Rodrigues
*)
open Location

(* An identifier is a string with a position in the source code, usually representing a name. *)
type identifier = string * position
[@@deriving show]

(* The kind of a literal expression. *)
type literal_kind =
  | LitInt of int
  | LitString of string
[@@deriving show]

(* A literal expression is a literal with its position in the source code. *)
type literal = literal_kind * position
[@@deriving show]

(* A pattern is used in match expressions to match against values and destructure them. *)
type pattern_kind =
  | PVar of identifier
  | PLit of literal
  | PWildcard
  | PConstructor of identifier * pattern list
[@@deriving show]

(* A pattern consists of its kind and its position in the source code. *)
and pattern = pattern_kind * position
[@@deriving show]

(* The kind of statement in the language. *)
type sttm_kind =
  | SttmLet of pattern * expr
  | SttmExpr of expr
[@@deriving show]

(* A statement consists of its kind and the associated expression. *)
and sttm = sttm_kind * position
[@@deriving show]

(* The kind of expression. *)
and expr_kind =
  | ExprVar of identifier
  | ExprLit of literal
  | ExprCall of expr * expr list
  | ExprMatch of expr * (pattern * expr) list
  | ExprBlock of sttm list
[@@deriving show]

(* An expression consists of its kind and its position in the source code. *)
and expr = expr_kind * position
[@@deriving show]

(* The kind of type definition. *)
type type_value =
  | TyInductive of (identifier * expr list) list
  | TyStruct of (identifier * expr) list
[@@deriving show]

(* A binder is a pair of an identifier and an optional expression. *)
type binder_kind = identifier * expr option
[@@deriving show]

(* A binder consists of its kind and its position in the source code. *)
type binder = binder_kind * position
[@@deriving show]

(* A type declaration defines a new type with its name, binders, return type, and value. *)
type type_decl = {
  name: identifier;
  binders: binder list;
  return_type: expr option;
  value: type_value
}
[@@deriving show]

(* A function declaration defines a function with its name, parameters, return type, and body. *)
type let_decl = {
  name: identifier;
  params: (identifier * expr) list;
  return_type: expr;
  body: expr;
}
[@@deriving show]

(* A declaration can either be a type definition or a function declaration. *)
type declaration_value =
  | TypeDef of type_decl
  | LetDecl of let_decl
[@@deriving show]

(* The visibility of a declaration, which can be either public or private. *)
type declaration_visibility =
  | Public
  | Private
[@@deriving show]

(* A declaration consists of a value and its visibility. *)
type declaration = {
  value: declaration_value;
  visibility: declaration_visibility;
}
[@@deriving show]

(* A program is a list of declarations. *)
type program = declaration list
[@@deriving show]
