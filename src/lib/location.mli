(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Sofia Rodrigues
*)

(* A point in the source code defined using line and column. *)
type point = { line : int; column : int } [@@deriving show]

(* A position represents a location in the source code. *)
type position = { start_pos : point; end_pos : point } [@@deriving show]

val expand_positions : position list -> int list
(** Expands a list of ranges into a set of selected line numbers. *)

(* Creates an AST position using the Menhir internal position *)
val mk_ast_position : Lexing.position * Lexing.position -> position
