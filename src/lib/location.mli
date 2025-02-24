(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Sofia Rodrigues
*)

(* A point in the source code defined using line and column. *)
type point = {
  line: int;
  column: int
}
[@@deriving show]

(* A position represents a location in the source code. *)
type position = {
  start_pos: point;
  end_pos: point;
}
[@@deriving show]

(* A position represents a location in a source code. *)
type range = {
  file: string;
  position: position
}
[@@deriving show]

val expand_positions : position list -> int list

val location : Lexing.position * Lexing.position -> position