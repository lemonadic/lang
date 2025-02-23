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
  file: int option;
  start: point;
  end': point;
}
[@@deriving show]