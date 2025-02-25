(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Sofia Rodrigues
*)

module IntSet = Set.Make(struct type t = int let compare = compare end)

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

(** Expands a list of ranges into a set of selected line numbers. *)
let expand_positions ranges =
  let add_if_valid acc line = if line > 0 then IntSet.add line acc else acc in

  let determine_lines l1 l2 =
    match l2 - l1 with
    | 0 -> [l1 - 1; l1; l1 + 1]
    | _ -> [l1 - 1; l1; l2; l2 + 1] in

  let process_range acc {start_pos; end_pos} =
    determine_lines start_pos.line end_pos.line
    |> List.fold_left add_if_valid acc in

  ranges
  |> List.fold_left process_range IntSet.empty
  |> IntSet.to_list

(* Creates an AST position using the Menhir internal position *)
let mk_ast_position (startpos, endpos) = {
  start_pos = {
    column = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol;
    line = startpos.Lexing.pos_lnum
  };
  end_pos = {
    column = endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol;
    line = endpos.Lexing.pos_lnum
  }
}
