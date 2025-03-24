(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

open Type_expr

(** Type environment maps identifiers to their types *)
type t = (string, type_expr) Hashtbl.t

(** Creates a new empty environment *)
let create () = Hashtbl.create 100

(** Creates a copy of an environment *)
let copy = Hashtbl.copy

(** Adds a binding to the environment *)
let add env name type_expr = Hashtbl.add env name type_expr

(** Looks up a binding in the environment *)
let lookup env name = 
  try Some (Hashtbl.find env name)
  with Not_found -> None

(** Creates an initial environment with primitive types *)
let initial_env () =
  let env = create () in
  add env "int" (TypeConst "type");
  add env "string" (TypeConst "type");
  add env "bool" (TypeConst "type");
  add env "unit" (TypeConst "type");
  env
