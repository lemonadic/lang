(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

open Type_expr

(** Type environment maps identifiers to their types *)
type t

(** Creates a new empty environment *)
val create : unit -> t

(** Creates a copy of an environment *)
val copy : t -> t

(** Adds a binding to the environment *)
val add : t -> string -> type_expr -> unit

(** Looks up a binding in the environment *)
val lookup : t -> string -> type_expr option

(** Creates an initial environment with primitive types *)
val initial_env : unit -> t
