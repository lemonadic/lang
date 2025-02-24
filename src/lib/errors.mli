(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Sofia Rodrigues
*)
open Location

type compiler_error = {
  id : int;
  message : string;
  location : range;
  hints : (string * position) list;
  additional_info : (string * string) list;
}

(* Pretty prints an error message. *)
val print_compiler_error : Format.formatter -> compiler_error -> string -> unit