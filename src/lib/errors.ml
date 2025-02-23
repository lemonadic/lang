(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Sofia Rodrigues
*)
open Location

type compiler_error = {
  id : int;
  message : string;
  location : position option;
  hints : string list;
  additional_info : (string * string) list;
}
