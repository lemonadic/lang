(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

type compiler_options = {
  input_files : string list;
  output : string;
  backend : string;
  root_dir : string;
  from_stdin : bool;
  json_output : bool;
  module_tree_json : bool;
  threads : int;
  dependencies : bool;
  features : bool;
}

val process : compiler_options -> [ `Ok of unit | `Error of string ]
(** Processes the given compiler options and performs the appropriate actions.

    - If [features] is set to [true], it prints the supported features.
    - If [from_stdin] is [true], it reads input from standard input and parses
      it.
    - If no input files are provided, it returns an error.
    - Otherwise, it proceeds to compile the provided input files.

    @param options The compiler options to process.
    @return [`Ok ()] if processing succeeds, [`Error of string] otherwise. *)
