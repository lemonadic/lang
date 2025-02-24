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

(* ANSI escape codes for color formatting *)
let bold = "\027[1m"
let reset = "\027[0m"
let red = "\027[31m"
let white = "\027[37m"

(* Pretty prints an error message with a more beautiful layout. *)
let print_compiler_error fmt error source =
  let pp_range fmt { start_pos; end_pos } = Format.fprintf fmt "%d:%d~%d:%d" start_pos.line start_pos.column end_pos.line end_pos.column in

  Format.fprintf fmt "\n %s%sERROR E%04d%s: %s%s\n" bold red error.id white error.message reset;

  Format.fprintf fmt "  --> %s:" error.location.file;
  pp_range fmt error.location.position;
  Format.fprintf fmt "\n\n";

  (* Split source code into lines and highlight relevant lines *)
  let relevant_lines = expand_positions (error.location.position :: (List.map (fun (_, pos) -> pos) error.hints)) in
  let sorted_lines = List.sort_uniq compare relevant_lines in
  let source_lines = String.split_on_char '\n' source |> Array.of_list in

  (* Print the relevant lines of code with error highlighting *)
  List.iter (fun line_num ->
    if line_num > 0 && line_num <= Array.length source_lines then
      let line = source_lines.(line_num - 1) in
      Format.fprintf fmt "%4d | %s\n" line_num line;

      if line_num = error.location.position.start_pos.line then
        let start_col = error.location.position.start_pos.column in
        let end_col = error.location.position.end_pos.column in
        let error_length = end_col - start_col in
        let caret_line = String.make (start_col) ' ' ^ String.make error_length '^' in
        Format.fprintf fmt "     | %s%s%s%s\n" red bold caret_line reset
  ) sorted_lines;

  Format.fprintf fmt "\n";