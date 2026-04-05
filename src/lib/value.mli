(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

type value =
  | VType
  | VPi of string * value * closure
  | VLam of string * closure
  | VIntType
  | VStringType
  | VBoolType
  | VUnitType
  | VIntLit of int
  | VStringLit of string
  | VNeutral of value * neutral
  | VDataType of string * value list
  | VConstructor of string * value list
  | VRecordType of (string * value) list
  | VRecord of (string * value) list

and neutral =
  | NVar of int
  | NApp of neutral * value
  | NAccess of neutral * string

and closure = Clos of env * string * Ast.expr

and env = (string * value) list
