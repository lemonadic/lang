(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

(** Semantic values for Normalization by Evaluation (NbE).

    THE KEY INSIGHT: In a dependently typed language, there is no separation
    between "types" and "values." Both are represented by the same [value] type.

    [int -> string] is a value (of type [Type]), just like [42] is a value
    (of type [int]). This is why a separate [type_expr] AST was the wrong
    approach — types ARE expressions, and we evaluate them to values.

    We evaluate syntax ([Ast.expr]) into values ([value]) for two reasons:
    1. To compare types: two types are equal if they evaluate to the same value
    2. To implement substitution efficiently via closures *)

(** The semantic domain — what expressions evaluate to. *)
type value =
  | VType
      (** [Type] — the type of all types. Having [Type : Type] is logically
          inconsistent (Girard's paradox), but keeps things simple. Lean, Coq,
          and Agda use universe hierarchies to avoid this, but that's a later
          refinement. *)
  | VPi of string * value * closure
      (** [(x : A) -> B(x)] — the dependent function type. This is the Pi type
          from the AST's [ExprPi], evaluated into a value.

          [VPi("x", domain, closure)] where:
          - ["x"] is the parameter name
          - [domain] is [A] evaluated to a value
          - [closure] computes [B(x)] when given a value for [x]

          When [x] doesn't appear in [B], this is just [A -> B]. *)
  | VLam of string * closure
      (** [fn x => body] — a function value. *)
  | VIntType    (** The [int] type *)
  | VStringType (** The [string] type *)
  | VBoolType   (** The [bool] type *)
  | VUnitType   (** The [unit] type *)
  | VIntLit of int       (** An integer value *)
  | VStringLit of string (** A string value *)
  | VNeutral of value * neutral
      (** A computation that is "stuck" — it can't reduce because it depends
          on an unknown variable. Carries its type for conversion checking.

          Example: when checking the body of [fn x => x + 1], we don't know
          what [x] is. So [x] becomes [VNeutral(VIntType, NVar 0)] — a stuck
          variable of type [int]. And [x + 1] would be a stuck application. *)
  | VDataType of string * value list
      (** A named data type, possibly applied to type arguments.
          [VDataType("Option", [])] or [VDataType("Result", [VIntType; VStringType])] *)
  | VConstructor of string * value list
      (** A data constructor, possibly partially applied.
          [VConstructor("None", [])] or [VConstructor("Some", [VIntLit 42])] *)
  | VRecordType of (string * value) list
      (** A record type: [{ name : string, age : int }] *)
  | VRecord of (string * value) list
      (** A record value: [{ name = "Alice", age = 30 }] *)

(** Neutral values — computations stuck on an unknown variable.

    When type checking under a binder (e.g., the body of a function), we don't
    know what the argument will be. We introduce a "neutral variable" as a
    placeholder. Any computation with it (application, field access) also gets
    stuck. Neutral values track this chain of stuck computations so we can
    compare them structurally during conversion checking. *)
and neutral =
  | NVar of int
      (** A variable, identified by its de Bruijn level.
          Level 0 is the outermost binder, 1 is the next, etc.
          (Levels count "from the top," unlike indices which count "from the bottom.") *)
  | NApp of neutral * value
      (** Stuck function application: [f(arg)] where [f] is stuck. *)
  | NAccess of neutral * string
      (** Stuck field access: [record.field] where [record] is stuck. *)

(** A closure captures an environment and a body expression.

    This is the heart of NbE. Instead of doing syntactic substitution
    (find-and-replace variable names in the AST — slow and error-prone),
    we capture the current environment and defer evaluation.

    [Clos(env, "x", body)] is like a function: give it a value [v],
    and it evaluates [body] with [("x", v) :: env]. *)
and closure = Clos of env * string * Ast.expr

(** The evaluation environment: a list of (name, value) bindings.
    Newer bindings shadow older ones (looked up with [List.assoc_opt]). *)
and env = (string * value) list
