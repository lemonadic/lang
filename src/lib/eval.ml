(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

open Ast
open Value

(* ========================================================================
   EVALUATION — the "E" in NbE
   ========================================================================

   [eval env expr] takes syntax and produces semantics. We walk the AST and
   compute values. The key operations:

   - Variables: look up in the environment
   - Lambdas: capture the environment into a closure (don't evaluate the body yet)
   - Application: evaluate function and argument, then apply
   - Pi types: evaluate the domain, capture codomain as a closure

   This is essentially an interpreter, but we use it at compile time to
   normalize type expressions so we can compare them. *)

(** Evaluate an expression to a value *)
let rec eval (env : env) (expr : expr) : value =
  let (kind, _) = expr in
  match kind with
  | ExprVar (name, _) ->
    (match List.assoc_opt name env with
     | Some v -> v
     | None -> failwith ("eval: unbound variable: " ^ name))

  | ExprLit (LitInt n, _) -> VIntLit n
  | ExprLit (LitString s, _) -> VStringLit s

  (* Pi types: evaluate the domain, capture codomain in a closure *)
  | ExprPi (None, domain, codomain) ->
    let domain_val = eval env domain in
    VPi ("_", domain_val, Clos (env, "_", codomain))

  | ExprPi (Some (name, _), domain, codomain) ->
    let domain_val = eval env domain in
    VPi (name, domain_val, Clos (env, name, codomain))

  (* Lambdas: just capture the environment, don't evaluate the body *)
  | ExprLambda ((name, _), body) ->
    VLam (name, Clos (env, name, body))

  (* Application: evaluate function and args, apply one at a time (currying) *)
  | ExprCall (func, args) ->
    let func_val = eval env func in
    List.fold_left (fun f arg ->
      let arg_val = eval env arg in
      apply f arg_val
    ) func_val args

  | ExprBlock stmts ->
    eval_block env stmts

  | ExprAccess (record, (ExprVar (field, _), _)) ->
    let record_val = eval env record in
    eval_access record_val field

  | ExprAccess _ -> failwith "eval: invalid field access"

  | ExprMatch (scrutinee, cases) ->
    let scrut_val = eval env scrutinee in
    eval_match env scrut_val cases

(** Apply a function value to an argument.

    Three cases:
    - [VLam]: substitute the argument into the body via the closure
    - [VNeutral] with Pi type: result is stuck (neutral application)
    - [VConstructor]: accumulate the argument *)
and apply (func : value) (arg : value) : value =
  match func with
  | VLam (_, clos) ->
    apply_closure clos arg
  | VNeutral (VPi (_, _, clos), ne) ->
    let result_ty = apply_closure clos arg in
    VNeutral (result_ty, NApp (ne, arg))
  | VConstructor (name, args) ->
    VConstructor (name, args @ [arg])
  | _ -> failwith "eval: cannot apply non-function"

(** Apply a closure: extend the captured environment and evaluate the body.
    This is how substitution works in NbE — no syntax manipulation needed. *)
and apply_closure (Clos (env, name, body)) (arg : value) : value =
  eval ((name, arg) :: env) body

(** Evaluate a block of statements, returning the last expression's value *)
and eval_block (env : env) (stmts : sttm list) : value =
  match stmts with
  | [] -> VUnitType
  | [(SttmExpr expr, _)] -> eval env expr
  | (SttmLet (pattern, rhs), _) :: rest ->
    let v = eval env rhs in
    let env' = bind_pattern env pattern v in
    eval_block env' rest
  | (SttmExpr expr, _) :: rest ->
    ignore (eval env expr);
    eval_block env rest

(** Bind a pattern match result into the environment *)
and bind_pattern (env : env) (pattern : pattern) (v : value) : env =
  let (kind, _) = pattern in
  match kind with
  | PVar (name, _) -> (name, v) :: env
  | PWildcard -> env
  | PLit _ -> env
  | PConstructor _ -> env

(** Evaluate field access *)
and eval_access (record : value) (field : string) : value =
  match record with
  | VRecord fields ->
    (match List.assoc_opt field fields with
     | Some v -> v
     | None -> failwith ("eval: field not found: " ^ field))
  | VNeutral (ty, ne) -> VNeutral (ty, NAccess (ne, field))
  | _ -> failwith "eval: not a record"

(** Try to match a value against pattern-match cases *)
and eval_match (env : env) (scrut : value)
    (cases : (pattern * expr) list) : value =
  match cases with
  | [] -> failwith "eval: non-exhaustive match"
  | (pattern, body) :: rest ->
    (match try_match env pattern scrut with
     | Some env' -> eval env' body
     | None -> eval_match env scrut rest)

(** Try to match a value against a single pattern *)
and try_match (env : env) (pattern : pattern) (v : value) : env option =
  let (kind, _) = pattern in
  match kind, v with
  | PVar (name, _), _ -> Some ((name, v) :: env)
  | PWildcard, _ -> Some env
  | PLit (LitInt n1, _), VIntLit n2 when n1 = n2 -> Some env
  | PLit (LitString s1, _), VStringLit s2 when s1 = s2 -> Some env
  | PConstructor ((name1, _), pats), VConstructor (name2, vals)
    when name1 = name2 && List.length pats = List.length vals ->
    List.fold_left2 (fun acc pat v ->
      match acc with Some env -> try_match env pat v | None -> None
    ) (Some env) pats vals
  | _ -> None


(* ========================================================================
   SHOWING VALUES — converting values back to readable strings
   ========================================================================

   This is a simplified form of "quoting" (the "N" in NbE). A full quoter
   would convert values back to [Ast.expr]; we just need strings for error
   messages. For Pi and Lambda, we apply the closure to a fresh neutral
   variable to "peek inside" the body. *)

(** Convert a value to a human-readable string *)
let rec show_value (lvl : int) (v : value) : string =
  match v with
  | VType -> "Type"
  | VIntType -> "int"
  | VStringType -> "string"
  | VBoolType -> "bool"
  | VUnitType -> "unit"
  | VIntLit n -> string_of_int n
  | VStringLit s -> "\"" ^ s ^ "\""
  | VPi (name, domain, clos) ->
    let fresh = VNeutral (domain, NVar lvl) in
    let codomain = apply_closure clos fresh in
    if name = "_" then
      show_value lvl domain ^ " -> " ^ show_value (lvl + 1) codomain
    else
      "(" ^ name ^ " : " ^ show_value lvl domain ^ ") -> "
      ^ show_value (lvl + 1) codomain
  | VLam (name, clos) ->
    let fresh = VNeutral (VType, NVar lvl) in
    let body = apply_closure clos fresh in
    "fn " ^ name ^ " => " ^ show_value (lvl + 1) body
  | VNeutral (_, ne) -> show_neutral ne
  | VDataType (name, []) -> name
  | VDataType (name, args) ->
    name ^ "(" ^ String.concat ", " (List.map (show_value lvl) args) ^ ")"
  | VConstructor (name, []) -> name
  | VConstructor (name, args) ->
    name ^ "(" ^ String.concat ", " (List.map (show_value lvl) args) ^ ")"
  | VRecordType fields ->
    "{ " ^ String.concat ", "
      (List.map (fun (n, t) -> n ^ " : " ^ show_value lvl t) fields) ^ " }"
  | VRecord fields ->
    "{ " ^ String.concat ", "
      (List.map (fun (n, v) -> n ^ " = " ^ show_value lvl v) fields) ^ " }"

and show_neutral (ne : neutral) : string =
  match ne with
  | NVar l -> "x" ^ string_of_int l
  | NApp (f, arg) -> show_neutral f ^ "(" ^ show_value 0 arg ^ ")"
  | NAccess (r, field) -> show_neutral r ^ "." ^ field


(* ========================================================================
   CONVERSION CHECKING — are two values equal?
   ========================================================================

   This is the heart of dependent type checking. Since types are values,
   "are these two types the same?" becomes "do these two values compute
   to the same thing?"

   The algorithm: compare values structurally. For binders (Pi, Lambda),
   apply both closures to the SAME fresh variable and compare the results.

   The [lvl] parameter tracks how many binders we've gone under, so fresh
   variables don't collide. This is why we use de Bruijn levels — two
   independent closures that bind different variable names still produce
   the same level for the same position, making alpha-equivalence automatic.

   Example: [fn x => x] and [fn y => y] are the same function.
   - Apply both to [NVar 0]
   - First gives [NVar 0], second gives [NVar 0]
   - They match → equal ✓ *)

(** Check if two values are convertible (definitionally equal) *)
let rec conv (lvl : int) (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | VType, VType -> true
  | VIntType, VIntType -> true
  | VStringType, VStringType -> true
  | VBoolType, VBoolType -> true
  | VUnitType, VUnitType -> true
  | VIntLit n1, VIntLit n2 -> n1 = n2
  | VStringLit s1, VStringLit s2 -> s1 = s2

  (* Pi types: compare domains, then compare codomains under a fresh binder *)
  | VPi (_, a1, clos1), VPi (_, a2, clos2) ->
    if not (conv lvl a1 a2) then false
    else
      let fresh = VNeutral (a1, NVar lvl) in
      conv (lvl + 1) (apply_closure clos1 fresh) (apply_closure clos2 fresh)

  (* Lambdas: compare bodies under a fresh binder *)
  | VLam (_, clos1), VLam (_, clos2) ->
    let fresh = VNeutral (VType, NVar lvl) in
    conv (lvl + 1) (apply_closure clos1 fresh) (apply_closure clos2 fresh)

  (* Eta: fn x => f(x) is equal to f *)
  | VLam (_, clos), other ->
    let fresh = VNeutral (VType, NVar lvl) in
    conv (lvl + 1) (apply_closure clos fresh) (apply other fresh)
  | other, VLam (_, clos) ->
    let fresh = VNeutral (VType, NVar lvl) in
    conv (lvl + 1) (apply other fresh) (apply_closure clos fresh)

  (* Neutral values: compare structurally *)
  | VNeutral (_, ne1), VNeutral (_, ne2) ->
    conv_neutral lvl ne1 ne2

  | VDataType (n1, args1), VDataType (n2, args2) ->
    n1 = n2
    && List.length args1 = List.length args2
    && List.for_all2 (conv lvl) args1 args2

  | VConstructor (n1, args1), VConstructor (n2, args2) ->
    n1 = n2
    && List.length args1 = List.length args2
    && List.for_all2 (conv lvl) args1 args2

  | VRecordType f1, VRecordType f2 ->
    List.length f1 = List.length f2
    && List.for_all2
         (fun (n1, t1) (n2, t2) -> n1 = n2 && conv lvl t1 t2)
         f1 f2

  | VRecord f1, VRecord f2 ->
    List.length f1 = List.length f2
    && List.for_all2
         (fun (n1, v1) (n2, v2) -> n1 = n2 && conv lvl v1 v2)
         f1 f2

  | _ -> false

(** Compare two neutral values structurally *)
and conv_neutral (lvl : int) (ne1 : neutral) (ne2 : neutral) : bool =
  match ne1, ne2 with
  | NVar l1, NVar l2 -> l1 = l2
  | NApp (f1, a1), NApp (f2, a2) ->
    conv_neutral lvl f1 f2 && conv lvl a1 a2
  | NAccess (r1, f1), NAccess (r2, f2) ->
    conv_neutral lvl r1 r2 && f1 = f2
  | _ -> false
