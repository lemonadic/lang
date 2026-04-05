(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

open Ast
open Value
open Errors
open Location

(* ========================================================================
   BIDIRECTIONAL TYPE CHECKING
   ========================================================================

   The type checker has two modes (two "directions"):

   1. CHECK mode: [check ctx expr expected_type]
      "I know this expression should have this type — verify it."
      Used when we have type information flowing DOWN from the context.
      Example: [let x : int = expr] — we check [expr] against [int].

   2. INFER mode: [infer ctx expr]
      "I don't know the type — figure it out."
      Used when type information flows UP from the expression.
      Example: [5] infers to [int], [ExprVar "x"] infers from the context.

   The key rule connecting them: when checking and we don't have a special
   case, fall back to infer-then-compare:
     check ctx expr expected =
       let inferred = infer ctx expr in
       if conv inferred expected then OK else TYPE MISMATCH

   This separation is what makes dependent type checking work:
   - Lambdas can only be CHECKED (we need to know the domain type)
   - Variables and applications can be INFERRED
   - Pi types infer to [Type] *)

(** Result monad helpers *)
let ( >>= ) r f = match r with Ok v -> f v | Error e -> Error e

(* ========================================================================
   TYPE CHECKING CONTEXT
   ======================================================================== *)

(** The context carries everything we need during type checking:
    - [env]: for EVALUATING expressions to values (name → value)
    - [types]: for LOOKING UP types of variables (name → type-as-value)
    - [level]: de Bruijn level counter for generating fresh variables *)
type ctx = {
  env : env;
  types : (string * value) list;
  level : int;
}

(** Extend context with a defined name (known value) *)
let define ctx name ty value =
  { env = (name, value) :: ctx.env;
    types = (name, ty) :: ctx.types;
    level = ctx.level; }

(** Extend context with a fresh variable (for going under binders).
    Returns the fresh variable AND the extended context.

    When checking under a binder like [(x : A) -> B], we don't know [x]'s
    value. So we create a fresh neutral variable [VNeutral(A, NVar level)]
    as a placeholder. This lets us evaluate [B] and check it. *)
let bind ctx name ty =
  let fresh = VNeutral (ty, NVar ctx.level) in
  (fresh, { env = (name, fresh) :: ctx.env;
            types = (name, ty) :: ctx.types;
            level = ctx.level + 1; })

(** The initial context with primitive types.
    Each primitive is both a TYPE (has type [Type]) and a VALUE. *)
let initial_ctx () =
  { env = [
      ("Type",   VType);
      ("int",    VIntType);
      ("string", VStringType);
      ("bool",   VBoolType);
      ("unit",   VUnitType);
    ];
    types = [
      ("Type",   VType);      (* Type : Type — inconsistent but practical *)
      ("int",    VType);      (* int : Type *)
      ("string", VType);      (* string : Type *)
      ("bool",   VType);      (* bool : Type *)
      ("unit",   VType);      (* unit : Type *)
    ];
    level = 0;
  }

(* ========================================================================
   ERROR HELPERS
   ======================================================================== *)

let dummy_pos =
  { start_pos = { line = 0; column = 0 };
    end_pos = { line = 0; column = 0 }; }

let make_error id message loc =
  { id; message; file = ""; location = loc; hints = []; additional_info = [] }

let type_mismatch_error ctx expected actual loc =
  make_error 1002
    ("Type mismatch: expected " ^ Eval.show_value ctx.level expected
     ^ ", got " ^ Eval.show_value ctx.level actual)
    loc

let unbound_var_error name loc =
  make_error 1001 ("Unbound variable: " ^ name) loc

let not_a_function_error ctx ty loc =
  make_error 1003
    ("Expected a function type, got " ^ Eval.show_value ctx.level ty)
    loc

(* ========================================================================
   AST BUILDERS — for synthesizing expressions from declarations
   ======================================================================== *)

(** Build a nested Pi type from parameters and a return type.
    [(x: A, y: B) -> C] becomes [ExprPi(x, A, ExprPi(y, B, C))] *)
let build_pi_type params return_type =
  List.fold_right (fun ((name, name_pos), type_expr) acc ->
    (ExprPi (Some (name, name_pos), type_expr, acc), dummy_pos)
  ) params return_type

(** Build nested lambdas from parameters and a body.
    [(x: A, y: B) => body] becomes [fn x => fn y => body] *)
let build_lambda params body =
  List.fold_right (fun ((name, name_pos), _) acc ->
    (ExprLambda ((name, name_pos), acc), dummy_pos)
  ) params body

(* ========================================================================
   BIDIRECTIONAL TYPE CHECKER — check and infer
   ======================================================================== *)

(** CHECK mode: verify that [expr] has type [expected].

    Special cases handle constructs that need type information pushed down:
    - Lambdas: need the Pi type to know the domain
    - Blocks: propagate the expected type to the last expression

    Everything else falls through to infer-then-compare. *)
let rec check ctx (expr : expr) (expected : value)
    : (unit, compiler_error) result =
  let (kind, loc) = expr in
  match kind with
  (* Lambda checked against Pi: introduce the binding *)
  | ExprLambda ((name, _), body) ->
    check_lambda ctx name body expected loc

  (* Blocks: propagate expected type to the last statement *)
  | ExprBlock stmts ->
    check_block ctx stmts expected

  (* Default: infer the type, then check it matches *)
  | _ ->
    infer ctx expr >>= fun inferred ->
    if Eval.conv ctx.level inferred expected then Ok ()
    else Error (type_mismatch_error ctx expected inferred loc)

(** Check a lambda against an expected type (must be a Pi type) *)
and check_lambda ctx name body expected loc =
  match expected with
  | VPi (_, domain, codomain_clos) ->
    (* Introduce a fresh variable for the parameter *)
    let (fresh, ctx') = bind ctx name domain in
    (* Compute the expected return type by applying the codomain closure *)
    let codomain = Eval.apply_closure codomain_clos fresh in
    (* Check the body against the expected return type *)
    check ctx' body codomain
  | _ ->
    Error (make_error 1002
      ("Lambda requires a function type, got "
       ^ Eval.show_value ctx.level expected)
      loc)

(** Check a block of statements against an expected type *)
and check_block ctx (stmts : sttm list) (expected : value)
    : (unit, compiler_error) result =
  match stmts with
  | [] ->
    if Eval.conv ctx.level VUnitType expected then Ok ()
    else Error (type_mismatch_error ctx expected VUnitType dummy_pos)
  | [(SttmExpr expr, _)] ->
    check ctx expr expected
  | (SttmLet (pattern, rhs), _) :: rest ->
    infer ctx rhs >>= fun rhs_ty ->
    let rhs_val = Eval.eval ctx.env rhs in
    let ctx' = bind_pattern_ctx ctx pattern rhs_ty rhs_val in
    check_block ctx' rest expected
  | (SttmExpr expr, _) :: rest ->
    infer ctx expr >>= fun _ ->
    check_block ctx rest expected

(** INFER mode: determine the type of [expr].

    Each construct has a natural type:
    - Variables: look up in context
    - Literals: [int] or [string]
    - Pi types: always [Type] (a type of types)
    - Applications: infer function type, check argument, return codomain *)
and infer ctx (expr : expr) : (value, compiler_error) result =
  let (kind, loc) = expr in
  match kind with
  | ExprVar (name, _) ->
    (match List.assoc_opt name ctx.types with
     | Some ty -> Ok ty
     | None -> Error (unbound_var_error name loc))

  | ExprLit (LitInt _, _) -> Ok VIntType
  | ExprLit (LitString _, _) -> Ok VStringType

  (* Pi types have type Type — but we must check that domain and codomain
     are themselves valid types *)
  | ExprPi (name_opt, domain, codomain) ->
    check ctx domain VType >>= fun () ->
    let domain_val = Eval.eval ctx.env domain in
    let bound_name = match name_opt with Some (n, _) -> n | None -> "_" in
    let (_, ctx') = bind ctx bound_name domain_val in
    check ctx' codomain VType >>= fun () ->
    Ok VType

  (* Lambdas cannot be inferred — we need the domain type from context *)
  | ExprLambda _ ->
    Error (make_error 1003
      "Cannot infer type of lambda without context; add a type annotation"
      loc)

  (* Application: infer function type, check args against domains *)
  | ExprCall (func, args) ->
    infer ctx func >>= fun func_ty ->
    infer_app ctx func_ty args loc

  | ExprMatch (scrutinee, cases) ->
    infer_match ctx scrutinee cases loc

  | ExprBlock stmts ->
    infer_block ctx stmts loc

  | ExprAccess (record, (ExprVar (field, _), _)) ->
    infer_access ctx record field loc

  | ExprAccess _ ->
    Error (make_error 1005 "Invalid field access" loc)

(** Infer the result type of a function application.
    For [f(a, b, c)], we process arguments one at a time:
    1. [f : (x:A) -> B] applied to [a] gives [B[x:=a]]
    2. Then apply that to [b], etc. *)
and infer_app ctx func_ty args loc =
  match args with
  | [] -> Ok func_ty
  | arg :: rest ->
    (match func_ty with
     | VPi (_, domain, codomain_clos) ->
       check ctx arg domain >>= fun () ->
       let arg_val = Eval.eval ctx.env arg in
       let result_ty = Eval.apply_closure codomain_clos arg_val in
       infer_app ctx result_ty rest loc
     | _ ->
       Error (not_a_function_error ctx func_ty loc))

(** Infer the type of a match expression.
    All branches must have the same type. *)
and infer_match ctx scrutinee cases loc =
  infer ctx scrutinee >>= fun scrut_ty ->
  match cases with
  | [] -> Error (make_error 1007 "Empty match expression" loc)
  | (first_pat, first_body) :: rest_cases ->
    let ctx' = bind_match_pattern ctx first_pat scrut_ty in
    infer ctx' first_body >>= fun result_ty ->
    let rec check_rest = function
      | [] -> Ok result_ty
      | (pat, body) :: rest ->
        let ctx' = bind_match_pattern ctx pat scrut_ty in
        check ctx' body result_ty >>= fun () ->
        check_rest rest
    in
    check_rest rest_cases

(** Infer the type of a block *)
and infer_block ctx (stmts : sttm list) loc =
  match stmts with
  | [] -> Ok VUnitType
  | [(SttmExpr expr, _)] -> infer ctx expr
  | (SttmLet (pattern, rhs), _) :: rest ->
    infer ctx rhs >>= fun rhs_ty ->
    let rhs_val = Eval.eval ctx.env rhs in
    let ctx' = bind_pattern_ctx ctx pattern rhs_ty rhs_val in
    infer_block ctx' rest loc
  | (SttmExpr expr, _) :: rest ->
    infer ctx expr >>= fun _ ->
    infer_block ctx rest loc

(** Infer the type of a field access *)
and infer_access ctx record field loc =
  infer ctx record >>= fun record_ty ->
  match record_ty with
  | VRecordType fields ->
    (match List.assoc_opt field fields with
     | Some field_ty -> Ok field_ty
     | None ->
       Error (make_error 1005
         ("Record type has no field: " ^ field) loc))
  | _ ->
    Error (make_error 1005
      ("Expected a record type, got " ^ Eval.show_value ctx.level record_ty)
      loc)

(** Bind a pattern variable to the scrutinee's type in the context *)
and bind_match_pattern ctx (pattern : pattern) (scrut_ty : value) : ctx =
  let (kind, _) = pattern in
  match kind with
  | PVar (name, _) -> snd (bind ctx name scrut_ty)
  | _ -> ctx

(** Bind a let-pattern into the context with a known type and value *)
and bind_pattern_ctx ctx (pattern : pattern) ty v =
  let (kind, _) = pattern in
  match kind with
  | PVar (name, _) -> define ctx name ty v
  | _ -> ctx


(* ========================================================================
   DECLARATION TYPE CHECKING
   ======================================================================== *)

(** Type check a let declaration.

    For [let f (x: A, y: B) : C = body]:
    1. Check that [A], [B], [C] are valid types (check against [Type])
    2. Introduce parameters as fresh variables
    3. Check [body] against [C]
    4. Build the function type [(x:A) -> (y:B) -> C]
    5. Add [f] to the context *)
let type_check_let_decl ctx (decl : let_decl) =
  (* Process parameters: check each type annotation and bind the parameter *)
  let rec process_params ctx = function
    | [] -> Ok ctx
    | ((name, _), type_expr) :: rest ->
      check ctx type_expr VType >>= fun () ->
      let param_ty = Eval.eval ctx.env type_expr in
      let (_, ctx') = bind ctx name param_ty in
      process_params ctx' rest
  in
  process_params ctx decl.params >>= fun body_ctx ->

  (* Check the return type annotation is a valid type *)
  check body_ctx decl.return_type VType >>= fun () ->
  let return_ty = Eval.eval body_ctx.env decl.return_type in

  (* Check the body against the return type *)
  check body_ctx decl.body return_ty >>= fun () ->

  (* Build the overall function type by wrapping params as Pi types *)
  let func_type_expr = build_pi_type decl.params decl.return_type in
  let func_ty = Eval.eval ctx.env func_type_expr in

  (* Build the function value by wrapping body in lambdas *)
  let func_body_expr = build_lambda decl.params decl.body in
  let func_val = Eval.eval ctx.env func_body_expr in

  Ok (define ctx (fst decl.name) func_ty func_val)

(** Type check a type declaration.

    For [type Option = | Some(int) | None]:
    1. Register the type name
    2. For each constructor, compute its type and register it

    For [type User = { name: string, age: int }]:
    1. Check each field type
    2. Register the type as a record type *)
let type_check_type_decl ctx (decl : type_decl) =
  let type_name = fst decl.name in

  (* Register the type name first so constructors can reference it *)
  let type_val = VDataType (type_name, []) in
  let ctx = define ctx type_name VType type_val in

  match decl.value with
  | TyInductive variants ->
    let rec process_variants ctx = function
      | [] -> Ok ctx
      | ((ctor_name, ctor_pos), arg_types) :: rest ->
        (* Check all constructor argument types are valid *)
        let rec check_arg_types = function
          | [] -> Ok ()
          | arg :: rest_args ->
            check ctx arg VType >>= fun () ->
            check_arg_types rest_args
        in
        check_arg_types arg_types >>= fun () ->

        (* Build the constructor type: arg1 -> arg2 -> ... -> T *)
        let ctor_type_expr =
          List.fold_right (fun arg_ty acc ->
            (ExprPi (None, arg_ty, acc), dummy_pos)
          ) arg_types (ExprVar (type_name, ctor_pos), dummy_pos)
        in
        let ctor_ty = Eval.eval ctx.env ctor_type_expr in
        let ctor_val = VConstructor (ctor_name, []) in
        let ctx = define ctx ctor_name ctor_ty ctor_val in
        process_variants ctx rest
    in
    process_variants ctx variants

  | TyStruct fields ->
    (* Check each field type *)
    let rec check_fields = function
      | [] -> Ok ()
      | ((_, _), type_expr) :: rest ->
        check ctx type_expr VType >>= fun () ->
        check_fields rest
    in
    check_fields fields >>= fun () ->

    (* Build the record type value *)
    let field_types = List.map (fun ((name, _), type_expr) ->
      (name, Eval.eval ctx.env type_expr)
    ) fields in

    (* Redefine the type name with the record type as its value *)
    let ctx = define ctx type_name VType (VRecordType field_types) in
    Ok ctx

(** Type check a declaration *)
let type_check_decl ctx (decl : declaration) =
  match decl.value with
  | LetDecl ld -> type_check_let_decl ctx ld
  | TypeDef td -> type_check_type_decl ctx td

(** Type check an entire program *)
let type_check_program (program : program) : (unit, compiler_error) result =
  let ctx = initial_ctx () in
  let rec go ctx = function
    | [] -> Ok ()
    | decl :: rest ->
      type_check_decl ctx decl >>= fun ctx' ->
      go ctx' rest
  in
  go ctx program
