(*
Copyright 2025 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Júnior Nascimento
*)

open Ast
open Type_expr
open Type_env
open Location
open Errors

(* Type checking error codes *)
let error_unbound_variable = 1001
let error_type_mismatch = 1002
let error_not_a_function = 1003
let error_return_type_mismatch = 1004
let error_undefined_type = 1005
let error_pattern_match = 1006
let error_incomplete_match = 1007

(* Create a type checking error *)
let make_type_error id message file location hints =
  { id; message; file; location; hints; additional_info = [] }

(** Result type for type checking operations *)
type 'a type_check_result = ('a, compiler_error) result

(** Bind operator for the result monad *)
let ( >>= ) result f =
  match result with
  | Ok value -> f value
  | Error err -> Error err

(** Map operator for the result monad *)
let ( >>| ) result f =
  match result with
  | Ok value -> Ok (f value)
  | Error err -> Error err

(** Creates a type error for an unbound variable *)
let unbound_variable_error name loc =
  make_type_error 
    error_unbound_variable
    ("Unbound variable: " ^ name)
    ""  (* file name will be filled in later *)
    loc
    []

(** Creates a type error for a type mismatch *)
let type_mismatch_error expected actual loc =
  make_type_error
    error_type_mismatch
    ("Type mismatch: expected " ^ show_type_expr expected ^ 
     ", got " ^ show_type_expr actual)
    ""
    loc
    []

(** Creates a type error for calling a non-function *)
let not_a_function_error ty loc =
  make_type_error
    error_not_a_function
    ("This expression has type " ^ show_type_expr ty ^ 
     " but is used as a function")
    ""
    loc
    []

(** Creates a type error for pattern matching *)
let pattern_match_error message loc =
  make_type_error
    error_pattern_match
    message
    ""
    loc
    []

(** Creates a type error for incomplete pattern matching *)
let incomplete_match_error ty loc =
  make_type_error
    error_incomplete_match
    ("Pattern matching on type " ^ show_type_expr ty ^ 
     " is not exhaustive")
    ""
    loc
    []

(** Checks if two types are compatible (structural equality) *)
let rec type_compatible t1 t2 =
  match (t1, t2) with
  | (TypeVar v1, TypeVar v2) -> 
      v1 = v2
  | (TypeConst c1, TypeConst c2) -> 
      c1 = c2
  | (TypeApp(c1, args1), TypeApp(c2, args2)) ->
      c1 = c2 && 
      List.length args1 = List.length args2 &&
      List.for_all2 type_compatible args1 args2
  | (TypeArrow(a1, r1), TypeArrow(a2, r2)) ->
      type_compatible a1 a2 && type_compatible r1 r2
  | (TypeRecord fields1, TypeRecord fields2) ->
      (* Check if all fields in the first record are in the second with compatible types *)
      List.for_all 
        (fun (name1, ty1) -> 
          List.exists 
            (fun (name2, ty2) -> name1 = name2 && type_compatible ty1 ty2) 
            fields2)
        fields1
  | _ -> false

(** Type checks a literal expression *)
let type_check_literal = function
  | (LitInt _, _) -> type_int
  | (LitString _, _) -> type_string

(** Forward declarations for mutually recursive functions *)
let rec type_check_expr env expr expected_type_opt =
  let (expr_kind, loc) = expr in
  match expr_kind with
  | ExprVar (name, _) -> 
      type_check_var env name loc expected_type_opt
  | ExprLit lit -> 
      type_check_lit lit loc expected_type_opt
  | ExprCall (func, args) -> 
      type_check_call env func args loc expected_type_opt
  | ExprMatch (scrutinee, cases) ->
      type_check_match env scrutinee cases loc expected_type_opt
  | ExprBlock stmts ->
      type_check_block env stmts loc expected_type_opt
  | ExprLambda (name, body) ->
      type_check_lambda env name body loc expected_type_opt
  | ExprAccess (record, field) ->
      type_check_access env record field loc expected_type_opt
  | ExprPi (_, _, _) ->
      Error (make_type_error
              error_undefined_type
              "Pi expressions should only be used in type contexts"
              ""
              loc
              [])

(** Convert an expression to a type expression *)
and expr_to_type_expr env (expr_kind, loc) =
  match expr_kind with
  | ExprVar (name, _) ->
      (match Type_env.lookup env name with
       | Some ty -> Ok ty
       | None -> Error (unbound_variable_error name loc))
  
  | ExprCall (func, args) ->
      (match func with
       | (ExprVar type_name, _) ->
           type_check_args env args >>= fun arg_types ->
           Ok (TypeApp (fst type_name, arg_types))
       | _ -> 
           Error (make_type_error 
                   error_undefined_type
                   "Invalid type application"
                   ""
                   loc
                   []))
  
  | ExprPi (Some (name, _), param_type, return_type) ->
      expr_to_type_expr env param_type >>= fun param_ty ->
      let local_env = Type_env.copy env in
      Type_env.add local_env name param_ty;
      expr_to_type_expr local_env return_type >>= fun return_ty ->
      Ok (TypeArrow (param_ty, return_ty))
  
  | ExprPi (None, param_type, return_type) ->
      expr_to_type_expr env param_type >>= fun param_ty ->
      expr_to_type_expr env return_type >>= fun return_ty ->
      Ok (TypeArrow (param_ty, return_ty))
  
  | _ -> 
      Error (make_type_error 
              error_undefined_type
              "Invalid type expression"
              ""
              loc
              [])

(** Check argument types for type applications *)
and type_check_args env args =
  let rec check_args acc = function
    | [] -> Ok (List.rev acc)
    | arg :: rest ->
        expr_to_type_expr env arg >>= fun ty ->
        check_args (ty :: acc) rest
  in
  check_args [] args

(** Type check a pattern and return variable bindings *)
and type_check_pattern env (pattern_kind, loc) expected_type =
  match pattern_kind with
  | PVar (name, _) ->
      (* Pattern variables bind the scrutinee's type to the variable *)
      Ok [(name, expected_type)]
  
  | PLit lit ->
      let lit_type = type_check_literal lit in
      if type_compatible lit_type expected_type then
        (* Literal patterns don't bind variables *)
        Ok []
      else
        Error (type_mismatch_error expected_type lit_type loc)
  
  | PWildcard ->
      (* Wildcard patterns don't bind variables *)
      Ok []
  
  | PConstructor ((name, _), args) ->
      (* Look up the constructor type *)
      match Type_env.lookup env name with
       | Some (TypeArrow (param_type, return_type)) ->
           (* Constructor must return an instance of the expected type *)
           if type_compatible return_type expected_type then
             match args with
             | [arg] -> 
                 (* Handle single argument constructors *)
                 type_check_pattern env arg param_type
             | [] -> 
                 (* Nullary constructors (no arguments) *)
                 Ok []
             | _ -> 
                 (* Multi-argument constructors not yet supported *)
                 Error (pattern_match_error 
                        "Multiple argument constructors not yet supported" 
                        loc)
           else
             Error (type_mismatch_error expected_type return_type loc)
       | Some ty ->
           (* The constructor is not a function *)
           Error (type_mismatch_error expected_type ty loc)
       | None ->
           (* Constructor not found *)
           Error (unbound_variable_error name loc)

(** Type check a variable reference *)
and type_check_var env name loc expected_type_opt =
  match Type_env.lookup env name with
  | Some ty -> 
      (match expected_type_opt with
       | Some expected_type ->
           if type_compatible ty expected_type then
             Ok expected_type
           else
             Error (type_mismatch_error expected_type ty loc)
       | None -> Ok ty)
  | None -> 
      Error (unbound_variable_error name loc)

(** Type check a literal *)
and type_check_lit lit loc expected_type_opt =
  let lit_type = type_check_literal lit in
  match expected_type_opt with
  | Some expected_type ->
      if type_compatible lit_type expected_type then
        Ok expected_type
      else
        Error (type_mismatch_error expected_type lit_type loc)
  | None -> Ok lit_type

(** Type check a function call *)
and type_check_call env func args loc expected_type_opt =
  (* First type check the function expression *)
  type_check_expr env func None >>= fun func_type ->
  
  match func_type with
  | TypeArrow (param_type, return_type) ->
      (* Handle function application based on argument count *)
      match args with
      | [arg] -> 
          (* Type check the argument against the parameter type *)
          type_check_expr env arg (Some param_type) >>= fun _ ->
          (* Check if the return type matches the expected type, if any *)
          (match expected_type_opt with
           | Some expected_type ->
               if type_compatible return_type expected_type then
                 Ok expected_type
               else
                 Error (type_mismatch_error expected_type return_type loc)
           | None -> Ok return_type)
      | [] ->
          (* Function call with no arguments *)
          Error (make_type_error
                  error_not_a_function
                  "Function call with no arguments"
                  ""
                  loc
                  [])
      | _ ->
          (* Multiple arguments would require curried functions or tuples *)
          Error (make_type_error
                  error_not_a_function
                  "Multiple arguments not yet supported"
                  ""
                  loc
                  [])
  | _ ->
      (* Not a function type *)
      Error (not_a_function_error func_type loc)

(** Type check a match expression *)
and type_check_match env scrutinee cases loc expected_type_opt =
  (* First type check the scrutinee to determine what we're matching on *)
  type_check_expr env scrutinee None >>= fun scrutinee_type ->
  
  (* Process each case and ensure consistent result types *)
  let process_case result_type_opt (pattern, expr) =
    (* Type check the pattern against the scrutinee type *)
    type_check_pattern env pattern scrutinee_type >>= fun bindings ->
    
    (* Create a new environment with pattern bindings *)
    let case_env = Type_env.copy env in
    List.iter (fun (name, ty) -> Type_env.add case_env name ty) bindings;
    
    (* Type check the case expression *)
    type_check_expr case_env expr expected_type_opt >>= fun case_type ->
    
    (* Ensure all cases have the same type *)
    match result_type_opt with
    | Some prev_type ->
        if type_compatible prev_type case_type then
          Ok (Some case_type)
        else
          Error (make_type_error
                  error_type_mismatch
                  "Match cases have inconsistent types"
                  ""
                  loc
                  [])
    | None -> Ok (Some case_type)
  in
  
  (* Process all cases to ensure they have consistent types *)
  let rec check_cases result_type_opt = function
    | [] -> 
        (match result_type_opt with
         | Some t -> Ok t
         | None -> 
             Error (make_type_error
                     error_type_mismatch
                     "Cannot determine type of empty match expression"
                     ""
                     loc
                     []))
    | case :: rest ->
        process_case result_type_opt case >>= fun new_result_type_opt ->
        check_cases new_result_type_opt rest
  in
  
  (* Ideally, we would check exhaustiveness here *)
  (* For now, we just check for consistent types *)
  check_cases None cases

(** Type check a block of statements *)
and type_check_block env stmts _ expected_type_opt =
  let block_env = Type_env.copy env in
  
  let rec check_stmts = function
    | [] -> 
        (* Empty block has unit type *)
        Ok type_unit
    | [(SttmExpr expr, _)] ->
        (* Last expression determines the block's type *)
        type_check_expr block_env expr expected_type_opt
    | (SttmLet (pattern, expr), _) :: rest ->
        (* Type check the binding expression *)
        type_check_expr block_env expr None >>= fun expr_type ->
        (* Type check the pattern against the expression type *)
        type_check_pattern block_env pattern expr_type >>= fun bindings ->
        
        (* Add bindings to the environment *)
        List.iter (fun (name, ty) -> Type_env.add block_env name ty) bindings;
        
        (* Continue with the rest of the statements *)
        check_stmts rest
    | (SttmExpr expr, _) :: rest ->
        (* Non-final expressions are evaluated for side effects *)
        type_check_expr block_env expr None >>= fun _ ->
        check_stmts rest
  in
  
  check_stmts stmts

(** Type check a lambda expression *)
and type_check_lambda env (name, _) body loc expected_type_opt =
  match expected_type_opt with
  | Some (TypeArrow (param_type, return_type)) ->
      (* Create environment with parameter binding *)
      let lambda_env = Type_env.copy env in
      Type_env.add lambda_env name param_type;
      
      (* Type check the body with the expected return type *)
      type_check_expr lambda_env body (Some return_type) >>= fun body_type ->
      
      (* Check if body type matches expected return type *)
      if type_compatible body_type return_type then
        Ok (TypeArrow (param_type, body_type))
      else
        Error (type_mismatch_error return_type body_type loc)
  
  | Some other_type ->
      (* Expected type is not a function type *)
      Error (make_type_error
              error_type_mismatch
              ("Expected a function type, got " ^ show_type_expr other_type)
              ""
              loc
              [])
  
  | None ->
      (* No expected type, can't infer lambda type without context *)
      Error (make_type_error
              error_type_mismatch
              "Cannot infer type for lambda without context"
              ""
              loc
              [])

(** Type check a record field access *)
and type_check_access env record field loc expected_type_opt =
  match field with
  | (ExprVar (field_name, _), _) ->
      (* Type check the record expression *)
      type_check_expr env record None >>= fun record_type ->
      
      match record_type with
      | TypeRecord fields ->
          (* Look up the field in the record type *)
          (match List.find_opt (fun (name, _) -> name = field_name) fields with
           | Some (_, field_type) ->
               (* Check if field type matches expected type, if any *)
               (match expected_type_opt with
                | Some expected_type ->
                    if type_compatible field_type expected_type then
                      Ok expected_type
                    else
                      Error (type_mismatch_error expected_type field_type loc)
                | None -> Ok field_type)
           | None ->
               (* Field not found in record *)
               Error (make_type_error
                       error_undefined_type
                       ("Record does not have field: " ^ field_name)
                       ""
                       loc
                       []))
      | TypeVar _ | TypeConst _ | TypeApp (_, _) | TypeArrow (_, _) ->
          (* Not a record type *)
          Error (make_type_error
                  error_type_mismatch
                  ("Expected a record type, got " ^ show_type_expr record_type)
                  ""
                  loc
                  [])
  | _ ->
      (* Not a variable expression for field access *)
      Error (make_type_error
              error_undefined_type
              "Invalid field access expression"
              ""
              loc
              [])

(** Type checks a let declaration *)
let type_check_let_decl env let_decl =
  (* Convert parameter types *)
  let rec process_params current_env acc = function
    | [] -> Ok (List.rev acc, current_env)
    | ((name, _), type_expr) :: rest ->
        (* Convert expression to type expression *)
        expr_to_type_expr current_env type_expr >>= fun param_type ->
        (* Add parameter to environment for subsequent parameters *)
        let updated_env = Type_env.copy current_env in
        Type_env.add updated_env (fst name) param_type;
        (* Continue with rest of parameters *)
        process_params updated_env ((fst name, param_type) :: acc) rest
  in
  
  (* Process all parameters *)
  process_params env [] (List.map (fun ((name, pos), expr) -> ((name, ()), pos), expr) let_decl.params) >>= fun (param_bindings, param_env) ->
  
  (* Convert return type expression to type *)
  expr_to_type_expr param_env let_decl.return_type >>= fun return_type ->
  
  (* Type check the function body against the return type *)
  type_check_expr param_env let_decl.body (Some return_type) >>= fun body_type ->
  
  (* Check if body type matches return type *)
  if type_compatible body_type return_type then
    (* Create the function type (fold parameters right-to-left) *)
    let func_type = List.fold_right
      (fun (_, param_type) acc -> TypeArrow (param_type, acc))
      param_bindings
      return_type
    in
    
    (* Add the function to the environment *)
    let result_env = Type_env.copy env in
    Type_env.add result_env (fst let_decl.name) func_type;
    Ok result_env
  else
    (* Body type doesn't match return type *)
    Error (make_type_error
            error_return_type_mismatch
            ("Function body type " ^ show_type_expr body_type ^ 
             " doesn't match declared return type " ^ show_type_expr return_type)
            ""
            (snd let_decl.name)
            [])

(** Type checks a type declaration *)
let type_check_type_decl env (type_def : type_decl) =
  let type_name = fst type_def.name in
  
  (* Process type parameters (binders) *)
  let rec process_type_binders current_env = function
    | [] -> Ok current_env
    | ((name, expr_opt), _) :: rest ->
        (match expr_opt with
         | Some type_expr ->
             (* Convert expression to type *)
             expr_to_type_expr current_env type_expr >>= fun param_type ->
             let updated_env = Type_env.copy current_env in
             Type_env.add updated_env (fst name) param_type;
             process_type_binders updated_env rest
         | None ->
             (* If no type is specified, assume it's a type *)
             let updated_env = Type_env.copy current_env in
             Type_env.add updated_env (fst name) (TypeConst "type");
             process_type_binders updated_env rest)
  in
  
  (* Process all type binders *)
  process_type_binders env type_def.binders >>= fun binder_env ->
  
  (* Create the result environment that will contain all type definitions *)
  let result_env = Type_env.copy env in
  
  (* Add the type itself to the result environment *)
  Type_env.add result_env type_name (TypeConst "type");
  
  (* Get type parameters if any *)
  let type_params = 
    List.map 
      (fun ((name, _), _) -> TypeVar (fst name)) 
      type_def.binders 
  in
  
  (* Create the type application with parameters *)
  let type_app = TypeApp (type_name, type_params) in
  
  match type_def.value with
  | TyInductive variants ->
      (* Process each variant constructor *)
      let process_variant (name, types) =
        (* Convert constructor parameter types with proper error handling *)
        let rec process_params acc = function
          | [] -> Ok (List.rev acc)
          | expr :: rest ->
              expr_to_type_expr binder_env expr >>= fun ty ->
              process_params (ty :: acc) rest
        in
        
        process_params [] types >>= fun param_types ->
        
        (* Create the constructor type as a function from params to the type *)
        let constructor_type = 
          List.fold_right
            (fun param_type acc -> TypeArrow (param_type, acc))
            param_types
            type_app  (* Constructor returns the parameterized type *)
        in
        
        Type_env.add result_env (fst name) constructor_type;
        Ok ()
      in
      
      (* Process all variants with proper error handling *)
      let rec process_variants = function
        | [] -> Ok result_env
        | variant :: rest ->
            process_variant variant >>= fun () ->
            process_variants rest
      in
      process_variants variants
  
  | TyStruct fields ->
      (* Convert field types with proper error handling *)
      let rec process_fields acc = function
        | [] -> Ok (List.rev acc)
        | (name, type_expr) :: rest ->
            expr_to_type_expr binder_env type_expr >>= fun ty ->
            process_fields ((fst name, ty) :: acc) rest
      in 
      process_fields [] fields >>= fun field_types ->
      
      (* Register the type as a record type *)
      Type_env.add result_env type_name (TypeRecord field_types);
      
      Ok result_env

(** Type checks a declaration *)
let type_check_decl env decl =
  match decl.value with
  | LetDecl let_decl -> type_check_let_decl env let_decl
  | TypeDef type_def -> type_check_type_decl env type_def

(** Type checks a program (list of declarations) *)
let type_check_program program =
  let initial_env = Type_env.initial_env () in
  
  (* Type check each declaration in order, accumulating environment *)
  let rec check_decls current_env = function
    | [] -> Ok current_env
    | decl :: rest ->
        type_check_decl current_env decl >>= fun updated_env ->
        check_decls updated_env rest
  in
  check_decls initial_env program