(*
Copyright 2024 Lemonadic. All rights reserved.
Licensed under the Apache License, Version 2.0 as described in the file LICENSE.

Authors: Davi William, Sofia Rodrigues
*)
%{
open Ast
open Location

(* Creates an AST position using the Menhir internal position *)
let loc (startpos, endpos) = {
  file = None;
  start = {
    column = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol;
    line = startpos.Lexing.pos_lnum
  };
  end' = {
    column = endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol;
    line = endpos.Lexing.pos_lnum
  }
}

%}

%token <string> IDENT
%token <int> INT
%token <string> STRING

(* Keywords *)
%token TYPE FN PUB LET MATCH

(* Symbols *)
%token EQUAL DOT COMMA COLON WILDCARD
%token LPAREN RPAREN LBRACE RBRACE
%token ARROW FATARROW BAR

(* Virtual Tokens *)
%token EOF

%start <Ast.program> program

%%

(* Macro for creating some rule with a position in a tuple. *)
localized(x):
  | d = x { (d, (loc $loc)) }

(* The entrypoint of the entire parser, it parses a sequence of top level definitons. *)
program:
  | EOF { [] }
  | d = decl; p = program { d :: p }

(* The identifier rule. *)
ident:
  | localized(IDENT) { $1 }

literal_kind:
  | INT { LitInt $1 }
  | STRING { LitString $1 }

literal:
  | localized(literal_kind) { $1 }

atom_kind:
  | ident { ExprVar $1 }
  | literal { ExprLit $1 }
  | expr LPAREN separated_list(COMMA, expr) RPAREN { ExprCall ($1, $3) }
  | LPAREN expr_kind RPAREN { $2 }

atom:
  | localized(atom_kind) { $1 }

pattern_kind:
  | ident { PVar $1 }
  | literal { PLit $1 }
  | WILDCARD { PWildcard }
  | ident; LPAREN; separated_list(COMMA, pattern); RPAREN { PConstructor ($1, $3) }
  | LPAREN; pattern_kind; RPAREN { $2 }

pattern:
  | localized(pattern_kind) { $1 }

match_case:
  | BAR; pattern = pattern; FATARROW; expr = expr { pattern, expr }

stmt_kind:
  | LET; pattern = pattern; EQUAL; body = expr { SttmLet (pattern, body) }
  | expr { SttmExpr $1 }

stmt:
  | localized(stmt_kind) { $1 }

expr_kind:
  | MATCH; scrutinee = expr; LBRACE; cases = list(match_case); RBRACE { ExprMatch (scrutinee, cases) }
  | LBRACE; separated_list(COMMA, stmt); RBRACE { ExprBlock $2 }
  | atom_kind { $1 }

expr:
  | localized(expr_kind) { $1 }

param:
  | name = ident; COLON; typ = expr { (name, typ) }

parameters:
  | LPAREN; params = separated_list(COMMA, param); RPAREN; { params }
  | (* empty uwu *) { [] }

let_decl:
  | LET; name = ident; params = parameters; COLON; return_type = expr; EQUAL; body = expr
    { { name; params; return_type; body }}

visibility:
  | PUB { Public }
  | (* empty uwu *) { Private }

binder_kind:
  | name = ident; COLON; typ = expr { name, Some typ }
  | name = ident { name, None }

binder:
  | localized(binder_kind) { $1 }

type_binders:
  | LPAREN; binders = separated_list(COMMA, binder); RPAREN; { binders }
  | (* empty uwu *) { [] }

field:
  | name = ident; COLON; typ = expr { name, typ }

fields:
  | l = separated_list(COMMA, field) { l }

alt_params:
  | LPAREN; separated_list(COMMA, expr); RPAREN { $2 }
  | (* empty uwu *) { [] }

alt:
  | BAR; name = ident; params = alt_params { name, params }

type_value:
  | LBRACE; fields = fields; RBRACE { TyStruct fields }
  | nonempty_list(alt) { TyInductive $1 }

type_decl:
  | TYPE; name = ident; binders = type_binders; return_type = option(COLON; expr { $2 }); EQUAL; value = type_value {
    { name; binders; return_type; value }
  }

(* Top-level program declarations. *)
decl_kind:
  | let_decl { LetDecl $1 }
  | type_decl { TypeDef $1 }

decl:
  | visibility = visibility; value = decl_kind { { value; visibility }}