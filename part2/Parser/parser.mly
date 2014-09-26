%{
open Ast
open Exceptions
open Lexing

let parse_error _ =
  let start_pos = Parsing.symbol_start_pos () in
  let end_pos = Parsing.symbol_end_pos () in
  let start_line = string_of_int start_pos.pos_lnum in
  let start_char = string_of_int (start_pos.pos_cnum - start_pos.pos_bol) in
  let end_line = string_of_int end_pos.pos_lnum in
  let end_char = string_of_int (end_pos.pos_cnum - end_pos.pos_bol) in
  raise (ParseError (start_line^"."^start_char^"-"^end_line^"."^end_char))

let rec desugar_lambda e = function
  | [] -> e
  | v::vs -> Fun (v,desugar_lambda e vs)

let desugar_def vs e include_rec =
  let v,e' as t = match vs with
    | []    -> parse_error vs
    | [v]   -> v,e
    | x::xs -> x,desugar_lambda e xs in
  if include_rec then v, LetRec (v,e',Var v) else t

let desugar_let e e' xs include_rec =
  let desugar_to_tuple e e' = function
    | [] as vs -> parse_error vs
    | [v] -> (v,e,e')
    | v::vs -> (v, desugar_lambda e vs, e') in
  let (v,ne,ne') = desugar_to_tuple e e' xs in
  if include_rec then LetRec (v,ne,ne') else Let (v,ne,ne')

%}

%token ASSIGN
%token REF
%token DEREF
%token LBRACE
%token RBRACE
%token MATCH
%token WITH
%token PIPE
%token FALSE
%token TRUE
%token IF
%token THEN
%token ELSE
%token PLUS
%token MINUS
%token MULT
%token DIVIDE
%token MOD
%token LET
%token REC
%token IN
%token GT
%token LT
%token LTQ
%token GTQ
%token EQ
%token NEQ
%token NOT
%token AND
%token OR
%token <string> VAR
%token <string> INT
%token <string> FILE
%token LPAREN RPAREN
%token IMP
%token FUN
%token APP
%token NIL
%token UNIT
%token CONS
%token USE
%token RESET
%token QUIT
%token HELP
%token ENV
%token EOF
%token SEMICOLON

%nonassoc LET
%nonassoc FUN MATCH IMP IF IN
%right PIPE
%right OR AND CONS
%left GT LT LTQ GTQ EQ NEQ ASSIGN
%left PLUS MINUS MULT DIVIDE MOD
%nonassoc NOT REF DEREF
%nonassoc VAR LBRACE UNIT INT TRUE FALSE LPAREN
%left APP

/* entry point */

%start toplevel_input
%type <Ast.toplevel_input> toplevel_input

%start from_file
%type <Ast.definition list> from_file

%%

toplevel_input:
  | definition { Definition $1 }
  | expr       { Expression $1 }
  | directive  { Directive $1 }
;

from_file:
  | EOF                  { [] }
  | definition from_file { $1 :: $2 }
;

definition:
  | LET var_list EQ expr     { desugar_def $2 $4 false }
  | LET REC var_list EQ expr { desugar_def $3 $5 true }
;

directive:
  | ENV      { Env }
  | RESET    { Reset }
  | QUIT     { Quit }
  | HELP     { Help }
  | USE FILE { Use $2}

constant:
  | FALSE               { Bool false }
  | TRUE                { Bool true }
  | INT                 { Int (int_of_string $1) }
  | UNIT                { Unit }
;

pattern_matching:
  | pattern IMP expr PIPE pattern_matching %prec PIPE { ($1, $3)::$5 }
  | pattern IMP expr                       { [($1, $3)]   }
;

plist:
  |                     { PConstant Nil }
  | pattern             { PCons ($1, PConstant Nil) }
  | pattern SEMICOLON plist { PCons ($1, $3) }
;

pattern:
  | VAR                  { PVar $1 }
  | constant             { PConstant $1 }
  | LBRACE plist RBRACE  { $2 }
  | LPAREN pattern RPAREN { $2 }
  | pattern CONS pattern { PCons ($1, $3) }

list:
  |                     { Constant Nil}
  | expr                { Cons ($1, Constant Nil) }
  | expr SEMICOLON list { Cons ($1, $3) }
;

var_list:
  | VAR          { [$1] }
  | var_list VAR { $1@[$2]}

expr:
  | VAR                 { Var $1 }
  | LBRACE list RBRACE  { $2 }
  | constant            { Constant $1 }
  | IF expr THEN expr ELSE expr %prec IF { IfThenElse ($2, $4, $6) }
  | expr CONS expr      { Cons ($1, $3) }
  | expr PLUS expr      { BinaryOp (Plus,  $1, $3) }
  | expr MINUS expr     { BinaryOp (Minus, $1, $3) }
  | expr MULT expr      { BinaryOp (Mult,  $1, $3) }
  | expr DIVIDE expr    { BinaryOp (Divide,$1, $3) }
  | expr MOD expr       { BinaryOp (Mod,   $1, $3) }
  | expr GT expr        { BinaryOp (Gt,    $1, $3) }
  | expr LT expr        { BinaryOp (Lt,    $1, $3) }
  | expr LTQ expr       { BinaryOp (Ltq,   $1, $3) }
  | expr GTQ expr       { BinaryOp (Gtq,   $1, $3) }
  | expr EQ expr        { BinaryOp (Eq,    $1, $3) }
  | expr NEQ expr       { BinaryOp (Neq,   $1, $3) }
  | NOT expr            { UnaryOp (Not, $2) }
  | expr AND expr       { BinaryOp (And, $1, $3) }
  | expr OR expr        { BinaryOp (Or, $1, $3) }
  | MATCH expr WITH pattern_matching %prec MATCH      { Match ($2, $4) }
  | MATCH expr WITH PIPE pattern_matching %prec MATCH { Match ($2, $5) }
  | LET REC var_list EQ expr IN expr { desugar_let $5 $7 $3 true }
  | LET var_list EQ expr IN expr     { desugar_let $4 $6 $2 false }
  | LPAREN expr RPAREN  %prec LPAREN { $2 }
  | FUN var_list IMP expr %prec FUN { desugar_lambda $4 $2 }
  | expr expr %prec APP { App ($1, $2) }
;
