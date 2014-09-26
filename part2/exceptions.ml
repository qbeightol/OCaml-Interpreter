open Ast
open Printer

exception CircularDefinition of typ * typ
exception InexhaustiveMatch  of expr
exception ParseError         of string
exception RuntimeError       of string
exception TypeError          of expr * typ
exception UnboundVariable    of string
exception UndefinedValue     of expr
exception UnificationError   of typ * typ

let circular      t t' = raise (CircularDefinition (t,t'))
let inexhaustive  p    = raise (InexhaustiveMatch p)
let parse_error   s    = raise (ParseError s)
let runtime_error s    = raise (RuntimeError s)
let type_error    e t  = raise (TypeError (e,t))
let unbound_var   s    = raise (UnboundVariable s)
let undefined     e    = raise (UndefinedValue e)
let unify_error   t t' = raise (UnificationError (t,t'))

let add_tab s = "\t"^s

let show_circular t t' =
  print_endline "The type:";
  print_endline (add_tab (type_to_string t));
  print_endline "is circular. The type variable:";
  print_endline (add_tab (type_to_string t'));
  print_endline "should not occur in";
  print_endline (add_tab (type_to_string t))

let show_inexhaustive p =
  print_endline "The pattern matching is inexhaustive...";
  print_endline "Here is an example of a pattern that is not matched:";
  print_endline (add_tab (expr_to_string p))

let show_parse_error s = print_endline ("Parse Error: "^s)

let show_runtime_error s = print_endline ("Runtime Error: "^s)

let show_type_error e t =
  print_endline "The expression:";
  print_endline ("\t"^(expr_to_string e));
  print_endline "has the incorrect type...";
  print_endline "An expression of type:";
  print_endline (type_to_string t);
  print_endline "was expected."

let show_unbound x = print_endline ("The variable "^x^" is unbound")

let show_undefined e =
  print_endline "The expression:";
  print_endline ("\t"^(expr_to_string e));
  print_endline "has an undefined value at runtime."

let show_unify_error t t' =
  print_endline "An error occured during unification.";
  print_endline "The type:";
  print_endline (type_to_string t);
  print_endline "does not match the type";
  print_endline (type_to_string t')
