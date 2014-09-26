open Ast
open Exceptions
open Eval
open Infer
open Lexer
open Parser
open Printer
open Printf

let parse_toplevel s =
  toplevel_input token (Lexing.from_string s)

let parse_expression s =
  match parse_toplevel s with
  | Expression e -> e
  | Definition _ | Directive _ -> begin
    failwith "Parse expression did not return expression"
  end

let parse_definition s =
  match parse_toplevel s with
  | Definition (id,e) -> (id,e)
  | Expression _ | Directive _ -> begin
    failwith "Parse definition did not return definition"
  end

let eval_def id e env fvs =
  let t,v = infer e fvs, eval e env in
  printf "val %s : %s = %s\n" id (type_to_string t) (val_to_string v);
  (v,t)

let commands = [
  "Commands are:";
  "  #env       , #e -- prints the currently bound variables";
  "  #reset     , #r -- reset the environment";
  "  #use <file>, #u -- load a file into the interpreter";
  "  #help      , #h -- show the list of commands";
  "  #quit      , #q -- exit the interpreter"
]

let rec repl env fvs =
  print_string "zardoz # ";
  try
    match parse_toplevel (read_line ()) with
    | Definition (id,e) -> begin
      let v,t = eval_def id e env fvs in
      Hashtbl.replace fvs id t;
      repl (IdMap.add id (ref v) env) fvs
    end
    | Expression e -> begin
      let t,v = type_to_string (infer e fvs), val_to_string (eval e env) in
      printf "- : %s = %s\n" t v;
      repl env fvs
    end
    | Directive d -> begin
      match d with
      | Env      -> begin
        IdMap.iter (fun x v -> print_endline (x^" : "^(val_to_string !v))) env;
        print_endline "";
        repl env fvs
      end
      | Help     -> show_commands (); repl env fvs
      | Reset    -> repl IdMap.empty (Hashtbl.create 16)
      | Quit     -> print_string "Moriturus te saluto..."; exit 0
      | Use file -> handle_file file
    end
  with
  | CircularDefinition (t,t') -> show_circular t t'               ; repl env fvs
  | End_of_file               -> ()
  | Failure msg               -> print_endline ("Exception: "^msg); repl env fvs
  | InexhaustiveMatch e       -> show_inexhaustive e              ; repl env fvs
  | ParseError s              -> show_parse_error s               ; repl env fvs
  | RuntimeError s            -> show_runtime_error s             ; repl env fvs
  | TypeError (e,t)           -> show_type_error e t              ; repl env fvs
  | UnboundVariable s         -> show_unbound s                   ; repl env fvs
  | UndefinedValue e          -> show_undefined e                 ; repl env fvs
  | UnificationError (t,t')   -> show_unify_error t t'            ; repl env fvs
  | _                         -> begin
    print_endline "An unknown error occurred. Resetting...";
    repl IdMap.empty fvs
  end

and handle_file f =
  try
    let defns = from_file token (Lexing.from_channel (open_in f)) in
    let fvs = Hashtbl.create 16 in
    let add_def env (id, e) =
      let (v, t) = eval_def id e env fvs in
      Hashtbl.replace fvs id t;
      IdMap.add id (ref v) env in
    let env = List.fold_left add_def IdMap.empty defns in
    repl env fvs
  with
  | Failure msg -> print_endline msg; exit 1
  | _           -> begin
    print_endline "Error: Maybe you forgot to enclose the filename in quotes.";
    exit 1
  end

and show_commands () =
  List.iter print_endline commands
