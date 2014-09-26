open Ast
open Printf

let debug = true (*change back to false before submitting*)

let debug_print s =
  if debug then print_endline s

let show_binop f (ae1, ae2) op = (f ae1)^" "^op^" "^(f ae2)

let opstring_of_binop = function
  | Plus   -> "+"
  | Minus  -> "-"
  | Mult   -> "*"
  | Divide -> "/"
  | Mod    -> "mod"
  | Gt     -> ">"
  | Lt     -> "<"
  | Ltq    -> "<="
  | Gtq    -> ">="
  | Eq     -> "="
  | Neq    -> "<>"
  | And    -> "&&"
  | Or     -> "||"

let opstring_of_unop = function
  | Not   -> "not "

let rec expr_to_string = function
  | Constant x -> const_to_string x
  | Cons _ as l -> string_of_list l
  | IfThenElse (b,e,e') -> begin
    let sb,se,se' = expr_to_string b, expr_to_string e, expr_to_string e' in
    sprintf "if %s then %s else %s" sb se se'
  end
  | Let (x,e,e') -> begin
    let se,se' = expr_to_string e, expr_to_string e' in
    sprintf "let %s = %s in %s" x se se'
  end
  | LetRec (x,e,e') -> begin
    let se,se' = expr_to_string e, expr_to_string e' in
    sprintf "let rec %s = %s in %s" x se se'
  end
  | BinaryOp (op,l,r) -> begin
    let sl,sop,sr = expr_to_string l, opstring_of_binop op, expr_to_string r in
    sprintf "(%s %s %s)" sl sop sr
  end
  | UnaryOp (op,e) -> sprintf "%s%s" (opstring_of_unop op) (expr_to_string e)
  | Fun (x,e) -> sprintf "fun %s -> %s" x (expr_to_string e)
  | App (e,e') -> sprintf "(%s) (%s)" (expr_to_string e) (expr_to_string e')
  | Var x -> x
  | Match (e,pes) -> begin
    let se = expr_to_string e in
    let sps = List.fold_right (fun (p,e) a ->
      sprintf "%s -> %s" (pattern_to_string p) (expr_to_string e)^a) pes "" in
    sprintf "match %s with %s" se sps
  end

and pattern_to_string = function
  | PConstant c -> const_to_string c
  | PVar id -> id
  | PCons (p,p') -> begin
    sprintf "%s::%s" (pattern_to_string p) (pattern_to_string p')
  end

and string_of_list l =
  let rec string_of_list' = function
    | Cons (h, Constant Nil) -> expr_to_string h
    | Cons (h,t) -> expr_to_string h^"; "^string_of_list' t
    | _ -> failwith "string_of_list should only be used on non-empty lists" in
  "["^(string_of_list' l)^"]"

and const_to_string = function
  | Bool b -> string_of_bool b
  | Int n -> string_of_int n
  | Unit -> "()"
  | Nil -> "[]"

let rec type_to_string = function
  | TBool                        -> "bool"
  | TInt                         -> "int"
  | TVar x                       -> "'" ^ x
  | TUnit                        -> "unit"
  | TList (Arrow (u, v) as s)    -> sprintf "(%s) list" (type_to_string s)
  | TList t                      -> begin
    sprintf "%s list" (type_to_string t)
  end
  | Arrow (Arrow (u, v) as s, t) -> begin
    sprintf "(%s) -> %s" (type_to_string s) (type_to_string t)
  end
  | Arrow (s, t)                 -> begin
    sprintf "%s -> %s" (type_to_string s) (type_to_string t)
  end

let rec apattern_to_string = function
  | APConstant (c,t) -> begin
    match c with
    | Bool b -> "true"
    | Int i  -> string_of_int i
    | Nil    -> "[]"
    | Unit   -> "()"
  end
  | APVar (x,t) -> x
  | APCons (APCons (_, _, _) as p1, p2, _) -> begin
    sprintf "(%s) :: %s" (apattern_to_string p1) (apattern_to_string p2)
  end
  | APCons (p1, p2, t) -> begin
    sprintf "%s :: %s" (apattern_to_string p1) (apattern_to_string p2)
  end

let rec aexpr_to_string = function
  | ABool (b, t) -> sprintf "(bool : %s)" (type_to_string t)
  | AInt (i, t) -> sprintf "(%d : %s)" i (type_to_string t)
  | ANil t -> sprintf "([] : %s)" (type_to_string t)
  | AUnit t -> sprintf "(() : %s)" (type_to_string t)
  | ACons (ae1, ae2, t) -> begin
    let s1,s2,s3 = aexpr_to_string ae1, aexpr_to_string ae2, type_to_string t in
    sprintf "(%s :: %s : %s)" s1 s2 s3
  end
  | AIfThenElse (ae1, ae2, ae3, t) -> begin
    let s1 = aexpr_to_string ae1 in
    let s2 = aexpr_to_string ae2 in
    let s3 = aexpr_to_string ae3 in
    let st = type_to_string t in
    sprintf "(if %s then %s else %s : %s)" s1 s2 s3 st
  end
  | ALetRec (x, tx, ae1, ae2, t) -> begin
    let stx,st = type_to_string tx, type_to_string t in
    let s1,s2  = aexpr_to_string ae1, aexpr_to_string ae2 in
    sprintf "(let rec (%s : %s) = %s in %s : %s)" x stx s1 s2 st
  end
  | ALet (x, tx, ae1, ae2, t) -> begin
    let stx,st = type_to_string tx, type_to_string t in
    let s1,s2  = aexpr_to_string ae1, aexpr_to_string ae2 in
    sprintf "(let rec (%s : %s) = %s in %s : %s)" x stx s1 s2 st
  end
  | ABinaryOp (op,al,ar,t) -> begin
    let sop = opstring_of_binop op in
    let sl,sr = aexpr_to_string al, aexpr_to_string ar in
    sprintf "(%s %s %s : %s)" sl sop sr (type_to_string t)
  end
  | AUnaryOp (op,ae,t) -> begin
    let sop,se,st = opstring_of_unop op, aexpr_to_string ae, type_to_string t in
    sprintf "(%s%s : %s)" sop se st
  end
  | AFun (x, ae1, t) -> begin
    sprintf "(fun %s -> %s : %s)" x (aexpr_to_string ae1) (type_to_string t)
  end
  | AApp (ae1, ae2, t) -> begin
    let s1,s2,st = aexpr_to_string ae1, aexpr_to_string ae2, type_to_string t in
    sprintf "(%s %s : %s)" s1 s2 st
  end
  | AVar (x, t) -> sprintf "(%s : %s)" x (type_to_string t)
  | AMatch (ae1, ps_and_aes, t) -> begin
    let se = aexpr_to_string ae1 in
    let sp = string_of_pes_and_aes ps_and_aes in
    let st = type_to_string t in
    sprintf "(match %s with %s : %s)" se sp st
  end

and string_of_pes_and_aes = function
  | [] -> failwith "Parse Error: Parse for match case resulted in failure."
  | [(p, ae)] -> begin
    sprintf "%s -> %s" (apattern_to_string p) (aexpr_to_string ae)
  end
  | (p, ae)::t -> begin
    let sp,se = apattern_to_string p, aexpr_to_string ae in
    sprintf "%s -> %s | %s" sp se (string_of_pes_and_aes t)
  end

let rec val_to_string = function
  | VUndef               -> "undefined"
  | VBool b              -> string_of_bool b
  | VInt i               -> string_of_int i
  | VNil                 -> "[]"
  | VUnit                -> "()"
  | VCons (v1,v2)        -> sprintf "[%s]" (list_to_string v1 v2)
  | VClosure (env, x, e) -> "<fun>"

and list_to_string v1 = function
  | VNil            -> val_to_string v1
  | VCons (v1',v2') -> begin
    sprintf "%s; %s" (val_to_string v1) (list_to_string v1' v2')
  end
  | _               -> failwith "list_to_string: Error in typechecker."

let constr_to_string ((t1, t2) : constr) : string =
  (sprintf "%s = %s" (type_to_string t1) (type_to_string t2))

let add_constraint s c =
  s ^ "\n" ^ (constr_to_string c)

let constraints_to_string = function
  | [] -> "(none)"
  | c :: cs -> List.fold_left add_constraint (constr_to_string c) cs

let subst_to_string (subst : substitution) : string =
  let onesub_to_string (x, t) =
    sprintf "'%s ==> %s" x (type_to_string t) in
  let add_sub str sub =
    str ^ "\n" ^ (onesub_to_string sub) in
  match List.rev subst with
    | [] -> "(none)"
    | s :: ss -> List.fold_left add_sub (onesub_to_string s) ss

let print f x = print_endline (f x)
let print_list f = List.iter (print f)

let print_type         = print type_to_string
let print_val          = print val_to_string
let print_constraint   = print constr_to_string
let print_substitution = print subst_to_string
