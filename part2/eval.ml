open Ast
open Exceptions
open Printer

let rec vcompare v1 v2 = match v1,v2 with
  | VBool b1       , VBool b2        -> compare b1 b2
  | VInt  n1       , VInt n2         -> compare n1 n2
  | VNil           , VNil            -> 0
  | VCons _        , VNil            -> 1
  | VNil           , VCons _         -> -1
  | VCons (x1, xs1), VCons (x2, xs2) -> begin
    let c = compare x1 x2 in
    if c = 0 then vcompare xs1 xs2 else c
  end
  | _ -> begin
    let msg = Printf.sprintf
      "The comparison of the values %s and %s resulted in failure."
      (val_to_string v1)
      (val_to_string v2) in
    runtime_error msg
  end

let concat (env1 : environment) (env2 : environment) : environment =
  IdMap.fold (fun x v e -> IdMap.add x v e) env1 env2

let rec pattern_match (v : value) (p : pattern) : bool * environment =
  match p with
  | PConstant Bool b -> (v = VBool b, IdMap.empty)
  | PConstant Int n -> (v = VInt n, IdMap.empty)
  | PConstant Nil -> (v = VNil, IdMap.empty)
  | PConstant Unit -> (v = VUnit, IdMap.empty)
  | PVar ident -> (true, IdMap.add ident (ref v) IdMap.empty)
  | PCons (p1, p2) -> (match v with
    | VCons (v1, v2) -> 
      let (b1, e1) = pattern_match v1 p1
      and (b2, e2) = pattern_match v2 p2 in
      if (b1 && b2) then 
        (true, concat (snd (pattern_match v1 p1)) (snd (pattern_match v2 p2)))
      else (false, IdMap.empty)
    | _ -> (false, IdMap.empty))

let update (x : id) (v : value) (env : environment) : unit =
  try (IdMap.find x env) := v with Not_found -> ()



let look_up (x: id) (env: environment) : value = ! (IdMap.find x env)

let rec eval (e : expr) (env : environment) : value =
  match e with
  | Constant c -> eval_const c
  | BinaryOp (_,_,_) -> eval_operator env e
  | UnaryOp (_,_) -> eval_operator env e
  | Var ident -> look_up ident env
  | Fun (ident,e) -> VClosure (env, ident, e)
  | Cons (h,t) -> VCons (eval h env, eval t env)
  | IfThenElse (b,t,e) -> 
    (match (eval b env) with 
      | VBool b' -> eval (if b' then t else e) env
      | _ -> type_error b TBool)
    (* ^ double check the code above (it might not evaluate laziliy) *)
  | Let (ident, e, e') -> eval (App (Fun (ident, e'), e)) env
  | LetRec (ident, e, e') -> begin
    let new_env = (IdMap.add ident (ref VUndef) env) in
      let v' = eval e new_env in
        eval e' ((fun () -> new_env) (update ident v' new_env))
  end 
  | App (f, e) -> begin
    match (eval f env) with
    | VClosure (env_f, ident, e_f) -> let v' = (eval e env) in 
      (eval e_f (IdMap.add ident (ref v') env_f))
    | _ -> failwith ("You cannot apply "^(expr_to_string f)^"; it is not a function")
  end 
  | Match (e, ms) -> match_helper (eval e env) env ms

and match_helper (v: value) (env: environment)
(ms: (pattern * expr) list) : value =
    match ms with 
    | (p, e)::t -> let (b, env') = pattern_match v p in
        if b then eval e (concat env' env) else match_helper v env t
    | _ -> failwith "nonexhaustive pattern matching"

and eval_const (c: constant) : value =
  match c with
  | Bool b -> VBool b
  | Int n -> VInt n
  | Nil -> VNil
  | Unit -> VUnit

and eval_operator env = function
  | BinaryOp (op,e1,e2) -> begin
    match op with
    | Plus   -> eval_arith e1 e2 env (+)
    | Minus  -> eval_arith e1 e2 env (-)
    | Mult   -> eval_arith e1 e2 env ( * )
    | Divide -> eval_arith e1 e2 env (/)
    | Mod    -> eval_arith e1 e2 env (mod)
    | And    -> eval_bool  e1 e2 env (&&)
    | Or     -> eval_bool  e1 e2 env (||)
    | Eq     -> eval_comp e1 e2 env (=)
    | Neq    -> eval_comp e1 e2 env (<>)
    | Gt     -> eval_comp e1 e2 env (>)
    | Lt     -> eval_comp e1 e2 env (<)
    | Gtq    -> eval_comp e1 e2 env (>=)
    | Ltq    -> eval_comp e1 e2 env (<=)
  end
  | UnaryOp (op,e) -> 
    (*not is the only unary operator in 3110 Caml, so I'm only going to
      check whether e is a constant build from a Vbool--which is the only type
      compatable with not. There's no need to pattern match op for now*)
    (match e with
    | Constant (Bool b) -> VBool (not b)
    | _ -> type_error e TBool)
  | _ as e -> begin
    let msg = Printf.sprintf
      "The expression: %s\n is not an operator, but an operator was expected."
      (expr_to_string e) in
    runtime_error msg
  end

and eval_arith e e' env op = match eval e env, eval e' env with
  | VInt n1, VInt n2 -> VInt (op n1 n2)
  | VInt _, _ -> type_error e' TInt
  | _ -> type_error e TInt

and eval_bool e e' env op = match eval e env, eval e' env with
  | (VBool b1, VBool b2) -> VBool (op b1 b2)
  | VBool _, _ -> type_error e' TBool
  | _ -> type_error e TBool

and eval_comp e e' env op = VBool (op (vcompare (eval e env) (eval e' env)) 0)
