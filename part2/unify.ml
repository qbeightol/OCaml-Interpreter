open Ast
open Exceptions
open Printer

let rec occurs (x : id) (t : typ) : bool =
  match t with
    | TBool
    | TUnit
    | TInt         -> false
    | TList t      -> occurs x t
    | TVar y       -> x = y
    | Arrow (u, v) -> occurs x u || occurs x v

let rec subst (s : typ) (x : id) = function
  | TBool        -> TBool
  | TInt         -> TInt
  | TUnit        -> TUnit
  | TList t'     -> TList (subst s x t')
  | TVar y as t  -> if x = y then s else t
  | Arrow (u, v) -> Arrow (subst s x u, subst s x v)

let apply (s : substitution) (t : typ) : typ =
  List.fold_right (fun (x, e) -> subst e x) s t



(*Checks whether t is a variable and whether that variable occurs in t' 
  (assuming that t actually is a variable)*)
let check (t: typ) (t': typ) : bool = 
  match t with
  | TVar ident -> not (occurs ident t')
  | _ -> false

(*Applies a substitution to a list of constraints*)
let rec constraint_sub (c: constr list) (s: substitution) : constr list =
  match c with
  | (t1, t2)::tl -> (apply s t1, apply s t2)::(constraint_sub tl s)
  | [] -> []

let rec add_sub (ident: id) (tp: typ) (s: substitution) : substitution =
  (ident, apply s tp)::s


let rec unify (s : constr list) : substitution =
  match s with
  | [] -> []
  | (t, t')::s' -> begin
    if t = t' then 
      unify s'
    else if check t t' then  
      let ident = begin
        match t with 
        | TVar ident -> ident
        | _ -> "this case shouldn't be reached (unify, check t t')"
        end
      in add_sub ident t' (unify (constraint_sub s' [(ident, t')]))
    else if check t' t then  
      let ident = begin
        match t' with 
        | TVar ident -> ident
        | _ -> "this case shouldn't be reached (unify, check t' t)"
        end
      in add_sub ident t (unify (constraint_sub s' [(ident, t)]))
    else 
      match (t,t') with
      | (Arrow (t0, t1), Arrow (t0', t1')) -> unify ((t0, t0')::(t1,t1')::s')
      | (TList t0, TList t0') -> unify ((t0, t0')::s')
      | _ -> unify_error t t'
  end
