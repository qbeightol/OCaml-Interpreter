open Ast
open Exceptions
open Printer

let char_count  = ref (Char.code 'a')
let alpha_count = ref 0

let reset_type_vars () =
  char_count  := Char.code 'a';
  alpha_count := 0

let rec next_type_var () : typ =
  let helper () =
    let result =
      ((String.make 1 (Char.chr (!char_count)))^
          (if !alpha_count = 0
           then ""
           else string_of_int (!alpha_count))) in
    if !char_count >= Char.code 'z' then begin
      char_count := Char.code 'a';
      incr alpha_count end
    else incr char_count;
    result in
  TVar (helper ())

let type_of = function
  | ABool       (_, a)
  | AInt        (_, a)
  | ANil        a
  | AUnit       a
  | ACons       (_, _, a)
  | AIfThenElse (_, _, _, a)
  | ALetRec     (_, _, _, _, a)
  | ALet        (_, _, _, _, a)
  | ABinaryOp   (_,_,_,a)
  | AUnaryOp    (_,_,a)
  | AVar        (_, a)
  | AFun        (_, _, a)
  | AApp        (_, _, a)
  | AMatch      (_, _, a) -> a

let type_of_pattern = function
  | APConstant (_, t)
  | APVar      (_,t)
  | APCons     (_, _, t) -> t

module VarSet = Set.Make (struct
  type t = id
  let compare = Pervasives.compare
end)


let alpha_vary (t : typ) : typ =
  let open VarSet in
  let rec collect_tvars tvars = function
    | TBool
    | TUnit
    | TInt          -> tvars
    | TList t'      -> collect_tvars tvars t'
    | TVar  id      -> add id tvars
    | Arrow (t1,t2) -> begin
      union (collect_tvars tvars t1) (collect_tvars tvars t2)
    end in
  let rec fresh_vars n acc =
    if n = 0 then List.rev acc else fresh_vars (n-1) (next_type_var()::acc) in
  let ids = collect_tvars VarSet.empty t in
  let s   =
    List.combine (VarSet.elements ids) (fresh_vars (VarSet.cardinal ids) []) in
  let rec replace s = function
    | TVar x      -> List.assoc x s
    | TBool       -> TBool
    | TInt        -> TInt
    | TUnit       -> TUnit
    | TList u     -> TList (replace s u)
    | Arrow (u,v) -> Arrow (replace s u, replace s v) in
  replace s t



let rec t_vars (t: typ) : typ list =
  match t with
  | TBool
  | TUnit
  | TInt          -> []
  | TList t'      -> t_vars t'
  | TVar  id      -> [t]
  | Arrow (t1,t2) -> (t_vars t1)@(t_vars t2)


let rec remove_dups (ts: 'a list) : 'a list =
  match ts with
  | [] -> []
  | hd::tl -> hd::(remove_dups (List.filter (fun x -> x <> hd) tl))

let unique_t_vars (t:typ) : typ list =
  let non_unq_t_vars = t_vars t in
    remove_dups non_unq_t_vars

let rec shift_gen (ts: typ list) : substitution =
  match ts with
  | [] -> []
  | hd::tl -> begin
    match hd with
    | TVar ident -> (shift_gen tl) @ [(ident, next_type_var ())]
    | _ -> failwith "This shouldn't be reached (shift)"
  end



let rec annotate (e : expr) (fvs : (id, typ) Hashtbl.t) : aexpr =
  (fun () -> annotate_helper e fvs (Hashtbl.create 5)) (reset_type_vars ())

and annotate_helper (e: expr) (fvs : (id, typ) Hashtbl.t) 
(bv: (id, typ) Hashtbl.t) : aexpr = 
  match e with
  | Constant c -> annotate_const c
  | BinaryOp _ -> annotate_op fvs bv e
  | UnaryOp _ -> annotate_op fvs bv e
  | Var ident -> begin
    let tp = begin
      try 
        Hashtbl.find bv ident 
      with Not_found -> try
        let tp' = Hashtbl.find fvs ident in
          let unq_t_vars = unique_t_vars tp' in
            let sub = shift_gen unq_t_vars in
              Unify.apply sub tp'
      with Not_found -> 
        failwith ("unbound variable " ^ ident)
    end in
      AVar (ident, tp)
  end
  | Fun (ident, e) -> begin
    let n_t = next_type_var () in
      let sub_ae = 
        (fun () -> annotate_helper e fvs bv) (Hashtbl.add bv ident n_t) in
          AFun (ident, sub_ae, Arrow (n_t, type_of sub_ae))
  end
  | Cons (h,t) -> begin
    let h_ae = annotate_helper h fvs bv in
      ACons (h_ae, annotate_helper t fvs bv, TList (type_of h_ae))
  end
  | IfThenElse (b, t, e) -> 
    let b_ae = annotate_helper b fvs bv
    and t_ae = annotate_helper t fvs bv
    and e_ae = annotate_helper e fvs bv in
      AIfThenElse (b_ae, t_ae, e_ae, type_of t_ae)
  | Let (ident, e, e') -> begin
    let ae = annotate_helper e fvs bv 
    and hlpr = (fun () -> annotate_helper e' fvs bv) in 
      let ae' =  hlpr (Hashtbl.add bv ident (type_of ae)) in
        ALet (ident, type_of ae, ae, ae', type_of ae')
  end 
  | LetRec (ident, e, e') -> begin
    let hlpr = (fun () -> annotate_helper e fvs bv) in 
      let ae =  hlpr (Hashtbl.add bv ident (next_type_var ())) in
        let hlpr = (fun () -> annotate_helper e' fvs bv) in 
          let ae' =  hlpr (Hashtbl.add bv ident (type_of ae)) in
            ALet (ident, type_of ae, ae, ae', type_of ae')
  end 
  | App (f, e) ->
    let f_ae = annotate_helper f fvs bv
    and e_ae = annotate_helper e fvs bv in
      AApp (f_ae, e_ae, next_type_var ())
  | Match (e, ms) ->
    let e_ae = annotate_helper e fvs bv in
      let ms_a = match_helper e_ae ms fvs bv in
        let (_, hd_aexpr) = List.hd ms_a in 
          AMatch (e_ae, ms_a, type_of hd_aexpr)

and match_helper (ae: aexpr) (ms: (pattern * expr) list) (fvs:(id, typ) Hashtbl.t)
  (bv: (id, typ) Hashtbl.t) : (apattern * aexpr) list =
    match ms with 
    | (p, e)::t -> 
      let p_a = pat_to_apat ae p fvs bv
      and e_a = annotate_helper e fvs bv in
        (p_a, e_a)::(match_helper ae t fvs bv)
    | [] -> []

and pat_to_apat (ae: aexpr) (p: pattern) (fvs : (id, typ) Hashtbl.t) 
(bv: (id, typ) Hashtbl.t) : apattern =
  match p with
  | PConstant c -> APConstant (c, (type_of (annotate_const c)))
  | PVar ident -> begin
    (*Fix this so that i now binds ident to some type*)
      let new_tp = next_type_var () in
        let tp_hlp = (fun () -> new_tp) in
          APVar (ident, tp_hlp (Hashtbl.add bv ident (new_tp)))
  end
  | PCons (h,t) ->
    let ah = pat_to_apat ae h fvs bv
      and at = pat_to_apat ae t fvs bv in
        APCons (ah, at, TList (type_of_pattern ah))

and annotate_const (c: constant) : aexpr =
  match c with
  | Bool b -> ABool (b, TBool)
  | Int n -> AInt (n, TInt)
  | Nil -> ANil (TList (next_type_var () ))
  | Unit -> AUnit TUnit

and annotate_op fvs bv = function
  | BinaryOp (op,l,r) -> begin
    match op with
    | Plus
    | Minus
    | Mult
    | Divide
    | Mod    -> 
      let l_a = annotate_helper l fvs bv 
      and r_a = annotate_helper r fvs bv in
        ABinaryOp (op, l_a, r_a, TInt)
    | Gt
    | Lt
    | Ltq
    | Gtq
    | Eq
    | Neq
    | And
    | Or     ->
      let l_a = annotate_helper l fvs bv 
      and r_a = annotate_helper r fvs bv in
        ABinaryOp (op, l_a, r_a, TBool)
  end
  | UnaryOp (op,e)  -> AUnaryOp (op, annotate_helper e fvs bv, TBool)
  | _ as e -> begin
    let msg = Printf.sprintf
      "The expression: %s\n is not an operator, but an operator was expected."
      (expr_to_string e) in
    runtime_error msg
  end

let add_constraint  t  t' u = (t,t')::u
let add_constraints cs u    = List.fold_left (fun a c -> c::a) u cs

let rec apcons_collect (ap: apattern) (u: constr list) : (constr list) =
  match ap with
  | APCons (_, tl, tp) -> apcons_collect tl ((type_of_pattern tl, tp)::u)
  | _ -> u

let rec collect (aexprs: aexpr list) (u: constr list) : (constr list)= 
  match aexprs with 
  | hd::tl -> begin
    match hd with
    | ABool (_ , _)
    | AInt (_ , _)
    | AUnit _ 
    | ANil _
    | AVar (_, _) -> begin
      collect tl u
    end
    | ACons (h, t, tp) -> begin
      collect (h::t::tl) ((type_of t, tp)::u)
    end
    | ABinaryOp (_, _, _, _)
    | AUnaryOp (_, _, _) -> begin
      collect_operator_constraints tl u hd
    end
    | AIfThenElse (b, t, e, tp)-> begin
      collect (b::t::e::tl) ((type_of b, TBool)::(type_of t, type_of e)::u)
    end
    | ALet (ident, i_tp, e, e', tp') -> begin
      collect (e::e'::tl) ((i_tp, type_of e)::u)
    end 
    | ALetRec (ident, i_tp, e, e', tp') -> begin
      collect (e::e'::tl) ((i_tp, type_of e)::u)
    end 
    | AFun (ident, e, tp)-> begin
      collect (e::tl) u
    end
    | AApp (f, e, tp)-> begin
      collect (f::e::tl) ((type_of f, Arrow (type_of e, tp))::u)
    end
    | AMatch (a_e, a_ms, tp) -> 
      match collect_match_helper (type_of a_e) tp a_ms [] [] with
      | (new_u, new_aes) -> collect (new_aes@tl) (new_u@u)
  end
  | [] -> u 

(*traverses a list of match statements to generate a list of constraints on
patterns and annotated expressions it sees--specifically, it ensures that the
types of the patterns agree with the type of the match statements input 
expression, that any lists within a match statement have elements with uniform
typing, and that the output exressions of a match statement match the overall
expression's type. The function returns the constraints it has generated along
with any annotated expressions it found during the traversal*)
and collect_match_helper (ptp: typ) (etp: typ) (a_ms: (apattern * aexpr) list)
 (u: constr list) (aes: aexpr list) : (constr list * aexpr list) =
  match a_ms with
    | [] -> (u, aes)
    | h::t ->
      match h with
      | (APCons (ap1, ap2, tp), ae) -> begin
        let apc_u =  apcons_collect (APCons (ap1, ap2, tp)) [] in 
          let new_u = (type_of ae, etp)::(tp, ptp)::apc_u in
            collect_match_helper ptp etp t (new_u@u) (ae::aes)
      end 
      | (APConstant (_, tp), ae) -> begin
        collect_match_helper ptp etp t ((type_of ae, etp)::(tp, ptp)::u) (ae::aes)
      end
      | (APVar (_, tp), ae) -> begin
        collect_match_helper ptp etp t ((type_of ae, etp)::(tp, ptp)::u) (ae::aes)
      end

and collect_operator_constraints aexprs u = function
  | ABinaryOp (op,l,r,t) -> begin
    match op with
    | Plus
    | Minus
    | Mult
    | Divide
    | Mod    -> begin
      let (t1,t2) = (type_of l, type_of r) in
      collect (l::r::aexprs) ((t1, TInt)::(t2, TInt)::u)
    end
    | And
    | Or     -> begin
      let (t1,t2) = (type_of l, type_of r) in
      collect (l::r::aexprs) ((t1,TBool) :: (t2,TBool) :: u)
    end
    | Gt
    | Lt
    | Ltq
    | Gtq
    | Eq
    | Neq    -> begin
      let (t1,t2) = (type_of l, type_of r) in
      collect (l::r::aexprs) ((t1,t2) :: u)
    end
  end
  | AUnaryOp (op,ae,t) -> collect (ae::aexprs) ((type_of ae, t)::u)
  | _ as ae -> begin
    let msg = Printf.sprintf
      "The expression: %s\n is not an operator, but an operator was expected."
      (aexpr_to_string ae) in
    runtime_error msg
  end

let infer (e : expr) (fvs : (id, typ) Hashtbl.t) : typ =
  let ae = annotate e fvs in
  debug_print "annotated expression:";
  debug_print (aexpr_to_string ae);
  let cl = collect [ae] [] in
  debug_print "constraints:";
  debug_print (constraints_to_string cl);
  let s = Unify.unify cl in
  debug_print "substitution:";
  debug_print (subst_to_string s);
  let t = (Unify.apply s (type_of ae)) in
  debug_print "type before alpha varying:";
  debug_print (type_to_string t);
  reset_type_vars ();
  alpha_vary t
