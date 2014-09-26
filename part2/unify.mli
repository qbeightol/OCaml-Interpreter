(** [Unify] contains all of the machinery required to do
    unification. The unification algorithm outlined in this module is
    used for the final step of the type inference phase of the
    interpreter. The unification algorithm is outlined in {{:
    http://www.cs.cornell.edu/courses/cs3110/2012sp/lectures/lec27-type-inference/lec27.html}
    the lecture notes}. You may search the internet for a description
    of or psuedocode for unification. You are responsible for
    implementing unification in OCaml, but may use outside resources
    to help you better understand what the algorithm does and/or
    why it works. *)

(** The [occurs] function is used to figure out if a given identifier
    occurs within a given type. This check must be used in order to
    prevent cyclic type definitions. *)
val occurs : Ast.id -> Ast.typ -> bool

(** [subst t x t'] substitutes the type [t] for all occurences of the
    type variable [x] in type [t']. *)
val subst : Ast.typ -> Ast.id -> Ast.typ -> Ast.typ

(** [apply s t] applies the substitution [s] to the type [t],
    right to left. *)
val apply : Ast.substitution -> Ast.typ -> Ast.typ

(** The [unify] function takes a list of constraints and finds a
    substitution that satisfies all of the constraints.
    The solution output should be the most general
    unifier for the list of constraints: output a minimal
    singleton substitution set or assert that the input is
    unsolvable. *)
val unify : Ast.constr list -> Ast.substitution
