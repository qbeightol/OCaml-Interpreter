(** Print functions for many of the data
    types outlined in the [Ast] module. These functions are intended
    to ease debugging. *)

(** The [debug] flag is used to enable all of the [debug_print]
    statements that appear throughout the codebase. *)
val debug : bool

(** [debug_print] is used to print a string based on the value of the
    [debug] flag. If [debug] is [true], then the string is printed,
    otherwise it is not. *)
val debug_print : string -> unit

(** [show_binop] is used to print out expressions involving binary
    operators. [show_binop to_string (x,y) op] converts the values [x]
    and [y] to strings and then the operator string is placed in
    between each of the values. The string that is printed is ["x op
    y"]. *)
val show_binop : ('a -> string) -> 'a * 'a -> string -> string

(** [opstring_of_binop op] takes a binary operator and returns the
    string representation of that operator. *)
val opstring_of_binop : Ast.binary_op -> string

(** [opstring_of_unop op] takes a unary operator and returns the
    string representation of that operator. *)
val opstring_of_unop : Ast.unary_op -> string

(** [expr_to_string e] prints the canonical string representation of a
    given 3110Caml expression. *)
val expr_to_string : Ast.expr -> Ast.id

(** [pattern_to_string p] prints the canonical string representation of a
    given 3110Caml pattern. *)
val pattern_to_string : Ast.pattern -> Ast.id

(** [string_of_list lst] returns the string representation of the {b
    3110Caml list}, [lst]. This function raises an exception when called
    on expressions which are not lists. *)
val string_of_list : Ast.expr -> Ast.id

(** [const_to_string] returns the string representation of a {b
    3110Caml} constant. Raises an exception when
    called on other types of expressions. *)
val const_to_string : Ast.constant -> Ast.id

(** [type_to_string] returns the canonical string representation of a
    3110Caml type. *)
val type_to_string : Ast.typ -> string

(** [apattern_to_string] returns the canonical string representation
    of an annotated 3110Caml pattern. *)
val apattern_to_string : Ast.apattern -> Ast.id

(** [aexpr_to_string] returns the canonical string representation
    of an annotated 3110Caml expression. *)
val aexpr_to_string : Ast.aexpr -> string

(** [val_to_string] returns the canonical string representation of a
    3110Caml value. *)
val val_to_string : Ast.value -> string

(** [constr_to_string] prints out the canonical string representation
    of a constraint for unification. *)
val constr_to_string : Ast.constr -> string

(** [constraints_to_string] prints a list of constraints. *)
val constraints_to_string : Ast.constr list -> string

(** [subst_to_string] prints out the canonical string representation
    of a substitution. *)
val subst_to_string : Ast.substitution -> string

(** [print to_string value] prints [to_string value] to standard
    output. *)
val print : ('a -> string) -> 'a -> unit

(** [print_list to_string values] prints each of the values in
    [values] on a new line to standard output. *)
val print_list : ('a -> string) -> 'a list -> unit

(** [print_type t] prints the type [t] to standard output. *)
val print_type : Ast.typ -> unit

(** [print_val v] prints the value [v] to standard output. *)
val print_val : Ast.value -> unit

(** [print_constraint c] prints the constraint [c] to standard
    output. *)
val print_constraint : Ast.constr -> unit

(** [print_substitution s] prints the substitution [s] to standard
    output. *)
val print_substitution : Ast.substitution -> unit
