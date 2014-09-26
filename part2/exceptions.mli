(** Wrapper functions describing the different errors which may
    occur during the interpretation of a 3110Caml program.
    You should use these exception functions rather than the built-in
    [failwith] construct so that the error messages printed by the
    REPL are more helpful during debugging. *)

(** [CircularDefinition] exceptions are used to indicate that the
    inferred type of an expression is circular. This is typically thrown
    during unification. *)
exception CircularDefinition of Ast.typ * Ast.typ

(** The [InexhaustiveMatch] exception should be thrown when the user
    does not input enough patterns to match the given input value in the
    [match] case. *)
exception InexhaustiveMatch of Ast.expr

(** [ParseError]s are thrown when the user enters syntactically
    malformed expression. *)
exception ParseError of string

(** [RuntimeError]s are thrown when something goes wrong internal to
    the interpreter. These should be used when the error is the fault
    of the interpreter and not the user. *)
exception RuntimeError of string

(** [TypeError]s get thrown during semantic analysis when the types of
    the input expressions do not match up correctly. *)
exception TypeError of Ast.expr * Ast.typ

(** [UnboundVariable] exceptions are thrown during evaluation when a
    variable that has no binding in the current environment is
    accessed. *)
exception UnboundVariable of string

(** [UndefinedValue] exceptions are used to indicate that an
    expression has evaluated to the internal value [VUndef]. *)
exception UndefinedValue of Ast.expr

(** [UnificationErrors] are thrown during type unification when the
    user has input an expression whose annotated counterpart generated
    unsatisfiable constraints. *)
exception UnificationError of Ast.typ * Ast.typ

(** [circular] is a helper function that is used to throw the
    [CircularDefinition] exception. *)
val circular : Ast.typ -> Ast.typ -> 'a

(** [inexhaustive] is used to raise the [InexhaustiveMatch]
    exception. *)
val inexhaustive : Ast.expr -> 'a

(** [parse_error] is used to raise the [ParseError] exception. *)
val parse_error : string -> 'a

(** [runtime_error] is used to raise the [RuntimeError] exception. *)
val runtime_error : string -> 'a

(** [type_error] is used to raise the [TypeError] exception. *)
val type_error : Ast.expr -> Ast.typ -> 'a

(** [unify_error] is used to raise the [UnificationError] exception. *)
val unify_error : Ast.typ -> Ast.typ -> 'a

(** [unbound_var] is used to raise the [UnboundVariable] exception. *)
val unbound_var : string -> 'a

(** [undefined] is used to raise the [UndefinedValue] exception. *)
val undefined : Ast.expr -> 'a

(** [show_circular] is used to print out a helpful error message for
    circular types. This is used by the REPL when a
    [CircularDefinition] exception is raised. *)
val show_circular : Ast.typ -> Ast.typ -> unit

(** [show_inexhaustive] is used to print out a helpful error message
    for inexhaustive match cases. This is used by the REPL when a
    [InexhaustiveMatch] exception is raised. *)
val show_inexhaustive : Ast.expr -> unit

(** [show_parse_error] is used to print out a helpful error message
    for parse errors. This is used by the REPL when a
    [ParseError] exception is raised. *)
val show_parse_error : string -> unit

(** [show_runtime_error] is used to print out a helpful error message
    for runtime errors. This is used by the REPL when a [RuntimeError]
    exception is raised. *)
val show_runtime_error : string -> unit

(** [show_type_error] is used to print out a helpful error message for
    type errors. This is used by the REPL when a [TypeError] exception
    is raised. *)
val show_type_error : Ast.expr -> Ast.typ -> unit

(** [show_unbound] is used to print out a helpful error message for
    unbound variable exceptions. This is used by the REPL when an
    [UnboundVariable] exception is raised. *)
val show_unbound : string -> unit

(** [show_undefined] is used to print out a helpful error message for
    undefined value exceptions. This is used by the REPL when an
    [UndefinedValue] exception is raised. *)
val show_undefined : Ast.expr -> unit

(** [show_unify_error] is used to print out a helpful error message
    for unification errors. This is used by the REPL when
    [UnificationError] exceptions are raised. *)
val show_unify_error : Ast.typ -> Ast.typ -> unit
