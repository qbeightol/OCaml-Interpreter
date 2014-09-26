(** Contains the core functions to evaluate 3110Caml expressions. *)

(** [vcompare] is the analog to OCaml's [Pervasives.compare]. It
    compares two [Ast.value]s, [x,y] and outputs [1] if [x > y], [0]
    if [x=y] and [-1] if [x<y]. The comparison operators are with
    respect to the "natural" ordering of each type. *)
val vcompare : Ast.value -> Ast.value -> int

(** [concat env env'] concatenates the environments [env] and
    [env']. It outputs a new environment containing the bindings of both
    [env] and [env']. *)
val concat : Ast.environment -> Ast.environment -> Ast.environment

(** [pattern_match v p] takes the value [v] and the pattern [p] and
    outputs the following data: {ol {li A [bool] that indicates
    whether the [v] matches the pattern [p].} {li An environment that
    contains the bindings of the variables in the pattern that witness
    the match.}} *)
val pattern_match : Ast.value -> Ast.pattern -> bool * Ast.environment

(** [update x v env] updates the value of the identifier [x] to the
    value [v] in the environment [env]. *)
val update : Ast.id -> Ast.value -> Ast.environment -> unit

(** [eval] contains the main logic for evaluating 3110Caml
    expressions. The evaluation model that we use is the {{:
http://www.cs.cornell.edu/Courses/cs3110/2014sp/lectures/13/the-environment-model.html}
    environment model}. *)
val eval : Ast.expr -> Ast.environment -> Ast.value

(** [eval_arith] is a helper function used to evaluate purely
    arithmetic expressions in 3110Caml. The arguments are two arithmetic
    expressions, and an environment that has assignments to their
    respective free variables, and a function that is used to combine
    the two [Int] values of the expressions after evaluation. *)
val eval_arith :
  Ast.expr -> Ast.expr -> Ast.environment -> (int -> int -> int) -> Ast.value

(** [eval_bool] is similar to [eval_arith], but is used for boolean
    expressions. *)
val eval_bool :
  Ast.expr ->
  Ast.expr -> Ast.environment -> (bool -> bool -> bool) -> Ast.value

(** [eval_comp] is similar to [eval_arith], but is used for
    expressions involving the polymorphic comparison operators. *)
val eval_comp :
  Ast.expr ->
  Ast.expr -> Ast.environment -> (int -> int -> bool) -> Ast.value
