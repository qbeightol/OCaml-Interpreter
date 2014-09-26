(** The [Repl] module is the front-end for the interpreter. [Repl]
    stands for {{: http://en.wikipedia.org/wiki/Read–eval–print_loop}
    Read-Eval-Print-Loop}. It is responsible for parsing user
    input, evaluating the resulting expression, and printing the
    result back to the terminal. *)

(** [parse_toplevel s] calls the [Parser] on the string [s] with entry
    point [toplevel_input] to the grammar. *)
val parse_toplevel : string -> Ast.toplevel_input

(** [parse_expression] parses the input string to an expression. *)
val parse_expression : string -> Ast.expr

(** [parse_definition] parses the input string to a definition. *)
val parse_definition : string -> Ast.id * Ast.expr

(** [commands] is the list of commands available to the user. *)
val commands : string list

(** [eval_def] is used to evaluate a toplevel definition. Thus,
    [eval_def id exp env fvs] evaluates the expression [exp] in the
    environment [env] with known free variables [fvs]. The resulting
    value is bound to the identifier [id] and is returned along with
    its type. *)
val eval_def :
  Ast.id ->
  Ast.expr ->
  Ast.environment -> (Ast.id, Ast.typ) Hashtbl.t -> Ast.value * Ast.typ

(** [repl env fvs] is the entry point for the REPL loop. It maintains
    a global environment for the interpreter as well as the set of
    known free variables. *)
val repl : Ast.environment -> (Ast.id, Ast.typ) Hashtbl.t -> unit

(** [handle_file] is used exlusively to handle the [Use <filename>]
    directive. [handle_file filename] interprets the contents of the
    file [filename] and adds the definitions to the environment of the
    REPL. *)
val handle_file : string -> unit
