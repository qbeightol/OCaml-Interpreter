(** This module describes all of the syntactic constructs of our
    subset of OCaml, 3110Caml. The collection of types in this module
    completely specifies the {{:
    http://en.wikipedia.org/wiki/Abstract_syntax_tree} abstract syntax
    tree} as well as the {{: http://en.wikipedia.org/wiki/Type_system}
    type system} for the 3110Caml programming language. *)

(** The [id] type is an alias for the built-in OCaml [string] type. It
    represents the valid identifiers in our language. *)
type id = string

(** The [IdMap] module represents a mapping from identifiers (variable
    names) to (OCaml) values. This module is used to create the {{:
    http://en.wikipedia.org/wiki/Symbol_table} symbol table} for the
    3110Caml interpreter. {! Map.S} comes from the OCaml [Map] module. *)
module IdMap : Map.S with type key = id

(** Constants are the simplest types of expressions in 3110Caml. They
    correspond to values which may not be altered during program
    execution. *)
type constant =
  Bool of bool
(** The [Bool] constructor represents the 3110Caml constants
    corresponding to [true] and [false]. *)
| Int of int
(** The [Int] constructor wraps OCaml integers and corresponds to an
    integer literal value in 3110Caml. *)
| Nil
(** [Nil] is the internal representation of the empty list [[]]. *)
| Unit
(** This is the constant corresponding the OCaml value [()], the only
    inhabitant of the [unit] type. *)

(** The [pattern] specifies the valid set of patterns that can occur
    within a [match] expression in the 3110Caml language. *)
type pattern =
PConstant of constant
(** [PConstant] patterns correspond to putting in constant values
    within pattern matching expressions in OCaml. An example of this
    patern would be {[ fun n -> match n with 0 -> 0 | n -> 1 ]} the
    first pattern in the match statement would parse to a [PConstant]
    whereas the second pattern would correspond to a [PVar] (described
    below). *)
| PVar of id
(** [PVar] patterns correspond to patterns which are variable
    identifiers. Note that all values match patterns of the [PVar] type.*)
| PCons of pattern * pattern
(** [PCons (p1,p2)] corresponds to the OCaml pattern
    [p1::p2]. This pattern is used to deconstruct values of the [TList]
    type. *)

(** The [unary_op] type outlines the valid unary operators in the
    3110Caml language. *)
type unary_op =
  Not
(** The [Not] constructor corresponds to the logical negation operator
    [not]. It is the only unary operator in 3110Caml. *)

(** The [binary_op] type is an enumeration of all the valid binary
    operators in the 3110Caml operators. We support most of the
    arithmetic, logical, and comparison operators that are valid in
    OCaml. Like in OCaml, 3110Caml comparison operators are
    polymorphic. *)
type binary_op =
  Plus
(** [Plus] is the representation of the [+] operator. *)
| Minus
(** [Minus] is the representation of the [-] operator. *)
| Mult
(** [Mult] is the representation of the [*] operator. *)
| Divide
(** [Divide] is the representation of the [/] operator. *)
| Mod
(** [Mod] is the remainder operator [mod]. *)
| And
(** [And] corresponds to logical conjuction operator [&&]. *)
| Or
(** [Or] corresponds to logical disjunction operator [||]. *)
| Eq
(** [Eq] represents the equality operator, [=]. *)
| Neq
(** [Eq] represents the negation of the equality operator, [<>]. *)
| Lt
(** [Lt] represents the 'less than' comparison operator, [<]. *)
| Ltq
(** [Ltq] represents the 'less than or equal' comparison operator, [<=]. *)
| Gt
(** [Gt] represents the 'greater than' comparison operator, [>]. *)
| Gtq
(** [Gtq] represents the 'greater than or equal' comparison operator, [>=]. *)

(** This type encodes all of the valid expressions in
    3110Caml. Evaluation of 3110Caml expressions happens according to the
    usual rules of the {{:
    http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-21.html#%_sec_3.2}
    environment model} as specified in {{:
    http://www.cs.cornell.edu/courses/cs3110/2012sp/lectures/lec13-env-model/lec13.html}
    the notes}. *)
type expr =
  Constant of constant
(** [Constant] expressions are just wrappers for values of type [constant]. *)
| BinaryOp of binary_op * expr * expr
(** The [BinaryOp] expressions are generic representations for
    expressions involving one of the binary operators of type
    [binop]. The value [BinaryOp (op,l,r)] represents the
    expression [l op r]. *)
| UnaryOp of unary_op * expr
(** [UnaryOp] expressions are the representations for expressions
    involving one of the unary operators specified in the type
    [unary_op]. A value of type [UnaryOp (op,e)] represents the
    expression [op e]. *)
| Var of id
(** The [Var] constructor is a wrapper for an identifier, [id], of a
    3110Caml variable. *)
| Fun of id * expr
(** The [Fun] constructor is the representation for anonymous
    functions in 3110Caml. A value [Fun (x,e)] represents the anonymous
    function [fun x -> e]. Note that all values of this type have
    exactly one "argument" corresponding the [id] in the
    constructor. Desugaring the multiple argument systactic sugar
    happens in the Parser. That is, if the user types [fun x y -> e],
    the parser will produce the [expr] {[ Fun(Var "x", Fun (Var"y",e)) ]}.
    Consult the [Parser] directory for details on how this process
    takes place. *)
| Cons of expr * expr
(** [Cons (h,t)] is the representation of the list [h::t]. It is the
    familiar [::] operator from OCaml. *)
| IfThenElse of expr * expr * expr
(** The [IfThenElse] constructor is the encoding of the [if-then-else]
    syntactic construct. Note that evaluation of the branch
    expressions is {{: http://en.wikipedia.org/wiki/Lazy_evaluation}
    lazy}. Thus, [IfThenElse (b,t,e)] corresponds to the statement {[
    if b then t else e ]} Moreover, if [b] evaluates to [true] then
    [e] is never evaluated and conversely if [b] is [false] then [t]
    is never evaluated. *)
| Let of id * expr * expr
(** [Let (x,e,e')] corresponds to the syntactic construct
    {[let x = e in e']}. Also note that desugaring the expressions
    of the form {[let f x = x]} into [Let (Var "f", Fun ("x", Var "x")]
    happens during parsing. *)
| LetRec of id * expr * expr
(** A [LetRec] expression is similar to [Let] but includes a
    fixpoint. That is, in an expression [LetRec ("x",e,e')] the
    expression [e] can refer to the variable [x]. *)
| App of expr * expr
(** [App] is the constructor corresponding to function
    application. That is, [App (f,e)] represents the application [f
    e]. Recall that OCaml, and likewise 3110Ml, is a {{:
    http://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_value}
    call by value} language. This means that the expression [e] should
    be evaluated fully before any substitutions take place. *)
| Match of expr * (pattern * expr) list
(** Pattern matching is implemented in 3110Caml via the [Match]
    constructor. The value [Match (e,[(p1,e1);...;(pn,en)])] corresponds
    to the syntactic construct
    {[
    match e with
      p1 -> e1
    | ...
    | pn -> en
    ]}
    in OCaml. *)

(** The [definition] type is used for [let] expressions that are input
    into the toplevel REPL loop. In particular, if the user inputs
    {[let x = e]} in the REPL, this is translated to the definition
    [(x,e)]. *)
type definition = id * expr

(** Directives are the commands that are available to the user during
    a REPL session. *)
type directive =
  Env
(** The [Env] constructor represents the command that means to print
    the list of current bindings in the environment. *)
| Reset
(** The [Reset] command is used to reset the list of bindings in the
    environment of the REPL. *)
| Quit
(** [Quit] exits the interpreter.*)
| Help
(** [Help] prints the list of available commands to the user. *)
| Use of string
(** The [Use] directive allows the user to load an ML file into the
    interpreter. *)

(** [toplevel_input] represents all of the entry points into the
    interpreter. These are all of the types of input that a user can
    put in the toplevel. *)
type toplevel_input =
  Definition of definition
(** [Definition]s represent [let] expressions at the top level. This
    is a {[let x = e]} expression in the toplevel. Note that [in] is
    not used. {[let x = e in e']} will be represented as an
    [Expression]. *)
| Expression of expr
(** [Expression]s are the 3110Caml expressions that are entered into
    the toplevel. These are the representations of the [expr] type. *)
| Directive of directive
(** [Directive]s are a wrapper for the [directive] type. They
    represent the REPL commands available to the user. *)

(** These are the set of possible output values for 3110Caml expressions. *)
type value =
  VUndef
(** [VUndef] is a value that is used internally to implement recursion
    in the interpreter. It is used as a placeholder for the recursive
    call, until the value can be determined. *)
| VNil
(** [VNil] is the representation for the 3110Caml value [[]], the empty
    list. *)
| VUnit
(**[VUnit] is the representation for the value [()], the only value of
   type [unit]. *)
| VBool of bool
(** [VBool]s are the representation for boolean values. *)
| VInt of int
(** [VInt] are wrappers for 3110Caml integers. *)
| VCons of value * value
(** [VCons] is the value for a non-empty list. *)
| VClosure of value ref IdMap.t * id * expr
(** The [VClosure] constructor is used to represent high-order
    functions. It contains a local environment, as well as an
    identifier representing the argument variable and an expression
    representing the body of the function. Thus, [VClosure
    (env,"x",e)] represents the expression [fun x -> e] evaluated in
    the environment [env]. *)

(** [typ] is our internal representation of valid 3110Caml types. *)
type typ =
  TBool
(** [TBool] represents the type [bool]. *)
| TInt
(** [TInt] represents the type [int]. *)
| TUnit
(** [TUnit] represents the type [unit]. *)
| TList of typ
(** [TList t] represents the type [t list].*)
| TVar of id
(** [TVar "a"] represents the type variable ['a]. It is used to
    implement polymorphism. *)
| Arrow of typ * typ
(** Arrow are our representations of function types. [Arrow (t,t')] is
    the type of a function [t -> t']. *)

(** The [environment] type represents our mapping from identifiers to
    types. Because we are implementing the {{:
    http://www.cs.cornell.edu/courses/cs3110/2012sp/lectures/lec13-env-model/lec13.html}
    environment model}, the values are of type [value ref]. *)
type environment = value ref IdMap.t

(** [constr]s represent type constraints during unification. The
    [constr]: [(t,t')] represents the type equation [t = t']. *)
type constr = typ * typ

(** [substitution] is our representation for {{:
    http://en.wikipedia.org/wiki/Unification_(computer_science)#Substitution}
    substitutions} used during unification. Each element of a
    substitution is of the form [("x",t)] where the type [t] is
    substituted for the identifier ["x"]. *)
type substitution = (id * typ) list

(** The [apattern] type is used to represent patterns that have
    additional type annotations. *)
type apattern =
  APConstant of constant * typ
(** [APConstants] are just the type annotated versions of [Constant] patterns. *)
| APVar of id * typ
(** [APVar] is the type annotated version of the [Var] pattern. *)
| APCons of apattern * apattern * typ
(** [APCons] represents the type annotated version of the [Cons] patterns. *)

(** The [aexpr] type is the result of type inference. An annotated
    expression is a wrapped expression that contains type
    information. *)
type aexpr =
  ABool of bool * typ
(** [ABools] are boolean expressions, as well as their
    corresponding types. *)
| AInt of int * typ
(** [AInt]s are integer expressions, as well as their corresponding
    types. *)
| ANil of typ
(** [ANil] is the [Nil] expression, as well as the corresponding type. *)
| AUnit of typ
(** [AUnit] is the representation of [Unit], as well as the annotated
    type. *)
| ACons of aexpr * aexpr * typ
(** [ACons] are [Cons] list values, where both the head and tail are
    annotated, as well as the type associated with the entire list. *)
| AIfThenElse of aexpr * aexpr * aexpr * typ
(** [AIfThenElse] are [IfThenElse] expressions, but the subexpressions
    are annotated. They represent expressions like {[if e : t then e'
    : t' else e'' : t'' : t''']} *)
| ALetRec of id * typ * aexpr * aexpr * typ
(** [ALetRec] are [LetRec] expressions, except that each of the
    subexpressions is an annotated expression. The identifier of the
    [LetRec] is also annotated. It represents {[let rec x : t = e : t'
    in e' : t'' : t''']}. *)
| ALet of id * typ * aexpr * aexpr * typ
(** [ALet] expressions are similar to [ALetRec], but the [rec] is
    removed. *)
| ABinaryOp of binary_op * aexpr * aexpr * typ
(** [ABinaryOp] represents a binary operator expression, but each of
    the operands is annotated. The full expression is
    annotated. [ABinaryOp (op,e,e')] would represent {[e : t op e' :
    t' : t'']}*)
| AUnaryOp of unary_op * aexpr * typ
(** [AUnaryOp] is similar to binary operators, except that they wrap
    unary operations. In this case {[AUnaryOp (op,e,t)]} represents
    [op (e : t) : t']*)
| AFun of id * aexpr * typ
(** [AFun] represents annotated functions. This represents annotated
    expressions is [(fun x -> e : t) : t']. *)
| AApp of aexpr * aexpr * typ
(** [AApp] expressions are annotated applications of the form [(e : t)
    (e' : t') : t''] *)
| AVar of id * typ
(** [AVar] expressions are variable identifiers of the form [x :
    t]. *)
| AMatch of aexpr * (apattern * aexpr) list * typ
(** [AMatch] are annotated match expressions. We use annotated
    patterns on the left hand side of each [match] clause. These
    represent values of the form
    {[
    match e : t with
    | p1 : p1t -> e1 : t1
    ...
    | pn : pnt -> en : tn
    ]}
*)
