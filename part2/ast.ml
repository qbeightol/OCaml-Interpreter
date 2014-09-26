type id = string

module IdMap = Map.Make (struct
  type t = id
  let compare = Pervasives.compare
end)

type constant =
  | Bool of bool
  | Int  of int
  | Nil
  | Unit

type pattern =
  | PConstant of constant
  | PVar      of id
  | PCons     of pattern * pattern

type unary_op =
  | Not

type binary_op =
  | Plus
  | Minus
  | Mult
  | Divide
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Ltq
  | Gt
  | Gtq

type expr =
  | Constant   of constant
  | BinaryOp   of binary_op * expr * expr
  | UnaryOp    of unary_op * expr
  | Var        of id
  | Fun        of id * expr
  | Cons       of expr * expr
  | IfThenElse of expr * expr * expr
  | Let        of id * expr * expr
  | LetRec     of id * expr * expr
  | App        of expr * expr
  | Match      of expr * (pattern * expr) list

type definition = id * expr

type directive = Env | Reset | Quit | Help | Use of string

type toplevel_input = Definition of definition
                    | Expression of expr
                    | Directive of directive

type value =
  | VUndef
  | VNil
  | VUnit
  | VBool    of bool
  | VInt     of int
  | VCons    of value * value
  | VClosure of ((value ref) IdMap.t) * id * expr

type typ =
  | TBool
  | TInt
  | TUnit
  | TList of typ
  | TVar  of id
  | Arrow of typ * typ

type environment = (value ref) IdMap.t

type constr = typ * typ

type substitution = (id * typ) list

type apattern =
  | APConstant of constant * typ
  | APVar      of id * typ
  | APCons     of apattern * apattern * typ

type aexpr =
  | ABool       of bool * typ
  | AInt        of int * typ
  | ANil        of typ
  | AUnit       of typ
  | ACons       of aexpr * aexpr * typ
  | AIfThenElse of aexpr * aexpr * aexpr * typ
  | ALetRec     of id * typ * aexpr * aexpr * typ
  | ALet        of id * typ * aexpr * aexpr * typ
  | ABinaryOp   of binary_op * aexpr * aexpr * typ
  | AUnaryOp    of unary_op * aexpr * typ
  | AFun        of id * aexpr * typ
  | AApp        of aexpr * aexpr * typ
  | AVar        of id * typ
  | AMatch      of aexpr * ((apattern * aexpr) list) * typ
