open Util

type selector_name = string
type direct_name = string

type name = string
type subtype_mark = name

type plusminus = Plus | Minus
type aop = Add | Sub | BitAnd
type mop = Mult | Div | Mod | Rem
type pop = Pow
type eop = Eq | Neq | Lt | Lte | Gt | Gte

type numeric = unit

type aggregate = unit
and allocator = unit
and qualified_expr =
  | QExpr of subtype_mark * expression
  | QAggr of subtype_mark * aggregate
and primary =
  | PNum of numeric
  | PNull
  | PString of string
  | PAggretgate of aggregate
  | PName of name
  | PQualifedExpr of qualified_expr
  | PAllocator of allocator
  | PParen of expression
and factor =
  | FPow of primary * (pop * primary) list
  | FAbs of primary
  | FNot of primary
and term =
  | Term of factor * (mop * factor) list
and simple_expr =
  | SE of plusminus option * term * (aop * term) list
and range = unit
and relation =
  | RE  of simple_expr * (eop * simple_expr) list
  | RInRange of bool * simple_expr * range
  | RInSubMark of bool * simple_expr * subtype_mark
and expression =
  | EAnd of relation list
  | EOr of relation list
  | EXor of relation list

type param_assoc = selector_name option * expression
type statement =
(*TODO*)
  | StProcCall of name * param_assoc list option
(*TODO*)

type exc_handler = unit
type handled_statements =
  | HandledStatements of statement list * exc_handler list option
