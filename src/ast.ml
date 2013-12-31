open Util

type selector_name = string
type direct_name = string

type plusminus = Plus | Minus
type aop = Add | Sub | BitAnd
type mop = Mult | Div | Mod | Rem
type pop = Pow
type eop = Eq | Neq | Lt | Lte | Gt | Gte

type numeric = unit(*TODO*)

type name =
  | NDirect of direct_name
  | NChar of char
  | NExplicitDeref of name
  | NIndexedComp of name * expression list
  | NSlice of name * discrete_range
  | NSelectedComp of name * selector_name
  | NAttrRef of name * attribute
  | NTypeConv of subtype_mark * expression
  | NFunCall of name * param_assoc list option
and prefix = name
and subtype_mark = name
and attribute =
  | AIdent of string * expression option
  | AAccess | ADelta | ADigit
and comp_choice_list =
  | CCLOthers
  | CCLSels of selector_name list
and record_comp_assoc = comp_choice_list option * expression
and discrete_choice =
  | DcExpr of expression
  | DcRange of discrete_range
  | DcOthers
and array_aggregate =
  | ArrayAgPos of expression list
  | ArrayAgPosOthers of expression list * expression
  | ArrayAgNamed of (discrete_choice list * expression) list
and aggregate =
  | AgNullRecord
  | AgRecord of record_comp_assoc list
  | AgExtension of expression * record_comp_assoc list
  | AgArray of array_aggregate
and qualified_expr =
  | QExpr of subtype_mark * expression
  | QAggr of subtype_mark * aggregate
and allocator =
  | AlSubtype of subtype_ind | AlQual of qualified_expr
and primary =
  | PNum of numeric
  | PNull
  | PString of string
  | PAggregate of aggregate
  | PName of name
  | PQual of qualified_expr
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
and range =
  | RangeAttrRef of prefix * expression option
  | Range of simple_expr * simple_expr
and subtype_ind = subtype_mark * constraint_ option
and constraint_ =
  | CoRange of range
  | CoDigits of expression * range option
  | CoDelta of static_expr * range option
  | CoIndex of discrete_range list
  | CoDiscrim of (selector_name list option * expression) list
and discrete_range =
  | DrSubtype of subtype_ind | DrRange of range
and relation =
  | RE  of simple_expr * (eop * simple_expr) list
  | RInRange of bool * simple_expr * range
  | RInSubMark of bool * simple_expr * subtype_mark
and expression =
  | EAnd of relation list
  | EOr of relation list
  | EXor of relation list
and param_assoc = selector_name option * expression
and static_expr = expression

type statement =
(*TODO*)
  | StProcCall of name * param_assoc list option
(*TODO*)

type exc_handler = unit(*TODO*)
type handled_statements =
  | HandledStatements of statement list * exc_handler list option