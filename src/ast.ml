open Util

(** 2. Lexical Elements **)
type plusminus = Plus | Minus

type intpart = string
type fracpart = string
type exponent = plusminus option * string
type base = string
type numeric =
  | NumDecimal of intpart * fracpart option * exponent option
  | NumBased of base * intpart * fracpart option * exponent option

type declarative_elem = unit(*TODO*)

(** 4. Names and Expressions **)

type selector_name = string
type direct_name = string
type identifier = string
type operator_symbol = string
type ident_or_opsymb = string

type aop = Add | Sub | BitAnd
type mop = Mult | Div | Mod | Rem
type pop = Pow
type eop = Eq | Neq | Lt | Lte | Gt | Gte

type name =
  | NDirect of direct_name
  | NChar of char
  | NExplicitDeref of name
  | NIndexedComp of prefix * expression list
  | NSlice of prefix * discrete_range
  | NSelectedComp of prefix * selector_name
  | NAttrRef of prefix * attribute
  | NTypeConv of subtype_mark * expression
  | NFunCall of fname * param_assoc list option
and prefix = Prefix of name
and subtype_mark = SubtypeMark of name
and fname = FName of name
and attribute =
  | AIdent of identifier * expression option
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

let rec sname = function
  | NDirect dn -> dn
  | NChar c -> !%"'%c'" c
  | NExplicitDeref name -> !%"ExpDref(%s)" (sname name)
  | NIndexedComp (prefix, es) -> !%"NIdxCp(%s,[..])" (sprefix prefix)
  | NSlice (prefix, discrete_range) -> "NSlice"
  | NSelectedComp (prefix, selector_name) -> (!%"NSelComp(%s, %s)" (sprefix prefix) selector_name)
  | NAttrRef (prefix, attribute) -> "NAttrRef"
  | NTypeConv (subtype_mark, expression) -> "NTypeConv"
  | NFunCall (fname, params) -> "NFunCall"
and sprefix (Prefix n) = sname n

(** 5. Statements **)
type condition = expression

type statement_identifier = direct_name
type label = Label of statement_identifier
type var_name = name

type seq_statements = (label list * statement_elem) list
and statement_elem =
(*TODO simple_statement *)
  | StNull
  | StAssign of var_name * expression
  | StProcCall of pname * param_assoc list option
  | StReturn of expression option
(*TODO compound_statement *)
  | StIf of (condition * seq_statements) list * seq_statements option
(** 6. Subprograms **)
and proc_name = ProcName of name
and pname =
  | PNPrefix of prefix
  | PNProcName of proc_name

type mode =
  | NoMode | In | Out | InOut
type param_type_spec =
  | PSMode of mode * subtype_mark
  | PSAccess of subtype_mark
type param_spec = identifier list * param_type_spec * expression option
type formal = param_spec list

(*==================10*)
type parent_unit_name = ParentUnit of name
(*==================10*)

type designator = parent_unit_name option * ident_or_opsymb

type def_program_unit_name = parent_unit_name option * identifier

type defining_designator =
  | DdesUname of def_program_unit_name
  | DdesSymb of operator_symbol

type subprogram_spec =
  | SpecProc of def_program_unit_name * formal option
  | SpecFunc of defining_designator * formal option * subtype_mark

(*==================11*)
type exc_handler = unit(*TODO*)
type handled_statements =
  | HandledStatements of seq_statements * exc_handler list option
(*==================11*)

type subprogram_body = {
    spec : subprogram_spec;
    declarative : declarative_elem list;
    statements : handled_statements;
    designator : designator option;
  }

(** 7. Packages **)
type package_body = unit(*TODO*)

(** 8. Visibility Rules **)
type package_name = PackageName of name

type use_clause =
  | UCPackage of package_name list
  | UCType of subtype_mark list

(** 9. Tasks and Synchronization **)
(** 10. Program Structure and Compilation Issues **)

type library_unit_name = LibraryUnit of name

type with_clause = library_unit_name list

type context_clause =
  | WithClause of with_clause
  | UseClause of use_clause

type library_item =
  | LibDecl of bool * unit(*TODO*)
  | LibBody_Subprog of subprogram_body
  | LibBody_Package of package_body
  | LibRenameDecl of bool * unit(*TODO*)

(** 11. Exceptions **)

(** 13.Representation Clauses and Implementation-Dependent Features *)

type local_name =
  | LocalDirect of direct_name
  | LocalAttr of direct_name * attribute
  | LocalLib of library_unit_name

type first_subtype_local_name = direct_name
type enum_aggregate = array_aggregate
type mod_clause = expression
type bit = simple_expr
type comp_clause = local_name * expression * bit * bit
type repr_clause =
  | ReprAttr of local_name * attribute * expression
  | ReprAt of direct_name * expression
  | ReprEnum of first_subtype_local_name * enum_aggregate
  | ReprRecord of first_subtype_local_name * mod_clause option * comp_clause list
