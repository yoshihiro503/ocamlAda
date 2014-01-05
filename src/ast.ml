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

(** 3., 4. *)

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
  | NFunCall of function_name * param_assoc list option
and prefix = Prefix of name
and subtype_mark = SubtypeMark of name
and function_name = FunName of name
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
  | NFunCall (function_name, params) -> "NFunCall"
and sprefix (Prefix n) = sname n

type access_def = Access of subtype_mark

type discrim_spec =
  | DSpecSubtype of identifier list * subtype_mark * expression option
  | DSpecAccess of identifier list * access_def * expression option

type discrete_subtype_def =
  | DiscSubtyInd of subtype_ind
  | DiscSubtyRange of range

type comp_def = bool * subtype_ind

type array_type_def =
  | ArrTypeConst of discrete_subtype_def list * comp_def
  | ArrTypeUncon of subtype_mark list * comp_def

type record_def = (*TODO*)
  | NullRecord

type param_profile = unit(*TODO*)
type param_and_result_profile = unit(*TODO*)
type access_type_def =
  | AccTyObj of string option * subtype_ind
  | AccTySubprogProc of bool * param_profile
  | AccTySubprogFunc of bool * param_and_result_profile
type enum_lit_spec =
  | ELIdent of identifier
  | ELChar of char
type type_def =
  | TDefEnum of enum_lit_spec list
  | TDefInt_Range of simple_expr * simple_expr
  | TDefInt_Mod of expression
  | TDefReal_Float of expression * (simple_expr * simple_expr) option
  | TDefReal_OrdFixp of expression * (simple_expr * simple_expr) option
  | TDefReal_DecFixp of expression * (simple_expr * simple_expr) option
  | TDefArray of array_type_def
  | TDefRecord of bool option * bool * record_def
  | TDefAcc of access_type_def
  | TDefDerived of bool * subtype_ind * record_def option

(*============9*)
type task_def = unit(*TODO*)
type protected_def = unit(*TODO*)
(*============9*)

type full_type_decl =
  | FTDeclType of identifier * discrim_spec list option * type_def
  | FTDeclTask of identifier * discrim_spec list option * task_def option
  | FTDeclProtected of identifier * discrim_spec list option * protected_def

type type_decl =
  | TDeclFull of full_type_decl
  | TDeclIncomp(*TODO*)
  | TDeclPriv(*TODO*)
  | TDeclPrivExt(*TODO*)
let typename = function
  | TDeclFull (FTDeclType(id,_,_))
  | TDeclFull (FTDeclTask(id,_,_))
  | TDeclFull (FTDeclProtected(id,_,_)) -> id
  | TDeclIncomp -> failwith("Ast.typename: Incomp")
  | TDeclPriv -> failwith("Ast.typename: Priv")
  | TDeclPrivExt -> failwith("Ast.typename: PrivExt")

(** 5. Statements **)

type condition = expression

type statement_identifier = direct_name
type label = Label of statement_identifier
type variable_name = VarName of name

(** 6. Subprograms **)

type procedure_name = ProcName of name

type mode =
  | NoMode | In | Out | InOut
type param_type_spec =
  | PSMode of mode * subtype_mark
  | PSAccess of access_def
type param_spec = identifier list * param_type_spec * expression option
type formal = param_spec list

(*=======================10*)
type parent_unit_name = ParentUnit of name
(*=======================10*)

type designator = parent_unit_name option * ident_or_opsymb

type def_program_unit_name = parent_unit_name option * identifier

type defining_designator =
  | DdesUname of def_program_unit_name
  | DdesSymb of operator_symbol

type subprogram_spec =
  | SpecProc of def_program_unit_name * formal option
  | SpecFunc of defining_designator * formal option * subtype_mark


(** 7. Packages **)
type package_body = unit(*TODO*)

(** 8. Visibility Rules **)
type package_name = PackageName of name

type use_clause =
  | UCPackage of package_name list
  | UCType of subtype_mark list

(** 9. Tasks and Synchronization **)

type entry_name = EntryName of name

(** 10. Program Structure and Compilation Issues **)

type library_unit_name = LibraryUnit of name

type with_clause = library_unit_name list

type context_clause =
  | WithClause of with_clause
  | UseClause of use_clause

(** 11. Exceptions **)

(** 12. Generic Units *)

type dfp_unit_name = parent_unit_name option * identifier
let name_of_dfp_uname = function
  | (Some (ParentUnit name), id) -> NSelectedComp (Prefix name, id)
  | (None, id) -> NDirect id

type gen_assoc0 =
  | GAsExpr of expression
  | GAsVar of variable_name
  | GAsProc of procedure_name
  | GAsFunc of function_name
  | GAsEntry of entry_name
  | GAsSubtype of subtype_mark
  | GAsPackage of package_name
type gen_assoc = selector_name option * gen_assoc0

type generic_inst =
(*TODO*)
  | GInstPackage of dfp_unit_name * package_name * gen_assoc list option
  | GInstProc of dfp_unit_name * procedure_name * gen_assoc list option
  | GInstFunc of dfp_unit_name * function_name * gen_assoc list option

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

(*==================3*)
and basic_decl =
  | BDeclType of type_decl
  | BDeclSubtype of identifier * subtype_ind
  | BDeclSubprog of subprogram_spec
  | BDeclAbsSubprog of subprogram_spec
  | BDeclNumb of identifier list * expression
  | BDeclPackage of package_spec
  | BDeclGenInst of generic_inst
  | BDeclObj_1 of identifier list * bool * bool * subtype_ind * expression option
  | BDeclObj_2 of identifier list * bool * bool * array_type_def * expression option
  | BDeclObj_3 of identifier * task_def option
  | BDeclObj_4 of identifier * protected_def
  (*TODO*)
and basic_decl_item =
  | BDItemBasic of basic_decl
  | BDItemRepr of repr_clause
  | BDItemUse of use_clause
(*==================3*)
(*==================6*)
and package_spec = def_program_unit_name * basic_decl_item list * basic_decl_item list option * (parent_unit_name option * identifier) option
(*==================6*)

(** {2:c C山} *)

(** {2:d D山} *)

(** 3. **)
type proper_body =
  | ProperSubprog of subprogram_body
  | ProperPackage(*TODO*)
  | ProperTask(*TODO*)
  | ProperProtected(*TODO*)
and body =
  | BodyProper of proper_body
  | BodyStub(*TODO*)
and decl_part0 =
  | DeclBasic of basic_decl_item
  | DeclBody of body

(** 5. Statements **)

and seq_statements = (label list * statement_elem) list
and statement_elem =
(*TODO simple_statement *)
  | StNull
  | StAssign of variable_name * expression
  | StProcCall of pname * param_assoc list option
  | StReturn of expression option
  | StExit of name option * condition option
(*TODO compound_statement *)
  | StIf of (condition * seq_statements) list * seq_statements option
  | StLoop_While of (identifier option * condition * seq_statements * identifier option)
  | StLoop_For of (identifier option * identifier * bool * discrete_subtype_def * seq_statements * identifier option)
(** 6. Subprograms **)
and pname =
  | PNPrefix of prefix
  | PNProcName of procedure_name

(*==================11*)
and exc_handler = unit(*TODO*)
and handled_statements =
  | HandledStatements of seq_statements * exc_handler list option
(*==================11*)

(*==================6*)
and subprogram_body = {
    spec : subprogram_spec;
    declarative : decl_part0 list;
    statements : handled_statements;
    designator : designator option;
  }
(*==================6*)

(** 10. Program Structure and Compilation Issues **)
type library_item =
  | LibDecl of bool * unit(*TODO*)
  | LibBody_Subprog of subprogram_body
  | LibBody_Package of package_body
  | LibRenameDecl of bool * unit(*TODO*)

