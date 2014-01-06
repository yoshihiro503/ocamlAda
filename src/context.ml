open Util

module A = Ast

type kind =
  | Prefix
  | Variable
  | Submark
  | ProcName
  | FuncName
  | Package
  | LibraryUnit
  | ParentUnit
  | EntryName
let skind = function
  | Prefix -> "Prefix"
  | Variable -> "Variable"
  | Submark -> "Submark"
  | ProcName -> "ProcName"
  | FuncName -> "FuncName"
  | Package -> "Package"
  | LibraryUnit -> "LibraryUnit"
  | ParentUnit -> "ParentUnit"
  | EntryName -> "EntryName"

type t = (A.name, kind) Hashtbl.t

let set : kind -> A.name -> t -> t = fun kind name ctx ->
  Hashtbl.add ctx name kind; ctx
let set_all : kind -> A.name list -> t -> t = fun kind names ctx0 ->
  List.fold_left (fun ctx name -> set kind name ctx) ctx0 names

let check_ : kind -> A.name -> t -> bool = fun kind name ctx ->
  if kind = Prefix then true else
  try Hashtbl.find ctx name = kind with
  | Not_found -> false
let check k n c =
  let b = check_ k n c in
  print_endline (!%"check %s: %s: %b" (skind k) (A.sname n) b);
  b


let init ctx =
  List.map (fun n -> set Prefix n ctx) [
    A.NDirect "Ada";
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Numerics");
    A.NDirect "Positive";
  ] |> ignore;
  List.map (fun n -> set Submark n ctx) [
    A.NDirect "Boolean";
    A.NDirect "Integer";
    A.NDirect "Character";
    A.NDirect "Float";
    A.NDirect "Natural";
    A.NDirect "Positive";
  ] |> ignore;
  List.map (fun n -> set ProcName n ctx) [
    A.NDirect "Put_Line";
    A.NDirect "Put";
  ] |> ignore;
  List.map (fun n -> set LibraryUnit n ctx) [
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Text_Io");
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Text_Io");
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Command_Line");
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Integer_Text_Io");
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Calendar");
    A.NSelectedComp(A.Prefix(A.NSelectedComp(A.Prefix(A.NDirect"Ada"),"Numerics")),
                                             
                    "Generic_Elementary_Functions");
    A.NDirect "Text_IO";
    A.NSelectedComp(A.Prefix(A.NDirect "Gnat"), "Io");
  ] |> ignore;
  ctx

let init_size = 100
let create () : t =
  let ctx = Hashtbl.create init_size in
  init ctx
  

let update_for_basic_decl_item bditem ctx =
  match bditem with
  | A.BDItemBasic(A.BDeclType type_decl) ->
      set Submark (A.NDirect (A.typename type_decl)) ctx
  | A.BDItemBasic(A.BDeclSubtype (id, ind)) -> ctx(*TODO*)
  | A.BDItemBasic(A.BDeclSubprog subprogram_spec) -> ctx(*TODO*)
  | A.BDItemBasic(A.BDeclAbsSubprog subprogram_spec) -> ctx(*TODO*)
  | A.BDItemBasic(A.BDeclNumb (ids, expr)) -> ctx(*TODO*)
  | A.BDItemBasic(A.BDeclPackage package_spec) -> ctx(*TODO*)
  | A.BDItemBasic(A.BDeclGenInst(A.GInstPackage(uname, pack_name, actuals))) ->
      set Package (A.name_of_dfp_uname uname) ctx
  | A.BDItemBasic(A.BDeclGenInst (A.GInstProc (uname, prod_name, actuals))) ->
      set ProcName (A.name_of_dfp_uname uname) ctx
  | A.BDItemBasic(A.BDeclGenInst (A.GInstFunc (uname, func_name, actuals))) ->
      set FuncName (A.name_of_dfp_uname uname) ctx
  | A.BDItemBasic(A.BDeclObj_1 (ids, aliesed, constant, ind, expr_opt)) ->
      ids |> List.map (fun n -> A.NDirect n)
      |> fun ns -> set_all Variable ns ctx
  | A.BDItemBasic(A.BDeclObj_2 (ids, aliesed, constant, arrty, expr_opt)) ->
      ids |> List.map (fun n -> A.NDirect n)
      |> fun ns -> set_all Variable ns ctx
  | A.BDItemBasic(A.BDeclObj_3 (ids, task_opt)) -> ctx(*TODO*)
  | A.BDItemBasic(A.BDeclObj_4 (ids, prot_opt)) -> ctx(*TODO*)
  (*TODO*)
  | A.BDItemRepr repr_clause -> ctx(*TODO*)
  | A.BDItemUse use_clause -> ctx(*TODO*)
let update_for_body_decl body ctx = ctx(*TODO*)
let update_for_decl = function
  | A.DeclBasic bditem -> update_for_basic_decl_item bditem
  | A.DeclBody body -> update_for_body_decl body

