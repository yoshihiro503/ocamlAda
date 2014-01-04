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
    A.NDirect "Float";
    A.NDirect "Positive";
  ] |> ignore;
  List.map (fun n -> set ProcName n ctx) [
    A.NDirect "Put_Line"
  ] |> ignore;
  List.map (fun n -> set LibraryUnit n ctx) [
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Text_Io");
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Command_Line");
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Integer_Text_Io");
    A.NSelectedComp(A.Prefix(A.NDirect "Ada"), "Calendar");
    A.NSelectedComp(A.Prefix(A.NSelectedComp(A.Prefix(A.NDirect"Ada"),"Numerics")),
                                             
                    "Generic_Elementary_Functions");
  ] |> ignore;
  ctx

let init_size = 100
let create () : t =
  let ctx = Hashtbl.create init_size in
  init ctx
  

