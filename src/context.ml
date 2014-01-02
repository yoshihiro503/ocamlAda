open Ast

type t = unit

let is_variable ctx name =
  List.mem name []

let is_prefix ctx name =
  List.mem name [NDirect "Ada"]
let is_submark ctx name =
  List.mem name [NDirect "Integer"]
let is_function ctx name =
  false
let is_procedure ctx name =
  List.mem name [NDirect "Put_Line"]
let is_package ctx name =
  List.mem name [NSelectedComp(Prefix(NDirect "Ada"), "Text_Io")]
let is_libraryunit ctx name =
  List.mem name [NSelectedComp(Prefix(NDirect "Ada"), "Text_Io")]

let is_parent_unit ctx name =
  List.mem name []

let is_entry ctx name =
  List.mem name []
