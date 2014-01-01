open Ast

type t = unit
let is_prefix ctx name =
  List.mem name [NDirect "Ada"]
let is_submark ctx name =
  false
let is_fname ctx name =
  false
let is_precedure ctx name =
  List.mem name [NDirect "Put_Line"]
let is_package ctx name =
  List.mem name [NSelectedComp(Prefix(NDirect "Ada"), "Text_Io")]
let is_libraryunit ctx name =
  List.mem name [NSelectedComp(Prefix(NDirect "Ada"), "Text_Io")]
