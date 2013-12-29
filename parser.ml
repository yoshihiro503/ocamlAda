open Util
open ParserMonad

let guard b = if b then return () else error "Guard"

let todo s : 'a ParserMonad.t = print_endline ("TODO: "^s); error s

module SpecialChar = struct
  let space = char ' '
  let iso_10646_BMP = todo "ISO 10646 BMP"
  let quotation = char '\'' <|>  char '\"'
end

let white =
  many begin
    char_when (function | ' ' | '\t' | '\r' | '\n' -> true | _ -> false)
  end

(**  Lexical Elements **)
let digit =
  char_when (function | '0'..'9' -> true | _ -> false)

    
  


let identifier_letter =
  char_when (function | 'a'..'z' | 'A'..'Z' -> true | _ -> false)

let identifier_letter2 =
  identifier_letter <|> char '_' <|> digit

let identifier =
  identifier_letter >>= fun c1 ->
  many identifier_letter2 >>= fun cs ->
  return @@ string_of_chars (c1::cs)

let graphic_character =
  identifier_letter <|> digit
  <|> SpecialChar.space <|> SpecialChar.iso_10646_BMP

let character_literal =
  char '\'' >>
  graphic_character >>= fun c ->
  char '\'' >>
  return @@ String.make 1 c

let string_literal =
  let string_element =
    (keyword "\"\"" >> return '\"')
    <|> graphic_character
  in
  SpecialChar.quotation >>= fun q1 ->
  many string_element >>= fun cs ->
  SpecialChar.quotation >>= fun q2 ->
  guard (q1 = q2) >>
  return @@ string_of_chars cs

let operator_symbol = string_literal

(** 4. Names and Expressions **)
let selector_name =
  identifier <|> character_literal <|> operator_symbol

let rec direct_name () =
  identifier
  <|> operator_symbol

and explicit_dereference () = todo "explicit_dereference"
and indexed_component () = todo "indexed_component"
and slice () = todo "slice"

and selected_component () =
  name0() >>= fun prefix ->
  char '.' >>
  selector_name >>= fun name ->
  return @@ prefix ^ "." ^ name

and attribute_reference () = todo "attribute_reference"
and type_conversion () = todo "type_conversion"
and character_literal () = todo "character_literal"

and name0 () =
  direct_name ()

and name () =
  direct_name ()
  <|> explicit_dereference ()
  <|> indexed_component ()
  <|> slice ()
  <|> selected_component ()
  <|> attribute_reference ()
  <|> type_conversion ()
(*  <|> function_call TODO 循環？*)
  <|> character_literal ()

(** Program Structure and Compilation Issues **)
let library_unit_name = name () >>= fun s -> print_endline ("lu_name: "^s); return s

let with_clause =
  keyword "with" >> white >>
  sep (keyword ",") library_unit_name >>= fun lu_names ->
  white >> keyword ";" >>
  return (lu_names)

let use_clause = todo "use_clause" >> return []

let context_clause =
  many (with_clause <|> use_clause)


let library_item : int ParserMonad.t = error "library_item"

let subunit = error "subunit"

let compilation_unit =
  context_clause >>= fun cc ->
  (library_item <|> subunit)

let compilation = many1 compilation_unit (*TODO many1*)

let run_ch ch =
  ParserMonad.run_ch library_unit_name ch
