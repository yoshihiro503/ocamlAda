(** SEE: http://cui.unige.ch/isi/bnf/Ada95/BNFindex.html **)
open Util
open ParserMonad

type 'a parser = 'a ParserMonad.t

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
  identifier <|> operator_symbol

and name () =
  direct_name () >>= name_nexts
  <|> character_literal >>= name_nexts

and name_nexts prevname =
  (name_next prevname >>= fun n -> name_nexts n)
  <|> return prevname

and name_next prevname =
  (*explicit_dereference*)
  (char '.' >> keyword "all")
  (*indexed_component*)
  <|> (char '(' >> sep(char '.')(expression()) << char ')' >>= fun es -> return "indexed_component")
  (*slice*)
  (*selected_component*)
  <|> (char '.' >> selector_name)
  (*attribute_reference*)
  (*type_conversion*)
  (*function_call*)

and expression () : 'a parser = todo "expression"

(** Program Structure and Compilation Issues **)


let with_clause =
  let library_unit_name =
    name () >>= fun s -> print_endline ("lu_name: "^s); return s
  in
  keyword "with" >> white >>
  sep (keyword ",") library_unit_name >>= fun lu_names ->
  white >> keyword ";" >>
  return (lu_names)

let use_clause = todo "use_clause"

let context_clause =
  many (with_clause <|> use_clause)


let library_item : unit parser = error "library_item"

let subunit = error "subunit"

let compilation =
  let compilation_unit =
    context_clause >>= fun cc ->
    (library_item <|> subunit)
  in
  many compilation_unit

let run_ch ch =
  ParserMonad.run_ch (with_clause) ch
