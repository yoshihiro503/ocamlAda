(** SEE: http://cui.unige.ch/isi/bnf/Ada95/BNFindex.html **)
open Util
open ParserMonad

type 'a parser = 'a ParserMonad.t

let guard b = if b then return () else error "Guard"
let sep1 s p =
  p >>= fun x ->
  many (s >> p) >>= fun xs ->
  return (x :: xs)

let todo s : 'a ParserMonad.t = print_endline ("TODO: "^s); error s

module SpecialChar = struct
  let space = char ' '
  let iso_10646_BMP = todo "ISO 10646 BMP"
  let quotation = char '\'' <|>  char '\"'
end

let white1 =
  char_when (function | ' ' | '\t' | '\r' | '\n' -> true | _ -> false)
  >> return ()

let line_comment =
  keyword "--" >>
  many (char_when ((<>) '\n')) >>
  char '\n' >> return ()

let token p =
  p >>= fun x ->
  many (line_comment <|> white1) >>
  return x

let token_char c = token (char c)
let token_word w = token (keyword w)

(**  Lexical Elements **)
let digit =
  char_when (function | '0'..'9' -> true | _ -> false)

let identifier_letter =
  char_when (function | 'a'..'z' | 'A'..'Z' -> true | _ -> false)

let identifier =
  token begin
    identifier_letter >>= fun c1 ->
    many (identifier_letter <|> char '_' <|> digit) >>= fun cs ->
    return @@ string_of_chars (c1::cs)
  end

let graphic_character =
  identifier_letter <|> digit
  <|> SpecialChar.space <|> SpecialChar.iso_10646_BMP

let character_literal =
  token begin
    char '\'' >>
    graphic_character >>= fun c ->
    char '\'' >>
    return @@ String.make 1 c
  end

let string_literal =
  let string_element =
    (keyword "\"\"" >> return '\"')
    <|> graphic_character
  in
  token begin
    SpecialChar.quotation >>= fun q1 ->
    many string_element >>= fun cs ->
    SpecialChar.quotation >>= fun q2 ->
    guard (q1 = q2) >>
    return @@ string_of_chars cs
  end

let operator_symbol = string_literal

(**=====**)
let selector_name = (* sec. 4 *)
  identifier <|> character_literal <|> operator_symbol
let direct_name =
  identifier <|> operator_symbol
(**=====**)

(** 3. Declarations and Types **)
let defining_identifier = identifier

let rec subtype_indication () = todo "subtype_indication"

and subtype_mark () = name ()

and constraint_ () = todo "constraint"

and range_constraint () = todo "range_constraint"

and range () = todo "range"

and digits_constraint () = todo "digits_constraint"

and index_constraint() = todo "index_constraint"

and discrete_range () = todo "discrete_range"

and discriminant_constraint () = todo "discriminant_constraint"

and discriminant_association () = todo "discriminant_association"

and discrete_choice_list () = todo "discrete_choice_list"

and discrete_choice () = todo "discrete_choice"

(** 4. Names and Expressions **)
and name () =
  let name_next prevname =
    (*explicit_dereference*)
    (token_char '.' >> keyword "all")
    (*indexed_component*)
    <|> (token_char '(' >> sep1(token_char '.')(expression()) << token_char ')' >>= fun es -> return "indexed_component")
    (*slice*)
    (*selected_component*)
    <|> (token_char '.' >> selector_name)
    (*attribute_reference*)
    (*type_conversion*)
    (*function_call*)
  in
  let rec name_nexts prevname =
    (name_next prevname >>= fun n -> name_nexts n)
    <|> return prevname
  in
  direct_name >>= name_nexts
  <|> character_literal >>= name_nexts

and expression () : 'a parser = todo "expression"

(** 6. Subprograms **)

(**======from 10 **)
let parent_unit_name = name ()
(**======from 10 **)

let defining_program_unit_name =
  opt (parent_unit_name << token_char '.') >>= fun parent ->
  defining_identifier

let defining_designator : unit parser = todo "defining_designator"
let formal_part : unit parser = todo "formal_part"

let subprogram_specification =
  (token_word "procedure" >> defining_program_unit_name >>= fun dpuname ->
    opt formal_part >>= fun formal -> return dpuname)
  <|> (token_word "function" >> defining_designator >>= fun def ->
    opt formal_part >>= fun formal -> token_word "return" >> (subtype_mark()))

(**========from 2**)
let declarative_part : unit parser = todo "declarative_part"
(**========from 2**)

(**========from 11**)
let handled_sequence_of_statements : unit parser = todo "handled_sequence_of_statements"
(**========from 11**)

let designator: unit parser = todo "designator"

let subprogram_body =
  subprogram_specification >>= fun spec ->
  token_word "is" >>
  declarative_part >>= fun decl ->
  token_word "begin" >>
  handled_sequence_of_statements >>= fun stats ->
  token_word "end" >>
  opt designator >>= fun design ->
  token_char ';' >>
  return stats

(** 7. Packages **)

let package_body = todo "package_body"

(** 8. **)
let package_name = name() ^?"package_name" >>= fun s -> print_endline ("package_name: "^s); return s

let use_clause =
  token_word "use" >>
  begin
    sep1 (token_char ',') package_name
    <|> (token_word "type" >> sep1 (token_char ',') (subtype_mark ()))
  end << token_char ';'
  


(** 10. Program Structure and Compilation Issues **)
let with_clause =
  let library_unit_name =
    name () >>= fun s -> print_endline ("lu_name: "^s); return s
  in
  token_word "with" >>
  sep1 (token_char ',') library_unit_name >>= fun lu_names ->
  token_char ';' >>
  return (lu_names)

let context_clause =
  many (with_clause <|> use_clause)

let library_unit_declaration : unit parser = todo "library_unit_declaration"

let subunit = error "subunit"

let compilation =
  let compilation_unit =
    let library_item =
      let library_unit_declaration:unit parser = todo "library_unti_declaration" in
      let library_unit_body =
        subprogram_body <|> package_body
      in
      let library_unit_renaming_declaration =
        todo "library_unit_renaming_declaration"
      in
      (opt (token_word "private") >>= fun private_ -> library_unit_declaration)
      <|> library_unit_body
      <|> (opt (token_word "private") >>= fun private_ -> library_unit_renaming_declaration)
    in
    context_clause >>= fun cc ->
    (library_item <|> subunit)
  in
  many compilation_unit

let run_ch ch =
  ParserMonad.run_ch (compilation) ch
