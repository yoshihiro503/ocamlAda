(** SEE: http://cui.unige.ch/isi/bnf/Ada95/BNFindex.html **)
open Util
open ParserMonad
open Ast

let is_some = function Some _ -> true | None -> false

type 'a parser = 'a ParserMonad.t

let map f p = p >>= fun x -> return @@ f x
let guard b = if b then return () else error "Guard"
let sep1 s p =
  p >>= fun x ->
  many (s >> p) >>= fun xs ->
  return (x :: xs)
let (>*<) p1 p2 = p1 >>= fun x -> p2 >>= fun y -> return (x,y)

let todo s : 'a ParserMonad.t = print_endline ("TODO: "^s); error s

module SpecialChar = struct
  let space = char ' '
  let iso_10646_BMP = char_when ((<>) '\"')
  let quotation = char '\"'
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
    graphic_character
    << char '\''
  end

let string_literal =
  let string_element =
    (keyword "\"\"" >> return '\"')
    <|> graphic_character
  in
  token begin
    SpecialChar.quotation >>
    many string_element >>= fun cs ->
    SpecialChar.quotation >>
    return @@ string_of_chars cs
  end

let operator_symbol = string_literal

let numeric_literal = todo "numeric_literal"
(**=====**)
let selector_name = (* sec. 4 *)
  identifier <|> (character_literal |> map (String.make 1)) <|> operator_symbol
let direct_name =
  identifier <|> operator_symbol
(**=====**)

(** 3. Declarations and Types **)
let defining_identifier = identifier

let rec subtype_indication () = todo "subtype_indication"

and subtype_mark () = name()

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
  let name_next prefix =
    (*explicit_dereference*)
    (token_char '.' >> keyword "all" >> return @@ NExplicitDeref prefix)
    (*indexed_component*)
    <|> (token_char '(' >> sep1(token_char '.')(expression()) << token_char ')' >>= fun es -> return @@ NIndexedComp(prefix, es))
    (*slice*)
    <|> (token_char '(' >> discrete_range() << token_char ')' >>= fun dr ->
         return @@ NSlice(prefix, dr))
    (*selected_component*)
    <|> (token_char '.' >> selector_name >>= fun sel ->
         return @@ NSelectedComp(prefix, sel))
    (*attribute_reference*)
    <|> (token_char '\'' >> attribute_designator() >>= fun attr ->
         return @@ NAttrRef(prefix, attr))
    (*type_conversion*)
    <|> (token_char '(' >> expression() >>= fun expr ->
         return @@ NTypeConv (prefix, expr))
    (*function_call*) (*仕様では(params)がない場合も規定しているが、実際それはnameと区別できない。*)
    <|> (actual_parameter_part() >>= fun params ->
         return @@ NFunCall (prefix, Some params))
  in
  let rec name_nexts prevname =
    (name_next prevname >>= fun n -> name_nexts n)
    <|> return prevname
  in
  (direct_name >>= fun d -> name_nexts (NDirect d))
  <|> (character_literal >>= fun c -> name_nexts (NChar c))

and attribute_designator () : attribute parser =
  (token_word "Access" >> return AAccess)
  <|> (token_word "Delta" >> return ADelta)
  <|> (token_word "Digit" >> return ADigit)
  <|> (identifier >>= fun ident ->
   opt (token_char '(' >> expression() << token_char ')') >>= fun e ->
   return @@ AIdent(ident, e))
    

and primary () =
  (numeric_literal >>= fun num -> return @@ PNum num)
  <|> (token_word "null" >> return @@ PNull)
  <|> (string_literal >>= fun s -> return @@ PString s)
(*  <|> TODO: aggregate *)
  <|> (name() >>= fun n -> return @@ PName n)
(*  <|> TODO: qual expr *)
(*  <|> TODO: allocator *)
  <|> (token_char '(' >> expression() << token_char ')' >>= fun e ->
       return @@ PParen e)

and factor () =
  let pop = token_word "**" >> return Pow in
  (token_word "abs" >> primary() >>= fun p -> return @@ FAbs p)
  <|> (token_word "not" >> primary() >>= fun p -> return @@ FNot p)
  <|> (primary() >>= fun p -> many (pop >*< primary()) >>= fun pps ->
       return @@ FPow(p, pps))

and term () =
  let mop = (token_char '*' >> return Mult) <|> (token_char '/' >> return Div) (*TODO*)
  in
  factor() >>= fun f -> many (mop >*< factor()) >>= fun mfs ->
  return @@ Term(f, mfs)

and simple_expression () =
  let plusminus =
    (token_char '+' >> return Plus) <|> (token_char '-' >> return Minus)
  in
  let aop =
    (token_char '+' >> return Add) <|> (token_char '-' >> return Sub)
    <|> (token_char '&' >> return BitAnd)
  in
  opt plusminus >>= fun pm -> term() >>= fun t -> many (aop >*< term()) >>= fun ats -> return @@ SE(pm, t, ats)
    
and relation () =
  let aop =
    (token_char '=' >> return Eq) <|> (token_word "/=" >> return Neq)
    <|> (token_char '<' >> return Lt) (*TODO*)
  in
  (simple_expression() >>= fun se -> opt (token_word "not") >>= fun not ->
  token_word "in" >> begin
    (range() >>= fun range -> return @@ RInRange(is_some not, se, range))
    <|> (subtype_mark() >>= fun smark -> return @@ RInSubMark(is_some not, se, smark))
  end)
  <|> (simple_expression() >>= fun se ->
       many (aop >*< simple_expression()) >>= fun ses -> return @@ RE(se, ses))

and expression () : expression parser =
  (sep1 (token_word "and") (relation ()) >>= fun rs -> return @@ EAnd rs)
  <|> (sep1 (token_word "and" >> token_word "then") (relation ()) >>= fun rs ->
       return @@ EAnd rs)
  <|> (sep1 (token_word "or") (relation ()) >>= fun rs -> return @@ EOr rs)
  <|> (sep1 (token_word "or" >> token_word "else") (relation ()) >>= fun rs ->
       return @@ EOr rs)
  <|> (sep1 (token_word "xor") (relation ()) >>= fun rs -> return @@ EXor rs)

(**=============6*)
and parameter_assoc () =
  opt (selector_name << token_word "=>") >>= fun sel ->
  expression() >>= fun expr ->
  return (sel, expr)
and actual_parameter_part () =
  token_char '(' >>
  sep1 (token_char ',') (parameter_assoc())
  << token_char ')'
(**=============6*)
  
(** 5. Statements **)

let simple_statement =
  (* TODO null *)
  (* TODO assignment *)
  (* TODO exit *)
  (* TODO goto *)
  (name() >>= fun n ->
   opt (actual_parameter_part()) << token_char ';' >>= fun ps ->
   return @@ StProcCall(n, ps))
  (* TODO return *)
  (* TODO entry *)
  (* TODO requeue *)
  (* TODO delay *)
  (* TODO abort *)
  (* TODO raise *)
  (* TODO code *)

let compound_statement = todo "compound_statement"
let label : unit parser = todo "label"
let statement =
  many label >>= fun ls ->
  (simple_statement <|> compound_statement)
let sequence_of_statements =
  many1 statement

(** 6. Subprograms **)

(**======from 10 **)
let parent_unit_name = name()
(**======from 10 **)

let defining_program_unit_name =
  opt (parent_unit_name << token_char '.') >>= fun parent ->
  defining_identifier

let defining_designator : unit parser = todo "defining_designator"
let formal_part : unit parser = todo "formal_part"

let subprogram_specification =
  (token_word "procedure" >> defining_program_unit_name >>= fun dpuname ->
    opt formal_part >>= fun formal -> return dpuname)
(*TODO  <|> (token_word "function" >> defining_designator >>= fun def ->
    opt formal_part >>= fun formal -> token_word "return" >> (subtype_mark()))
*)

(**========from 3**)
let basic_declarative_item : unit parser = todo "basic_declarative_item"
let body = todo "body"
let declarative_part =
  many (basic_declarative_item <|> body)
(**========from 3**)

(**========from 10**)
let parent_unit_name = name()
(**========from 10**)

(**========from 11**)
let exception_handler : unit parser = todo "exception_handler"
let handled_sequence_of_statements =
  sequence_of_statements >>= fun stats ->
  opt (token_word "exception" >> many1 exception_handler) >>= fun exc ->
  return @@ HandledStatements(stats, exc)
(**========from 11**)

let designator =
  opt (parent_unit_name << token_char '.') >>= fun n ->
  (identifier <|> operator_symbol)

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
let package_name = name() ^?"package_name"

let use_clause =
  token_word "use" >>
  begin
    sep1 (token_char ',') package_name
    <|> (token_word "type" >> sep1 (token_char ',') (subtype_mark ()))
  end << token_char ';'
  


(** 10. Program Structure and Compilation Issues **)
let with_clause =
  let library_unit_name = name() in
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
      let library_unit_declaration: handled_statements parser = todo "library_unit_declaration" in
      let library_unit_body : handled_statements parser =
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

