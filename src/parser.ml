(** SEE: http://cui.unige.ch/isi/bnf/Ada95/BNFindex.html **)
open Util
open ParserMonad
open Ast

let is_some = function Some _ -> true | None -> false

let sep2 s p =
  p >>= fun x -> many1 (s >> p) >>= fun xs -> return (x :: xs)

type 'a parser = 'a ParserMonad.t
let sep = ()
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

let popen = token_char '('
let pclose = token_char ')'
let comma = token_char ','
let vbar = token_char '|'
let comsep1 p = sep1 comma p
let comsep2 p = sep2 comma p
let symbol s = token(keyword s)

(** 2. Lexical Elements **)
let format_effector =
  char_when (function |'\x09'|'\x0b'|'\x0d'|'\x0a'|'\x0c'-> true | _ -> false)

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

let word w =
  identifier >>= fun s -> guard (s = w) >> return s

let graphic_character =
  identifier_letter <|> digit
  <|> SpecialChar.space <|> SpecialChar.iso_10646_BMP

let numeral =
  sep1 (token_char '_') digit >>= (return $ string_of_chars)

let exponent =
  let plusminus =
    (token_char '+'>> return Plus) <|> (token_char '-'>> return Minus)
  in
  token_char 'E' >> opt plusminus >*< numeral

let based_literal =
  let base = numeral in
  let based_numeral =
    let extended_digit =
      digit <|> char_when (function 'A'..'F' -> true | _ -> false)
    in
    sep1 (token_char '_') extended_digit >>= (return $ string_of_chars)
  in
  base >>= fun base ->
  token_char '#' >>
  based_numeral >>= fun int ->
  opt (token_char '.' >> based_numeral) >>= fun frac ->
  token_char '#' >>
  opt exponent >>= fun expo ->
  return @@ NumBased(base, int, frac, expo)

let numeric_literal =
  let decimal_literal =
    numeral >>= fun int ->
    opt (token_char '.' >> numeral) >>= fun frac ->
    opt exponent >>= fun expo ->
    return @@ NumDecimal(int, frac, expo)
  in
  decimal_literal <|> based_literal

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

(**=====**)
let selector_name = (* sec. 4 *)
  identifier <|> (character_literal |> map (String.make 1)) <|> operator_symbol
let direct_name =
  identifier <|> operator_symbol
(**=====**)

(** 3. Declarations and Types **)
let defining_identifier = identifier

let rec subtype_indication () =
  subtype_mark() >>= fun n -> opt (constraint_()) >>= fun c -> return(n,c)

and subtype_mark () = name()

and constraint_ () =
  (range_constraint() >>= fun r -> return @@ CoRange r)
  <|> digits_constraint()
  <|> delta_constraint()
  <|> index_constraint()
  <|> discriminant_constraint()

and range_constraint () =
  word "range" >> range()

and range () =
  (range_attribute_reference() >>= fun (p,d) -> return @@ RangeAttrRef (p,d))
  <|> (simple_expression() >*< (symbol "..">>simple_expression()) >>=
       fun (x,y) ->
       return @@ Range(x,y))

and digits_constraint () =
  word"digits">> expression() >*< opt(range_constraint()) >>= fun(e,r)->
  return @@ CoDigits(e, r)

and index_constraint() =
  popen >> comsep1 (discrete_range()) << pclose >>= fun rs ->
  return @@ CoIndex rs

and discrete_range () =
  (subtype_indication() >>= fun ind -> return @@ DrSubtype ind)
  <|> (range() >>= fun r -> return @@ DrRange r)

and discriminant_constraint () =
  popen >> comsep1 (discriminant_association()) << pclose >>= fun ds ->
  return @@ CoDiscrim ds

and discriminant_association () =
  opt (sep1 vbar selector_name << symbol"=>") >*< expression()

and discrete_choice_list () = sep1 vbar (discrete_choice())

and discrete_choice () =
  (expression() >>= fun e -> return @@ DcExpr e)
  <|> (discrete_range() >>= fun r -> return @@ DcRange r)
  <|> (word "others" >> return @@ DcOthers)

(** 4. Names and Expressions **)
and name () =
  let name_next prefix =
    (*explicit_dereference*)
    (token_char '.' >> keyword "all" >> return @@ NExplicitDeref prefix)
    (*indexed_component*)
    <|> (popen >> sep1(token_char '.')(expression()) << pclose >>= fun es -> return @@ NIndexedComp(prefix, es))
    (*slice*)
    <|> (popen >> discrete_range() << pclose >>= fun dr ->
         return @@ NSlice(prefix, dr))
    (*selected_component*)
    <|> (token_char '.' >> selector_name >>= fun sel ->
         return @@ NSelectedComp(prefix, sel))
    (*attribute_reference*)
    <|> (token_char '\'' >> attribute_designator() >>= fun attr ->
         return @@ NAttrRef(prefix, attr))
    (*type_conversion*)
    <|> (popen >> expression() >>= fun expr ->
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

and prefix () = name()

and attribute_designator () : attribute parser =
  (word "Access" >> return AAccess)
  <|> (word "Delta" >> return ADelta)
  <|> (word "Digit" >> return ADigit)
  <|> (identifier >>= fun ident ->
   opt (popen >> expression() << pclose) >>= fun e ->
   return @@ AIdent(ident, e))

and range_attribute_reference () =
  let range_attribute_disignator () =
    word "Range" >> opt (popen >> expression() << pclose)
  in
  prefix() >*< (token_char '\'' >> range_attribute_disignator())

and array_aggregate () =
  let array_component_associtiation () =
    discrete_choice_list() >>= fun dcs -> symbol "=>" >>
    expression() >>= fun e -> return @@ (dcs, e)
  in
  (*positional_array_aggregate*)
  (popen >> comsep2 (expression()) >>= fun es ->
   pclose >> return @@ ArrayAgPos es)
  <|> (popen >> comsep1 (expression()) >>= fun es ->
       comma >> word "others" >> symbol "=>" >>
       expression() >>= fun oe ->
       pclose >> return @@ ArrayAgPosOthers (es,oe))
  (*named_array_aggregate*)
  <|> (popen >> comsep1 (array_component_associtiation())
       << pclose >>= fun cs ->
       return @@ ArrayAgNamed cs)

and aggregate () =
  let record_component_association () =
    let component_choice_list =
      (word "others" >> return CCLOthers)
      <|> (sep1 vbar selector_name >>= fun ns -> return @@ CCLSels ns)
    in
    opt (component_choice_list << symbol "=>") >*< expression()
  in
  let ancestor_part = expression in
  let nullrec = word"null">> word"record">> return AgNullRecord in
  (*record_aggregate*)
  (popen >> comsep1 (record_component_association())
   << pclose >>= fun cs -> return @@ AgRecord cs)
  <|> nullrec
  (*extension_aggregate*)
  <|> (popen >> ancestor_part() << word "with" >>= fun anc ->
       begin nullrec
       <|> (comsep1 (record_component_association()) >>= fun cs->
            return @@ AgExtension(anc, cs))
       end << pclose)
  (*array_aggregate*)
  <|> (array_aggregate() >>= fun aa -> return @@ AgArray aa)

and qualified_expression () =
  subtype_mark() >>= fun n ->
    (popen >> expression() << pclose >>= fun es -> return @@ QExpr(n,es))
    <|> (aggregate() >>= fun ag -> return @@ QAggr(n,ag))

and primary () =
  (numeric_literal >>= fun num -> return @@ PNum num)
  <|> (word "null" >> return @@ PNull)
  <|> (string_literal >>= fun s -> return @@ PString s)
  <|> (word "new" >>
         (subtype_indication()>>= fun ind-> return @@ PAllocator(AlSubtype ind))
         <|> (qualified_expression()>>= fun q-> return @@ PAllocator(AlQual q)))
  <|> (aggregate() >>= fun a -> return @@ PAggregate a)
  <|> (qualified_expression() >>= fun q -> return @@ PQual q)
  <|> (name() >>= fun n -> return @@ PName n)
  <|> (popen >> expression() << pclose >>= fun e ->
       return @@ PParen e)

and factor () =
  let pop = symbol "**" >> return Pow in
  (word "abs" >> primary() >>= fun p -> return @@ FAbs p)
  <|> (word "not" >> primary() >>= fun p -> return @@ FNot p)
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
    (token_char '=' >> return Eq) <|> (symbol "/=" >> return Neq)
    <|> (token_char '<' >> return Lt) (*TODO*)
  in
  (simple_expression() >>= fun se -> opt(word "not") >>= fun not -> word "in">>
    begin
    (range() >>= fun range -> return @@ RInRange(is_some not, se, range))
    <|> (subtype_mark() >>= fun smark -> return @@ RInSubMark(is_some not, se, smark))
  end)
  <|> (simple_expression() >>= fun se ->
       many (aop >*< simple_expression()) >>= fun ses -> return @@ RE(se, ses))

and expression () : expression parser =
  (sep1 (word "and") (relation ()) >>= fun rs -> return @@ EAnd rs)
  <|> (sep1 (word "and" >> word "then") (relation ()) >>= fun rs ->
       return @@ EAnd rs)
  <|> (sep1 (word "or") (relation ()) >>= fun rs -> return @@ EOr rs)
  <|> (sep1 (word "or" >> word "else") (relation ()) >>= fun rs ->
       return @@ EOr rs)
  <|> (sep1 (word "xor") (relation ()) >>= fun rs -> return @@ EXor rs)

(**=============6*)
and parameter_assoc () =
  opt (selector_name << symbol "=>") >>= fun sel ->
  expression() >>= fun expr ->
  return (sel, expr)
and actual_parameter_part () =
  popen >>
  comsep1 (parameter_assoc())
  << pclose
(**=============6*)

(**============13*)
and static_expression () = expression()
and delta_constraint() =
  word"delta">> static_expression() >*< opt(range_constraint()) >>=
  fun (e, r) -> return @@ CoDelta(e, r)
  
(**============13*)
  
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
  (word "procedure" >> defining_program_unit_name >>= fun dpuname ->
    opt formal_part >>= fun formal -> return dpuname)
(*TODO  <|> (word "function" >> defining_designator >>= fun def ->
    opt formal_part >>= fun formal -> word "return" >> (subtype_mark()))
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
  opt (word "exception" >> many1 exception_handler) >>= fun exc ->
  return @@ HandledStatements(stats, exc)
(**========from 11**)

let designator =
  opt (parent_unit_name << token_char '.') >>= fun n ->
  (identifier <|> operator_symbol)

let subprogram_body =
  subprogram_specification >>= fun spec ->
  word "is" >>
  declarative_part >>= fun decl ->
  word "begin" >>
  handled_sequence_of_statements >>= fun stats ->
  word "end" >>
  opt designator >>= fun design ->
  token_char ';' >>
  return stats

(** 7. Packages **)

let package_body = todo "package_body"

(** 8. **)
let package_name = name() ^?"package_name"

let use_clause =
  word "use" >>
  begin
    comsep1 package_name
    <|> (word "type" >> comsep1 (subtype_mark ()))
  end << token_char ';'
  


(** 10. Program Structure and Compilation Issues **)
let with_clause =
  let library_unit_name = name() in
  word "with" >>
  comsep1 library_unit_name >>= fun lu_names ->
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
      (opt (word "private") >>= fun private_ -> library_unit_declaration)
      <|> library_unit_body
      <|> (opt (word "private") >>= fun private_ -> library_unit_renaming_declaration)
    in
    context_clause >>= fun cc ->
    (library_item <|> subunit)
  in
  many compilation_unit

let run_ch ch =
  ParserMonad.run_ch (compilation) ch

