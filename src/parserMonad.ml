open Util
open Llist

type ts = char llist
type pos = int * int * (char list * char * char list)
type 'res state = pos * 'res
type 'res error = 'res state * string
type ('res,'a) t = 'res state -> ts -> ('a * 'res state * ts, 'res error) either

exception ParseError of string

let lt_pos (l1,p1,_) (l2,p2,_) =
  if l1 < l2 then true
  else if l1 = l2 then p1 < p2
  else false

let eplus ((pos1,res1),msg1 as e1) ((pos2,res2),msg2 as e2) =
  if lt_pos pos1 pos2 then e2 else e1

let showerr ((line,pos,(pre,c,post)), res) msg =
  !%"{\n  line %d, %d: %s\n%s[%c]%s\n}"line pos msg (string_of_chars pre)
  c (string_of_chars post)
    
let return =
    fun x ->
      fun state code -> Inl (x, state, code)


let error msg = fun state _code -> Inr (state, msg)

let (>>=) =
    fun p f ->
      fun state code ->
	match p state code with
	| Inl (x, state', ts) -> f x state' ts
	| Inr err -> Inr err
	      
let (>>) =
    fun p1 p2 ->
      p1 >>= fun _ -> p2

let (<<) =
    fun p1 p2 ->
      p1 >>= fun x -> p2 >> return x

let ( ^? ) =
    fun p msg ->
      fun state code ->
	match p state code with
	| Inl l -> Inl l
	| Inr (st,msg0) -> Inr (st,msg ^": "^msg0)
    
    (* (<|>) : 'a m -> 'a m -> 'a m *)
let (<|>) : ('res, 'a) t -> ('res, 'a) t -> ('res, 'a) t =
    fun p1 p2 ->
      fun state code ->
	match p1 state code with
	| Inl (x1, state', ts) -> Inl (x1, state', ts)
	| Inr err1 ->
	    begin match p2 state code with
	    | Inl (x2, state', ts) -> Inl (x2,state',ts)
	    | Inr err2 -> Inr (eplus err1 err2)
	    end

(*
let (<|?>) p1 p2 = fun state code ->
  match p1 state code with
  | Inl (x1, state', ts) -> Inl (x1, state', ts)
  | Inr err1 ->
      print_endline err1;
      begin match p2 state code with
      | Inl (x2, state', ts) -> Inl (x2,state',ts)
      | Inr err2 -> Inr (eplus err1 err2)
      end
*)	

let rec many =
    fun p ->
      (p >>= fun x -> many p >>= fun xs -> return (x::xs))
	<|> (return [])

let many1 p =
  p >>= fun x -> many p >>= fun xs -> return (x::xs)

let sep separator p =
  (p >>= fun x -> many (separator >> p) >>= fun xs -> return (x::xs))
    <|> (return [])

let sep1 s p =
  p >>= fun x -> many (s >> p) >>= fun xs -> return (x :: xs)

let opt =
    fun p ->
      (p >>= fun x -> return (Some x)) <|> (return None)

let map f p = p >>= fun x -> return @@ f x

let guard b = if b then return () else error "Guard"

let (>*<) p1 p2 = p1 >>= fun x -> p2 >>= fun y -> return (x,y)

let _char1_with_debug (state :'res state) : ts -> (char * 'res state * ts, 'res error) either =
  let (pos, res) = state in
  function
  | Nil -> Inr ((pos,res),"(Nil)")
  | Cons (x,xs) ->
      let next (pre,x0, _) =
	let pre' = if List.length pre < 100 then pre @ [x0]
	  else List.tl pre @ [x0]
	in
	(pre' , x, Llist.take 100 !$xs)
      in
      match x, pos with
      | '\n', (line,_col,cs) ->
          let state' = ((line+1,-1, next cs), res) in
	  Inl (x, state', !$xs)
      | _, (line,col,cs) ->
          let state' = ((line, col+1, next cs),res) in
	  Inl (x, state', !$xs)

let char1wd : ('res,char) t = _char1_with_debug

let char1_without_debug state = function
  | Nil -> Inr (state,"(Nil)")
  | Cons (x,xs) -> Inl (x, state, !$xs)

let char1 : ('res, char) t = _char1_with_debug

let char_when f = char1 >>= fun c ->
  if f c then return c
  else error (!%"(char_when at:'%c')" c)

let char c : ('res,char)t = char_when ((=) c) ^? (!%"char '%c'" c)

let keyword w =
  let rec iter i =
    if i < String.length w then
      char w.[i] >> iter (i+1)
    else return w
  in
  iter 0

let make_ident f : ('res, string) t=
  many1 (char_when f) >>= fun cs ->
    return (string_of_chars cs)
(*
let int : ('res, int) t =
  opt (char '-') >>= fun minus ->
  make_ident (function '0'..'9' -> true | _ -> false) >>= fun s ->
  return
    begin match minus with
    | None -> int_of_string s
    | Some _ -> - int_of_string s
    end
*)
let run p state ts =
  match p state ts with
  | Inl (x,_state',_xs) ->
      print_endline (showerr _state' "Success");
      x
  | Inr (state, msg) -> 
      raise (ParseError (showerr state msg))

let update_state (f: 'res -> 'res) : ('res, unit) t =
  fun state ts ->
  let (pos, res) = state in
  Inl ((), (pos, f res), ts)

let with_state : ('res, 'a) t -> ('res, 'a * 'res) t =
  fun p state ts ->
  match p state ts with
  | Inl (x, (pos,res), ts') -> Inl ((x,res), (pos,res), ts')
  | Inr e -> Inr e 

let init_pos : pos = (1, 0, ([],'_',[]))
let init_state res = (init_pos,res)

let run_ch res p ch =
  run p (init_state res) (Llist.of_stream (Stream.of_channel ch))

let run_stdin res p = run_ch res p stdin

let run_file res p filename =
  open_in_with filename (fun ch -> run_ch res p ch)

let run_string res p s =
  run p (init_state res) (Llist.of_string s)
