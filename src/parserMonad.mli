type ts
type 'res state
type 'res error = 'res state * string
type ('res,'a) t

val error : string -> ('res,'a) t
val showerr : 'res state -> string -> string

val return : 'a -> ('res,'a) t
val ( >>= ) : ('res,'a) t -> ('a -> ('res, 'b) t) -> ('res, 'b) t

val ( >> ) : ('res,'a) t -> ('res, 'b) t -> ('res, 'b) t
val ( << ) : ('res,'a) t -> ('res, 'b) t -> ('res,'a) t
val ( ^? ) : ('res,'a) t -> string -> ('res,'a) t
val ( <|> ) : ('res,'a) t -> ('res,'a) t -> ('res,'a) t

val many : ('res,'a) t -> ('res, 'a list) t
val many1 : ('res,'a) t -> ('res, 'a list) t
val sep : ('res,'a) t -> ('res, 'b) t -> ('res, 'b list) t
val sep1 : ('res,'a) t -> ('res, 'b) t -> ('res, 'b list) t
val opt : ('res,'a) t -> ('res, 'a option) t

val map : ('a -> 'b) -> ('res,'a) t -> ('res, 'b) t
val guard : bool -> ('res, unit) t
val ( >*< ) : ('res,'a) t -> ('res, 'b) t -> ('res, 'a * 'b) t

val update_state : ('res -> 'res) -> ('res,'a) t -> ('res, 'a) t
val with_state : ('res, 'a) t -> ('res, 'a * 'res) t

val char1 : ('res, char) t
val char_when : (char -> bool) -> ('res, char) t
val char : char -> ('res, char) t
val keyword : string -> ('res, string) t
val make_ident : (char -> bool) -> ('res, string) t
(*val int : ('res, int) t*)

val init_state : 'res -> 'res state
val run_ch : 'res -> ('res,'a) t -> in_channel -> 'a
val run_stdin : 'res -> ('res,'a) t -> 'a
val run_file : 'res -> ('res,'a) t -> string -> 'a
val run_string : 'res -> ('res,'a) t -> string -> 'a
