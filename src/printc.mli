val (~:) : string list -> string         (** alias for String.concat *)
val (~%) : ('a,unit,string) format -> 'a (** alias for sprintf *)

(** printing functions *)

val print : string -> unit
val eprint : string -> unit
val fprint : out_channel -> string -> unit

(** printing functions that add endlines *)

val printl : string -> unit
val eprintl : string -> unit
val fprintl : out_channel -> string -> unit

(* adds padding to the left-hand side, filling with spaces by default *)
val lpad : ?fill:Char.t -> int -> string -> string

(* adds padding to the right-hand side, filling with spaces by default *)
val rpad : ?fill:Char.t -> int -> string -> string

(* [i2s n] returns a string representation of n *)
val i2s : int -> string

(* [f2s x] returns a string representation of x  *)
val f2s : float -> string
