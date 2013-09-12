open Core.Std

val load :
  ?pos : Int64.t -> ?len : Int64.t -> string -> 'a Bin_prot.Read_ml.reader -> 'a

val save :
  ?header : bool -> ?perm : Unix.file_perm -> string ->
  'a Bin_prot.Type_class.writer -> 'a -> unit

(* converts the value to a string with a newline at the end and no other newlines *)
val to_line : 'a Bin_prot.Type_class.t -> 'a -> Bigstring.t
(* reads a string with no newlines (which must be the output of [to_line] without the
   trailing newline) and converts it to a value *)
val of_line : string -> 'a Bin_prot.Type_class.t -> 'a
