(** Extensio to [Core.Memo] *)

(** A version of [Memo.general] more suitable for memoizing
    recursively-defined functions *)
val general_rec : (('a -> 'b) -> 'a -> 'b) -> ('a -> 'b)

