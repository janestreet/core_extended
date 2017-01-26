open! Core

(** Allows mutable float record fields where setting the value is much faster because it
    avoids write barrier.  Benchmarks show about eight times better performance for
    setting the value.  Reading the value is the same speed as for [mutable float]. *)
type t [@@deriving bin_io, sexp]

(** Create a [t] with the given initial value. *)
val create : float -> t
val get : t -> float
val set : t -> float -> unit
