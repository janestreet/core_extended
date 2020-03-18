(** This module implements efficient and compact arrays of boolean values. It stores its
    values in the bits of an integer, using multiple integers to allow for arrays larger
    than the machine word size. All operations are on immediates (no caml_modify), and are
    quite simple. Hence this data structure should be more efficient than an array of
    bools. *)

type t

include Core.Sexpable with type t := t

(** [create size] size must be less than ((word size - 2) * max array length) *)
val create : int -> t

(** [get t pos] get the value in position [pos], raises Invalid_argument if the position
    is out of bounds. *)
val get : t -> int -> bool

(** [set t pos] set the value in position [pos], raises Invalid_argument if the position
    is out of bounds. *)
val set : t -> int -> bool -> unit

(** [clear t] set the contents of every element to false O(n / (word_size - 2)) *)
val clear : t -> unit

(** [fold t ~init ~f] Fold over the array as in [Array.fold] *)
val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a

(** [iter t ~f] Iterate over the array as in [Array.iter] *)
val iter : t -> f:(bool -> unit) -> unit

(** [length t] returns the length of [t], i.e., the number of bits it contains *)
val length : t -> int
