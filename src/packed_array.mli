(** A packed array is a read-only array that has a fairly compact representation and will
    not be traversed by the GC. It's designed for cases where you might construct a very
    large array once and then read from it many times. Array access is reasonably
    efficient. Be warned, however, that construction can be excruciatingly slow and might
    even perform many intermediate allocations, depending on the type of array. *)
open! Core

(** [Basic] is the minimal interface you need to provide to make a packed array for a new
    type. *)
module type Basic = sig
  type elt [@@deriving sexp, bin_io]
  type t

  val length : t -> int
  val unsafe_get : t -> int -> elt
  val of_array : elt array -> t
  val unsafe_slice : t -> pos:int -> len:int -> t
end

(** [S] is the packed array interface. *)
module type S = sig
  include Basic

  include Sexpable.S with type t := t
  include Binable.S with type t := t

  val get : t -> int -> elt
  val slice : t -> pos:int -> len:int -> t
  val iter : t -> f:(elt -> unit) -> unit
  val fold : t -> init:'a -> f:('a -> elt -> 'a) -> 'a

  val of_array : elt array -> t
  val to_array : t -> elt array

  val of_list : elt list -> t
  val to_list : t -> elt list

  val empty : t
end

(** This is pretty pointless -- the GC will traverse a [Make(B).t] if and only if it would
    traverse the corresponding [B.t].  There is no sense in which the returned module
    implements a "packed" array. *)
module Make (B : Basic) : S with type elt := B.elt and type t := B.t

(** The representation of a packed array type created using [Of_binable] is a Bin_prot
    buffer and a packed array of indices pointing to the beginning of each serialized
    element in the buffer. *)
module Of_binable (B : sig
  include Binable.S
  include Sexpable.S with type t := t
end) : S with type elt := B.t

(** the representation of a packed array of tuples is a tuple of packed arrays. This makes
    the [zip_exn] and [unzip] functions constant time. *)
module Tuple2 (A : Basic) (B : Basic) : sig
  type elt = A.elt * B.elt
  include S with type elt := elt
  val zip_exn : A.t -> B.t -> t
  val unzip : t -> A.t * B.t
end

(** [Of_packed_array(P)] creates a packed array of packed arrays. The representation is a
    [P.t] and packed array of indices into it which point to the beginning of each inner
    array. *)
module Of_packed_array (P : S) : sig
  include S with type elt := P.t
  val concat : t -> P.t
end

(** These primitive packed arrays are represented by their respective Bigarray types. *)
module Bool           : S with type elt := bool
module Char           : S with type elt := char
module Int            : S with type elt := int
module Int8_unsigned  : S with type elt := int
module Int8           : S with type elt := int
module Int16_unsigned : S with type elt := int
module Int16          : S with type elt := int
module Int32          : S with type elt := int32
module Int64          : S with type elt := int64
module Float32        : S with type elt := float
module Float          : S with type elt := float
module String         : S with type elt := string
