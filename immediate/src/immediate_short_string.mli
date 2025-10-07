@@ portable

open! Core

(** Immediate_short_string represents strings of length 0-7 stored as an integer.

    Invariant: all strings are represented as non-negative integers, and Option.none is
    represented as a non-negative integer distinct from the representation of all strings.

    Invariant: the integer encoding ensures that [compare x y] is fast and corresponds
    with [String.compare (to_string x) (to_string y)]. *)

include Immediate_intf.String_no_option

val of_bytes : Bytes.t -> t [@@zero_alloc]
val of_uint32 : Int_repr.Uint32.t -> t [@@zero_alloc]
val of_local_string : local_ string -> t [@@zero_alloc]
val to_local_string : t -> local_ string [@@zero_alloc]
val of_substring : local_ string -> pos:int -> len:int -> t [@@zero_alloc]
val to_local_bytes : t -> local_ Bytes.t [@@zero_alloc]
val max_length : int
val is_valid_length : int -> bool [@@zero_alloc]
val is_valid_string : local_ string -> bool [@@zero_alloc]
val pad_right : t -> char:char -> len:int -> t [@@zero_alloc]

(** Raises if the result exceeds [max_length]. *)
val append_exn : t -> t -> t
[@@zero_alloc]

val%template gen' : char Quickcheck.Generator.t @ p -> t Quickcheck.Generator.t @ p
[@@mode p = (portable, nonportable)]

val%template gen_with_length
  :  int
  -> char Quickcheck.Generator.t @ p
  -> t Quickcheck.Generator.t @ p
[@@mode p = (portable, nonportable)]

module To_bigstring : Blit.S_distinct with type src := t with type dst := bigstring

module Option : sig
  (* Please note that the [of_string] function is derived from sexp representation so
     [of_string "abc"] raises instead of returning "[Some "abc"]". *)

  include Immediate_intf.String_option with type value := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving hash]

      include Sexpable.S_with_grammar with type t := t
      include Stable_without_comparator with type t := t
      include Intable with type t := t
    end
  end
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving hash]

    include%template Stable_without_comparator_with_witness [@mode local] with type t := t

    include Sexpable.S_with_grammar with type t := t
    include Intable with type t := t
    include Stringable with type t := t
    include Equal.S with type t := t

    val lexicographic_compare : t -> t -> int
  end
end
