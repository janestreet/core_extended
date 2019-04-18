open! Core_kernel
open! Import

(** A polymorphic data structure parametrized by ['a] to represent lists of elements of
    ['a] while supporting constant time append operations.

    One example of use is to manipulate a decorated text represented as a sequence of
    words.  Eventually we are interested in producing the concatenation of all the word in
    some form, but we do not want to pay the allocation costs of buildings intermediate
    string concatenations.

    This module is a generalization of the [Rope] module.  Essentially:
    [type Rope.t = string Appendable_list.t]

    The following operations all run in constant time:
    [empty], [of_list], [singleton], [append], [concat], [add_front], [add_back]

    [to_sequence] builds a sequence where access to the next element has an amortized
    constant time.

    All traversal operations such as [iter] and [fold] are tail recursive.

    The monad exported by the module is semantically the same as the one in List.  That
    is: [bind t f] applies [f] to each element of [t] and append the resulting list
    respecting the order in which the elements appear in [t]. *)

type +'a t [@@deriving sexp]

val empty : _ t
val of_list : 'a list -> 'a t
val singleton : 'a -> 'a t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val add_front : 'a -> 'a t -> 'a t
val add_back : 'a t -> 'a -> 'a t
val to_sequence : 'a t -> 'a Sequence.t

include Monad.S with type 'a t := 'a t
include Container.S1 with type 'a t := 'a t

module For_testing : sig
  module Element : sig
    type t [@@deriving compare, sexp_of]

    val quickcheck_observer : t Quickcheck.Observer.t
  end

  val map_simple : 'a t -> f:('a -> 'b) -> 'b t
  val quickcheck_generator : Element.t t Quickcheck.Generator.t
end
