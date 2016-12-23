(** Stores an Hlist as a set of right-nested tuples. *)
type 'elements t = 'elements
type nil
type ('head, 'tail) cons = 'head * 'tail

val empty : nil t
val cons : 'a -> 'tail t -> ('a, 'tail) cons t

val head : ('a, _) cons t -> 'a
val tail : (_, 'tail) cons t -> 'tail

module Suffix_index : sig
  type ('elements_before, 'elements_after) t

  val whole_list : ('elements, 'elements) t
  val tail_of : ('elements, (_, 'tail) cons) t -> ('elements, 'tail) t
end

(** Drop some prefix of an Hlist to get a suffix of it.

   /O(n)/ allocation and work.
*)
val drop : 'elements_before t
  -> ('elements_before, 'elements_after) Suffix_index.t
  -> 'elements_after t

module Element_index : sig
  type ('elements, 'element) t

  val first_element : (('head, _) cons, 'head) t
  val of_tail : ('tail, 'element) t -> ((_, 'tail) cons, 'element) t

  (** Transform an index to find an element within a suffix of the list.

      /O(n)/ allocation and work.
  *)
  val within : ('inner, 'element) t -> suffix:('outer, 'inner) Suffix_index.t -> ('outer, 'element) t
end

(** Get the element at some index of an hlist.

    /O(n)/ work, no allocation.
*)
val nth : 'elements t -> ('elements, 'element) Element_index.t -> 'element
