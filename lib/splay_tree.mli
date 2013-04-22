open Core.Std

module type Key = sig
  type t
  include Comparable with type t := t
end

module type S = sig
  type 'a t
  type key

  val empty : 'a t
  val is_empty : 'a t -> bool

  (* order-aware construction *)
  val concat : 'a t -> 'a t -> 'a t
  val sandwich : 'a t -> key -> 'a -> 'a t -> 'a t

  (* order-aware destruction *)
  val splay : 'a t -> key -> 'a t * 'a option * 'a t
  val splay' : 'a t -> key -> 'a t * (key * 'a) option * 'a t
  val delete_min : 'a t -> (key * 'a * 'a t) option
  val delete_max : 'a t -> (key * 'a * 'a t) option

  (* order-oblivious operations *)
  val mem : 'a t -> key -> 'a t * bool
  val find : 'a t -> key -> 'a t * 'a option
  val set : 'a t -> key -> 'a -> 'a t
  val delete : 'a t -> key -> 'a t
end

module Make (Key : Key) : (S with type key = Key.t)

