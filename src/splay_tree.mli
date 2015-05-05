open Core.Std


module type Key = sig
  type t with sexp
  include Comparable with type t := t
end

module type S = sig
  type 'a t with sexp
  type key with sexp

  val empty : 'a t
  val is_empty : 'a t -> bool
  val length : 'a t -> int

  val keys : 'a t -> key list
  val data : 'a t -> 'a list
  val to_alist : 'a t -> (key * 'a) list

  val mem : 'a t -> key -> 'a t * bool
  val find : 'a t -> key -> 'a t * 'a option
  val set : 'a t -> key:key -> data:'a -> 'a t

  val delete : 'a t -> key -> 'a t
  val delete_min : 'a t -> (key * 'a * 'a t) option
  val delete_max : 'a t -> (key * 'a * 'a t) option
  val delete_after  : 'a t -> key -> (key * 'a * 'a t) option
  val delete_before : 'a t -> key -> (key * 'a * 'a t) option

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val map_range
    :  'a t
    -> min_key:key
    -> max_key:key
    -> f:((key * 'a) list -> (key * 'a) list)
    -> 'a t

  val split : 'a t -> key -> 'a t * 'a option * 'a t
end

module Make (Key : Key) : (S with type key = Key.t)
