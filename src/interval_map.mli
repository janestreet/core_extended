(*
   This is to represent a data structure where we specify intervals each associated
   to a value, and given a key, we want to find what is the associated value.

   The base structure defines a continuous series where points for all keys have a
   value. Each point represents a low/left boundary of an interval which is open on the
   right side. So that keys before/smaller than/left of the first point have a value an
   additional default value is specified.

   Augmenting the keys with [key Left_boundary.t] can be used to construct series where
   the key is a continuous type, and the left boundaries of the intervals can be inclusive
   or exclusive. Make constructs a series type of this form.

   If you want to represent a series where some points do not have a value, this can be
   done by using ['a option] as the value type. This can be used to represent intervals
   over a limited range of keys (the leftmost default should be [None], and also some
   point at the right part of the range which sets to [None], inclusive or exclusive keys
   can be used to make the upper bound of the range exclusive or inclusive respectively).
*)

open Core_kernel.Std

(* The raw form of the type. *)
type ('k, 'v, 'cmp) t = {
  left_of_leftmost  : 'v; (* default *)
  value_right_of    : ('k, 'v, 'cmp) Map.t;
} with fields

val create
  : left_of_leftmost:'v -> value_right_of:('k, 'v, 'cmp) Map.t
  -> ('k, 'v, 'cmp) t

val always : 'v -> comparator:('k, 'cmp) Comparator.t -> ('k, 'v, 'cmp) t

val change : ('k, 'v, 'cmp) t -> at:'k -> 'v -> ('k, 'v, 'cmp) t

(* Returns the value at the point before the supplied key. *)
val value_immediately_left_of : ('k, 'v, 'cmp) t -> 'k -> 'v

val find : ('k Left_boundary.t, 'v, 'cmp) t -> 'k -> 'v

module type S = Interval_map_intf.S
  with type ('k, 'v, 'cmp) interval_map := ('k, 'v, 'cmp) t

module type S_with_boundary = Interval_map_intf.S_with_boundary
  with type ('k, 'v, 'cmp) interval_map := ('k, 'v, 'cmp) t

(* Note on the sexp format of generated interval_map types:
   This type is not serialised as the above record, but rather in a manner similar to
   ['a Left_boundary.Map.t], for example:

   sexp_of_t (create
   ~left_of_leftmost:`Pre
   ~value_right_of:(Map.of_alist [Exc 1, `A; Inc 2, `B]))
   = (Pre ((Exc 1) A) ((Inc 2) B))
*)

(* [Make] creates an interval map which directly uses some key type.

   As we use the normal form of map type to construct this, we pass in
   the type with its map module, rather than just the type and a comparison
   function (this means we can use the same comparator_witness type).

   In this case the keys used always act as inclusive left-boundaries of a range.
*)
module Make (T : Interval_map_intf.Type_with_map_module) : S
  with type Key.t = T.Map.Key.t
   and type Key.comparator_witness = T.Map.Key.comparator_witness

(* [Make_with_boundary] wraps the supplied key up using [Left_boundary.t],
   before constructing the interval map.

   The use of [Key.t Left_boundary.t] allows additional convenience functions.

   This is kept separate from [Make] because there are cases when the presence of
   inclusive and exclusive boundaries makes things very complicated.
*)
module Make_with_boundary (Key : Interval_map_intf.Key) : S_with_boundary
  with type key := Key.t
