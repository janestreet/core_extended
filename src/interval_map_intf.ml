open Core_kernel.Std

module type Key = Core_kernel.Core_map_intf.Key

(* Something which has a map type.

   The map part should be a subset of that generated with [Map.Make] or [Comparable.Make],
   so you should be able to satisfy this signature with many existing modules
   e.g. [String] or [Date], and also do e.g.

       Interval_map.Make(struct
         module T = struct
           type t = ... with compare, sexp
         end
         include T
         module Map = Map.Make(T)
         (* OR include Comparable.Make(T) *)
       end)
*)
module type Type_with_map_module = sig
  type t with sexp

  module Map : sig
    module Key : Comparator.S with type t = t

    type +'a t = (Key.t, 'a, Key.comparator_witness) Map.t
    with compare, sexp
  end
end

(* A standard interval map. *)
module type S = sig
  type ('k, +'v, 'cmp) interval_map

  module Key : Comparator.S

  type +'a t = (Key.t, 'a, Key.comparator_witness) interval_map
  with sexp

  val create
    : left_of_leftmost:'a
    -> value_right_of:(Key.t, 'a, Key.comparator_witness) Map.t
    -> 'a t

  val always : 'a -> 'a t
end

(* An interval map where the key has been wrapped with [Left_boundary.t]. *)
module type S_with_boundary = sig
  type key

  module Left_boundary : sig
    type t = key Left_boundary.t with sexp, compare
    include Comparable.S with type t := t
  end

  include S with type Key.t = Left_boundary.t

  val find : 'a t -> key -> 'a
end
