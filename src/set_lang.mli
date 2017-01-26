(* A simple DSL for sets. *)

open! Core

module Raw : sig

  (*
     [Raw.t] spells out the variants of a [t]:
     - value of base type
     - set constant
     - union
     - intersection
     - difference
     [Raw.t] is useful in interfaces because there is no explicit comparator.
     It is not intended for direct use in code; instead, use [t].
  *)
  type ('base, 'set) t = private
    | Base  of 'base
    | Set   of 'set
    | Union of ('base, 'set) t * ('base, 'set) t
    | Inter of ('base, 'set) t * ('base, 'set) t
    | Diff  of ('base, 'set) t * ('base, 'set) t
  [@@deriving compare]

end

(* A [t] is a Raw.t specialized to Set.t. *)
type ('base, 'elt, 'cmp) t = ('base, ('elt, 'cmp) Set.t) Raw.t [@@deriving compare]

(*
   [base], [set], [inter2], [union2], [diff]:
   These are "smart" constructors that simplify away constants where possible.
*)
val base   : 'base -> ('base, _, _) t
val set    : ('elt, 'cmp) Set.t -> (_, 'elt, 'cmp) t
val inter2 : ('b,'e,'c) t -> ('b,'e,'c) t -> ('b,'e,'c) t
val union2 : ('b,'e,'c) t -> ('b,'e,'c) t -> ('b,'e,'c) t
val diff   : ('b,'e,'c) t -> ('b,'e,'c) t -> ('b,'e,'c) t

(*
   [union], [union_list], [union_list_exn],
   [inter], [inter_list], [inter_list_exn]:
   These are more "smart" constructors like those above.
   They create unions or intersections of one or more [t].

   Without a specific set type, constructing an empty union is impossible.
   Regardless of set type, an empty intersection is meaningless.
*)
val union : ('b,'e,'c) t * ('b,'e,'c) t list -> ('b,'e,'c) t
val inter : ('b,'e,'c) t * ('b,'e,'c) t list -> ('b,'e,'c) t
val union_list : ('b,'e,'c) t list -> ('b,'e,'c) t Or_error.t
val inter_list : ('b,'e,'c) t list -> ('b,'e,'c) t Or_error.t
val union_list_exn : ('b,'e,'c) t list -> ('b,'e,'c) t
val inter_list_exn : ('b,'e,'c) t list -> ('b,'e,'c) t

(* [values] extracts all values of the base type *)
val values : ('base, _, _) t -> 'base list

(*
   [constant_value (Set s) = Some s]
   [constant_value _ = None] otherwise
*)
val constant_value : (_, 'elt, 'cmp) t -> ('elt, 'cmp) Set.t option

(*
   [subst], [map], [specialize], [eval]:
   These functions fold over an entire [t].

   [subst (Base b) ~f = f b]
   [subst (Set s) ~f = Set s]
   [subst (Union (t1,t2)) ~f = union2 (subst t1 ~f) (subst t2 ~f)]
   [subst (Inter (t1,t2)) ~f = inter2 (subst t1 ~f) (subst t2 ~f)]
   [subst (Diff (t1,t2)) ~f = diff (subst t1 ~f) (subst t2 ~f)]

   [map t ~f = subst t ~f:(fun b -> base (f b))]

   [specialize t ~f = subst t ~f:(fun b -> match f b with Some s -> set s | _ -> base b)]

   [eval t ~f = Option.value_exn (constant_value (specialize t ~f:(fun b -> Some (f b))))]
   except [eval] never raises an exception.
*)
val subst      : ('b1,'e,'c) t -> f:('b1 -> ('b2,'e,'c) t)       -> ('b2,'e,'c) t
val map        : ('b1,'e,'c) t -> f:('b1 -> 'b2)                 -> ('b2,'e,'c) t
val specialize : ('b,'e,'c) t  -> f:('b -> ('e,'c) Set.t option) -> ('b,'e,'c) t
val eval       : ('b,'e,'c) t  -> f:('b -> ('e,'c) Set.t)        -> ('e,'c) Set.t

(* [invariant t] should be equivalent to [assert true] *)
val invariant : ('base, 'elt, 'cmp) t -> unit

(*
   [Make_monadic_eval] defines new versions of [subst], [map], [specialize], and [eval]
   that work inside the given monad.

   Useful, for instance, if evaluating a base value produces a Deferred.t.
*)
module Make_monadic_eval( M : Monad.S ) : sig

  val subst      : ('b,'e,'c) t -> f:('b -> ('b,'e,'c) t M.t)         -> ('b,'e,'c) t M.t
  val map        : ('b,'e,'c) t -> f:('b -> 'b M.t)                   -> ('b,'e,'c) t M.t
  val specialize : ('b,'e,'c) t -> f:('b -> ('e,'c) Set.t option M.t) -> ('b,'e,'c) t M.t
  val eval       : ('b,'e,'c) t -> f:('b -> ('e,'c) Set.t M.t)        -> ('e,'c) Set.t M.t

end

(*
   The functors [Make] and [Make_binable] specialize this interface to specific set types.
   In doing so, they also enable sexp and bin_io conversions, and [union] of empty lists.
   See [Set_lang_intf] for their interfaces.
*)

module type S = Set_lang_intf.S with module Raw := Raw
module type S_binable = Set_lang_intf.S_binable with module Raw := Raw

module Make ( Elt : Comparable.S ) : S
  with module Set = Elt.Set

module Make_binable ( Elt : Comparable.S_binable ) : S_binable
  with module Set = Elt.Set
