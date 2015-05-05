(** A map that folds in new values.

    An example would be a multi-map in which a key is initialized with the empty
    list as its value, and adding a new key/value pair appends the value to the
    key's list. *)

open Core.Std

(** Input signature of the functor {!Make} *)
module type Foldable = sig

  (** The type of the accumlator *)
  type t

  (** The type of the folded in values. *)
  type data

  (** The initial value of the accumulator. *)
  val init : t

  (** The folding function.*)
  val f : t -> data -> t
end


(** Output signature of the functor {!Make}*)
module type S = sig

  (** The type of the values being fold over.*)
  type in_value

  (** The type of the accumulator *)
  type out_value

  type 'key t = private (('key,out_value) Map.Poly.t)

  (* Used internally to tie S1 and S2 together *)
  type 'a _in_value = in_value
  type 'a _out_value = out_value
  type ('a,'b) _t = 'a t

  (** A map containing no bindings *)
  val empty     : _ t

  val singleton : 'a -> in_value -> 'a t
  val is_empty  : _ t -> bool
  val length  : _ t -> int

  (** [add m ~key ~data] adds the key to the value already bound to [key] in [m]. If no
      value is bound to [key] than the initial value specified by the functor will be
      used instead.  *)
  val add       : key:'a -> data:in_value -> 'a t -> 'a t


  val find      : 'a t -> 'a -> out_value
  val remove    : 'a t -> 'a -> 'a t
  val set       : key:'a -> data:out_value -> 'a t -> 'a t
  val mem       : 'a t -> 'a -> bool
  val iter      : 'a t -> f:(key:'a -> data:out_value -> unit) -> unit
  val fold      :
    'a t
    -> init:'b
    -> f:(key:'a -> data:out_value -> 'b -> 'b)
    -> 'b
  val filter    : 'a t -> f:(key:'a -> data:out_value -> bool) -> 'a t
  val keys      : 'a t -> 'a list
  val data      : _ t -> out_value list
  val to_alist  : 'a t -> ('a * out_value) list
  val of_list   : ('a * in_value) list -> 'a t
  val for_all   : _ t -> f:(out_value -> bool) -> bool
  val exists    : _ t -> f:(out_value -> bool) -> bool
  val to_map    : 'a t -> ('a , out_value) Map.Poly.t
  val of_map    : ('a , out_value) Map.Poly.t -> 'a t

end

(** Builds a [fold_map] *)
module Make (Fold : Foldable) : S
  with type in_value = Fold.data
  and type out_value = Fold.t

(** {6 Sexpable interface}
    Same as above but builds the [sexp_of] and [of_sexp] functions. Requires the
    passed in types to be sexpable.
*)

module type S_sexpable = sig
  include S
  include Sexpable.S1 with type 'key t := 'key t
end

module type Foldable_sexpable = sig
  include Foldable
  include Sexpable with type t := t
end

module Make_sexpable (Fold : Foldable_sexpable) : S_sexpable
  with type in_value = Fold.data
  and type out_value = Fold.t

(** {3 Polymorphic folds}

    Polymorphic fold take a
*)

module type Foldable2 = sig
  type 'a t
  val init : _ t
  val f : 'a t -> 'a -> 'a t
end

module type S2 = sig

  type 'a out_value

  type ('key,'data) t = private ('key,'data out_value) Map.Poly.t

  type 'a _in_value = 'a
  type 'a _out_value = 'a out_value
  type ('a,'b) _t = ('a,'b) t

  val empty     : (_,_) t
  val singleton : 'a -> 'b -> ('a,'b) t
  val is_empty  : (_,_) t -> bool
  val length    : (_,_) t -> int
  val add       : key:'a -> data:'b -> ('a,'b) t -> ('a,'b) t
  val find      : ('a,'b) t -> 'a -> 'b out_value
  val remove    : ('a,'b) t -> 'a -> ('a,'b) t
  val set       : key:'a -> data:'b out_value -> ('a,'b) t -> ('a,'b) t
  val mem       : ('a,_) t -> 'a -> bool
  val iter      : ('a,'b) t -> f:(key:'a -> data:'b out_value -> unit) -> unit
  val fold      :
    ('a,'b) t
    -> init:'c
    -> f:(key:'a -> data:'b out_value -> 'c -> 'c)
    -> 'c
  val filter    :
    ('a,'b) t
    -> f:(key:'a -> data:'b out_value -> bool)
    -> ('a,'b) t
  val keys      : ('a,_) t -> 'a list
  val data      : (_,'b) t -> 'b out_value list
  val to_alist  : ('a,'b) t -> ('a * 'b out_value) list
  val of_list   : ('a * 'b) list -> ('a,'b) t
  val for_all   : (_,'b) t -> f:('b out_value -> bool) -> bool
  val exists    : (_,'b) t -> f:('b out_value -> bool) -> bool
  val to_map    : ('a,'b) t -> ('a,'b out_value) Map.Poly.t
  val of_map    : ('a,'b out_value) Map.Poly.t -> ('a,'b) t
end

module Make2 (Fold : Foldable2) : S2
  with type 'a out_value = 'a Fold.t

(** {6 Sexpable interface} *)

module type Foldable2_sexpable = sig
  include Foldable2
  include Sexpable.S1 with type 'a t := 'a t
end

module type S2_sexpable = sig
  include S2
  include Sexpable.S2 with type ('a,'b) t := ('a,'b) t
end

module Make2_sexpable (Fold : Foldable2_sexpable) : S2_sexpable
  with type 'a out_value = 'a Fold.t

(** {3 Predefined modules } *)

module Cons : S2_sexpable
  with type 'a out_value = 'a list

(** A fold for adding. e.g. symbol positions *)
module Add  : S_sexpable
  with type in_value  = int
  and  type out_value = int

module Multiply : S_sexpable
  with type in_value  = int
  and  type out_value = int
