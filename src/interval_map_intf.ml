open Core_kernel

(*
   This is to represent an unbounded sequence of keys where we specify what value is
   associated with each part interval of the sequence, and then can query the sequence
   for the value associated with a specific point along it.

   The base structure defines a continuous series where points for all keys have a
   value. Each point represents a low/left boundary of an interval which is open on the
   right side. To ensure that keys before/smaller than/left of the first point have a
   defined value an additional default value is specified.

   Augmenting the keys with [key Left_boundary.t] can be used to construct series where
   the left boundaries of the intervals can be inclusive
   or exclusive, which can be very useful when the key is a continuous type (as opposed to
   discrete, at least in terms of what it models).
   Make_with_boundary constructs a series type of this form.

   If you want to represent a series where some points do not have a value, this can be
   done by using ['a option] as the value type. This can be used to represent intervals
   over a limited range of keys (the leftmost default should be [None], and also some
   point at the right part of the range which sets to [None], inclusive or exclusive keys
   can be used to make the upper bound of the range exclusive or inclusive respectively).
*)

module type Key = Core_kernel.Map_intf.Key

(** A simple representation of a contiguous interval in some type. *)
module Interval : sig
  (** An interval on a key type which can be open (extend to ±infinity) at either end. *)
  type 'k t =
    [ `Always
    | `From of 'k
    | `Until of 'k
    | `Between of 'k * 'k ]
  [@@deriving sexp]

  (** Identify whether an interval is empty. *)
  val is_empty : 'k t -> cmp:('k -> 'k -> int) -> bool

  (** Check whether an interval contains a specific key. *)
  val contains : 'k t -> cmp:('k -> 'k -> int) -> 'k -> bool
end = struct
  type 'k t =
    [ `Always
    | `From of 'k
    | `Until of 'k
    | `Between of 'k * 'k ]
  [@@deriving sexp]

  let is_empty t ~cmp =
    match t with
    | `Always | `From _ | `Until _ -> false
    | `Between (min, max) -> cmp min max >= 0

  let contains t ~cmp k =
    match t with
    | `Always -> true
    | `From min -> cmp k min >= 0
    | `Until max -> cmp k max < 0
    | `Between (min, max) ->
      (cmp k min >= 0) && (cmp k max < 0)
end

(** The core operations of interval maps. *)
module type Operations = sig
  type ('k, +'v, 'cmp) t

  (** Note on complexities: As the mappings are for ranges, where complexities are
      given they are given in terms of variables (n, m) which are the number of change
      points that have been inserted into the sequence.
  *)

  (** Comparison works lexicographically pointwise across the whole sequence
      (from -infty to +infty).

      This means that [x = y ⇔ (∀k. find x k = find y k)].

      Complexity is O(n + m).

      Note that this is a normalising comparison (a change point which changes
      the value to the same value is treated as if it does not exist), which means
      that it is not entirely extensional. In particular, two equal sequences may
      be distinguished by converting to sexps or using [construct_preimage], both
      of which do not perform normalisation. In example:

      {[
        let module Int_interval_map = Interval_map.Make(Int) in
        let compare = Int_interval_map.compare Int.compare in
        let sexp_of_t = Int_interval_map.sexp_of_t Int.sexp_of_t in
        let list_preimage t =
          Sequence.to_list (Int_interval_map.construct_preimage t)
        in

        let x = Int_interval_map.always 42 in
        let y = Int_interval_map.change x ~at:0 42 in

        assert (compare x y = 0);
        assert (sexp_of_t x <> sexp_of_t y);
        assert (list_preimage x <> list_preimage y);
      ]}
  *)
  val compare : ('v -> 'v -> int) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> int

  (** Create a sequence with a specified far-leftmost value, and sequence of
      change points.

      Complexity is O(1), as the primary cost is in construction of the
      map which is passed into this function.

      e.g. suppose that
      [let x = create ~left_of_leftmost:"A"
                 ~value_right_of:Int.Map.of_alist_exn [
                   0, "B"; 2, "C"; 4, "D"; ]]

      Then [x] will be an interval map such that:
      [∀      k < 0. find x k = "A"]
      [∀ 0 <= k < 2. find x k = "B"]
      [∀ 2 <= k < 4. find x k = "C"]
      [∀ 4 <= k    . find x k = "D"]
  *)
  val create
    : left_of_leftmost:'v
    -> value_right_of:('k, 'v, 'cmp) Map.t
    -> ('k, 'v, 'cmp) t

  (** Create a sequence which has a single constant value across the whole
      sequence of keys.

      [find (always ~comparator x) k = x]

      O(1).
  *)
  val always : 'v -> comparator:('k, 'cmp) Comparator.t -> ('k, 'v, 'cmp) t

  (** Find the value associated with some point along the sequence of keys.

      O(log n).
  *)
  val find : ('k, 'v, 'cmp) t -> 'k -> 'v

  (** Insert a change point into the sequence of changes.

      The precise effect on the values along the sequence depends on what
      other change points are present, notionally inserting a change point
      means the value prior to this point is unchanged, then at this point
      the value becomes the supplied value, and then continues to be that value
      until the next change point which had already been inserted.

      If you want to control values directly within bounded intervals,
      [set_within] may be simpler to use.

      O(log n).
  *)
  val change : ('k, 'v, 'cmp) t -> at:'k -> 'v -> ('k, 'v, 'cmp) t

  (** Apply a function to all values within the sequence.

      [find (map x ~f) k = f (find x k)]

      O(n).
  *)
  val map : ('k, 'a, 'cmp) t -> f:('a -> 'b) -> ('k, 'b, 'cmp) t

  (** Apply a function to values from two sequences.

      [find (map2 x y ~f) = f (find x k) (find y k)]

      O(n + m).
  *)
  val map2 : ('k, 'a, 'cmp) t -> ('k, 'b, 'cmp) t
    -> f:('a -> 'b -> 'c) -> ('k, 'c, 'cmp) t

  (** Flatten a sequence of sequences.

      [find (flatten x) k = find (find x k) k]

      O(sum(k)) where k is the size/number of changes of each inner
      sequence, of which there will be n.
  *)
  val join : ('k, ('k, 'v, 'cmp) t, 'cmp) t -> ('k, 'v, 'cmp) t

  (** [remove_changes_within t interval] removes any change points within
      the specified interval.

      By removing these points of change, the value
      within the interval will become whatever the value already was outside
      the left-boundary of the interval.

      Some intervals are open on the left (e.g. [ `Always ] or [ `Until k ]),
      and in these cases the value in the interval will become [t.left_of_leftmost].

      Complexity is O(log(n) + n'), where n' is the number of change points
      within the specified interval (not the whole sequence).
  *)
  val remove_changes_within : ('k, 'v, 'cmp) t -> 'k Interval.t -> ('k, 'v, 'cmp) t

  (** [set_within t interval v] modifies the sequence so that all values
      within the specified interval are [v], and values outside the interval
      are not modified.

      [find (set_within t interval x) k =
        (if Interval.contains interval k then x else find t k)]

      Complexity is O(log(n) + n'), where n' is the number of change points
      within the specified interval (not the whole sequence).
  *)
  val set_within : ('k, 'v, 'cmp) t -> 'k Interval.t -> 'v -> ('k, 'v, 'cmp) t

  (** [map_within t interval ~f] modifies the sequence similarly to set_within,
      except that it applies a function to the range rather than a constant value
      (i.e. [map_within t interval ~f:(Fn.const x) = set_within t interval x]).

      Complexity is O(log(n) + n'), where n' is the number of change points
      within the specified interval (not the whole sequence).
  *)
  val map_within : ('k, 'v, 'cmp) t -> 'k Interval.t -> f:('v -> 'v) -> ('k, 'v, 'cmp) t

  (** Construct a preimage of the sequence. This is a series of pairs of a value
      and an interval of keys within which the sequence has that value.

      O(n).

      Importantly note that:
        1) A particular value may be output many times with different intervals.
        2) Each interval output will be unique and not overlap with any other.
        3) As noted above, this is one of the areas where extensionality breaks down.

      In example of the last point:

      {[
        let x = Int_interval_map.always 42 in
        let y = Int_interval_map.change x ~at:0 42 in
        let list_preimage t =
          Sequence.to_list (Int_interval_map.construct_preimage t)
        in
        assert (list_preimage x = [(42, `Always)]);
        assert (list_preimage y = [(42, `Until 0); (42, `From 0)];
      ]}
  *)
  val construct_preimage
    : ('k, 'v, 'cmp) t
    -> ('v * 'k Interval.t) Sequence.t
end

(* Something which has a map type. This is used when constructing Interval_map types so
   that the structures used are compatible with normal maps for that key type.

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
  type t [@@deriving sexp, compare]

  module Map : sig
    module Key : Comparator.S with type t = t

    type +'a t = (Key.t, 'a, Key.comparator_witness) Map.t
    [@@deriving compare, sexp]
  end
end

(** A standard incarnation of an interval map for some key type.

    The majority of the operations are commented in {!Operations}.
*)
module type S = sig
  type ('k, +'v, 'cmp) interval_map

  module Key : Comparator.S
  module Interval : sig
    type t = Key.t Interval.t

    val is_empty : t -> bool
    val contains : t -> Key.t -> bool
  end

  type +'a t = (Key.t, 'a, Key.comparator_witness) interval_map
  [@@deriving sexp, compare]

  include Applicative.S with type 'a t := 'a t
  include Monad.S with type 'a t := 'a t

  val create
    : left_of_leftmost:'a
    -> value_right_of:(Key.t, 'a, Key.comparator_witness) Map.t
    -> 'a t

  val always : 'a -> 'a t

  (** All values of type unit t are equal. *)
  val unit : unit t

  (* functions from {!Operations} adapted to this specific key type. *)
  val find : 'v t -> Key.t -> 'v
  val change : 'v t -> at:Key.t -> 'v -> 'v t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val remove_changes_within : 'v t -> Interval.t -> 'v t
  val set_within : 'v t -> Interval.t -> 'v -> 'v t
  val map_within : 'v t -> Interval.t -> f:('v -> 'v) -> 'v t
  val construct_preimage : 'v t -> ('v * Interval.t) Sequence.t
end

(** An incarnation of an interval map where the key type has been wrapped
    with [Left_boundary.t].

    The majority of the operations are further defined/explained in {!Operations}.
*)
module type S_with_boundary = sig
  type key

  module Left_boundary : sig
    type t = key Left_boundary.t [@@deriving sexp, compare]
    include Comparable.S with type t := t
  end

  include S with type Key.t = Left_boundary.t

  (** Finding the value for an unwrapped key in an interval map based on wrapped keys
      means searching for the value at the point [Inclusive k], because the point
      [Exclusive k] should not apply for keys equal to [k]. This can be very confusing,
      so [find' k] does this automatically.
  *)
  val find' : 'a t -> key -> 'a
end

module type M = sig
  include Operations

  module type S = S
    with type ('k, 'v, 'cmp) interval_map := ('k, 'v, 'cmp) t
  module type S_with_boundary = S_with_boundary
    with type ('k, 'v, 'cmp) interval_map := ('k, 'v, 'cmp) t

  (* [Make] creates an interval map which directly uses some key type.

     As we use the normal form of map type to construct this, we pass in
     the type with its map module, rather than just the type and a comparison
     function (this means we can use the same comparator_witness type).

     In this case the keys used always act as inclusive left-boundaries of a range.
  *)
  module Make (T : Type_with_map_module) : S
    with type Key.t = T.Map.Key.t
     and type Key.comparator_witness = T.Map.Key.comparator_witness

  (* Note on the sexp format of generated interval_map types:
     This type is not serialised as the above record, but rather in a manner similar to
     ['a Left_boundary.Map.t], for example:

     sexp_of_t (create
     ~left_of_leftmost:`Pre
     ~value_right_of:(Map.of_alist [Exc 1, `A; Inc 2, `B]))
     = (Pre ((Exc 1) A) ((Inc 2) B))
  *)

  (* [Make_with_boundary] wraps the supplied key up using [Left_boundary.t],
     before constructing the interval map.

     The use of [Key.t Left_boundary.t] allows additional convenience functions.

     This is kept separate from [Make] because there are cases when the presence of
     inclusive and exclusive boundaries makes things very complicated.
  *)
  module Make_with_boundary (Key : Key) : S_with_boundary
    with type key := Key.t
end
