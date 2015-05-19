
(** ['a t] represents a generator for values of type ['a] with a specific probability
    distribution.  See [Kernel_generator] for a higher-level interface. *)
type 'a t

(** [bind t f] is a monadic bind that replaces each value [x] that has probability [p] in
    [t] with the probability distribution [f x] weighted proportionally by [p].
    [singleton], below, corresponds to monadic return. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** ['a Choice.t] represents a specific choice of ['a] value from an ['a t]. *)
module Choice : sig

  type 'a gen = 'a t
  type 'a t

  (** [gen t] produces the generator from which [t] is constructed. *)
  val gen : 'a t -> 'a gen

  (** [value t] produces the value chosen from [gen t]. *)
  val value : 'a t -> 'a

  (** These functions produce subsets of [gen t] using [value t] as a pivot.  The notions
      of "less than" and "greater than" are based on an arbitrary, deterministic ordering
      imposed by [gen t].

      [lt t] produces values from [gen t] that are less than [value t].
      [le t] produces values from [gen t] that are less than or equal to [value t].
      [ne t] produces values from [gen t] that are not equal to [value t].
      [ge t] produces values from [gen t] that are greater than or equal to [value t].
      [lt t] produces values from [gen t] that are greater than [value t]. *)
  val lt : 'a t -> 'a gen
  val le : 'a t -> 'a gen
  val ne : 'a t -> 'a gen
  val ge : 'a t -> 'a gen
  val gt : 'a t -> 'a gen

end with type 'a gen := 'a t

(** [bind_choice t f] is like [bind t f], except [f] is passed an ['a Choice.t] and can
    thus be constructed using subsets of [t], i.e. by using [Choice.lt].

    [bind t f] is equal to [bind_choice t (fun c -> f (Choice.value c))], although [bind]
    is cheaper than [bind_choice]. *)
val bind_choice : 'a t -> ('a Choice.t -> 'b t) -> 'b t

(** Empty generator. *)
val empty : 'a t

(** Constant generator.  [singleton t] is equal to [return t]. *)
val singleton : 'a -> 'a t

(** [weighted_union alist] produces a generator that combines the distributions of each
    [t] in [alist] with the associated weights, which must be finite positive floating
    point values. *)
val weighted_union : (float * 'a t) list -> 'a t

(** [of_fun f] produces a generator that lazily applies [f].

    [f] *MUST* be deterministic or else random value generation will fail.  However, it is
    recommended that [f] not be memoized.  Instead, spread out the work of generating a
    whole distribution over many [of_fun] calls combined with [weighted_union].  This
    allows lazily generated generators to be garbage collected after each test and the
    relevant portions cheaply recomputed in subsequent tests, rather than accumulating
    without bound over time. *)
val of_fun : (unit -> 'a t) -> 'a t

(** [choose t ~random_float_between_zero_and_one] chooses an element of [t] at random,
    using [random_float_between_zero_and_one] to produce random floats between
    0. (inclusive) and 1. (exclusive) for each weighted choice.  If [t] has been fully
    explored, it produces [`Empty].  If the random choice leads to an empty sub-tree of
    [t], it produces [`Failure t'] where [t'] is updated to never repeat that choice.
    Otherwise, it produces [`Success c] for some choice [c]. *)
val choose
  :  'a t
  -> random_float_between_zero_and_one:(unit -> float)
  -> [ `Empty
     | `Failure of 'a t
     | `Success of 'a Choice.t
     ]

(** [inspect t] produces a concrete representation of the outermost constructor of [t].
    It is possible to explore [t] further with recursive calls to [inspect]; however, be
    aware that [t] may be infinite. *)
val inspect
  :  'a t
  -> [ `Empty
     | `Singleton of 'a
     | `Weighted_union of (float * 'a t) list
     ]
