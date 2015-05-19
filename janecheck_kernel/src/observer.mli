
(** An ['a t] represents an observer that maps values of type ['a] to disjoint subsets
    ("buckets") using a finite number of observations. *)
type 'a raw
type 'a t = 'a raw Generator.t

(** [singleton ()] maps all values to a single bucket. *)
val singleton : unit -> 'a t

(** [unmap t ~f] applies [f] to values before observing them using [t]. *)
val unmap : 'a t -> f:('b -> 'a) -> f_sexp:Sexplib.Sexp.t -> 'b t

(** Observers for binary tuple and variant types. *)
val tuple   : 'a t -> 'b t -> ('a * 'b) t
val variant : 'a t -> 'b t -> [ `A of 'a | `B of 'b ] t

(** [of_fun f] produces an observer that lazily applies [f].

    It is recommended that [f] should not do a lot of expensive work and should not be
    memoized.  Instead, spread out the work of generating an observer over many [of_fun]
    calls combined with, e.g., [variant] or [tuple].  This allows lazily generated
    observers to be garbage collected after each test and the relevant portions cheaply
    recomputed in subsequent tests, rather than accumulating without bound over time. *)
val of_fun : (unit -> 'a t) -> 'a t

(** [observe t gen ~sexp_of_rng] constructs a generator for a function type using [t] to
    observe the domain and [gen] to generate the range. *)
val observe
  :  'a t
  -> 'b Generator.t
  -> sexp_of_rng:('b -> Sexplib.Sexp.t)
  -> (('a -> 'b) * Sexplib.Sexp.t) Generator.t
