open Core_kernel.Std

(** An ['a Generator.t] represents a probability distribution for values of type ['a]. *)

type 'a t   = 'a Janecheck_kernel.Std.Generator.t
type 'a obs = 'a Janecheck_kernel.Std.Observer.t

include module type of Janecheck_kernel.Std.Generator
  with type 'a t := 'a t

(** Generators form a monad.  [t1 >>= fun x -> t2] replaces each value [x] in [t1] with
    the values in [t2]; each value's probability is the product of its probability in
    [t1] and [t2].

    This can be used to form distributions of related values.  For instance, the following
    expression creates a distribution of pairs [x,y] where [x <= y]:

    {[
      int
      >>= fun x ->
      int_between
        ~lower_bound:(`Inclusive x)
        ~upper_bound:(`Inclusive Int.max_value)
      >>| fun y ->
      x, y
    ]}
*)
include Monad.S with type 'a t := 'a t

module Infix : sig
  include module type of Monad_infix

  (** The operators below augment [t >>= fun x -> e] by passing an additional argument
      along with [x] representing a subset of the distribution [t].  The operator names
      are suggestive of the subsets: [>>=/<] passes a subset of values that are "less
      than" [x], [>>=/<=] passes a subset of values that are "less than or equal to" [x],
      and so forth.  The ordering used for "less than" and "greater than" is based on the
      construction of [t]; it is deterministic, but not guaranteed to coincide with any
      comparison function.

      These operators can be written in terms of [bind_point]:
      [t >>=/< f]  = [bind_point t (fun x p -> f x (point_lt p))]
      [t >>=/> f]  = [bind_point t (fun x p -> f x (point_gt p))]
      [t >>=/<= f] = [bind_point t (fun x p -> f x (point_le p))]
      [t >>=/>= f] = [bind_point t (fun x p -> f x (point_ge p))]
      [t >>=/<> f] = [bind_point t (fun x p -> f x (point_ne p))]
  *)
  val (>>=/<)  : 'a t -> ('a -> 'a t -> 'b t) -> 'b t
  val (>>=/>)  : 'a t -> ('a -> 'a t -> 'b t) -> 'b t
  val (>>=/<=) : 'a t -> ('a -> 'a t -> 'b t) -> 'b t
  val (>>=/>=) : 'a t -> ('a -> 'a t -> 'b t) -> 'b t
  val (>>=/<>) : 'a t -> ('a -> 'a t -> 'b t) -> 'b t
end

include module type of Infix

(** Default generators for basic types. *)
val unit   : unit     t
val bool   : bool     t
val int    : int      t
val float  : float    t
val string : string   t
val char   : char     t
val sexp   : Sexp.t   t

(** Generators for subsets of ASCII characters. *)
val char_digit      : char t
val char_lowercase  : char t
val char_uppercase  : char t
val char_alpha      : char t
val char_alphanum   : char t
val char_print      : char t
val char_whitespace : char t

(** Generator for strings, based on a generator for characters. *)
val string_of : char t -> string t

(** Generators for finite sets of values. *)
val singleton : 'a -> 'a t
val doubleton : 'a -> 'a -> 'a t
val of_list : 'a list -> 'a t

(** Combine arbitary generators, weighted equally. *)
val union : 'a t list -> 'a t

(** Generator for the values from a potentially infinite sequence. *)
val of_sequence : 'a Sequence.t -> 'a t

(** Generators for tuples. *)
val tuple  : 'a t -> 'b t -> ('a * 'b) t
val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
val tuple6
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  -> ('a * 'b * 'c * 'd * 'e * 'f) t

(** Generator for the Either type. *)
val either : 'a t -> 'b t -> ('a, 'b) Either.t t

(** Generators for polymorphic variant types. *)
val variant  : 'a t -> 'b t -> [ `A of 'a | `B of 'b ] t
val variant3 : 'a t -> 'b t -> 'c t -> [ `A of 'a | `B of 'b | `C of 'c ] t
val variant4
  :  'a t -> 'b t -> 'c t -> 'd t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd ] t
val variant5
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e ] t
val variant6
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e | `F of 'f ] t

(** Generator for the option type. *)
val option : 'a t -> 'a option t

(** Generators for functions; take observers for inputs and a generator for outputs. *)
val fn  : 'a obs -> 'b t -> ('a -> 'b) t
val fn2 : 'a obs -> 'b obs -> 'c t -> ('a -> 'b -> 'c) t
val fn3 : 'a obs -> 'b obs -> 'c obs -> 'd t -> ('a -> 'b -> 'c -> 'd) t
val fn4 : 'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e t -> ('a -> 'b -> 'c -> 'd -> 'e) t
val fn5
  :  'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e obs -> 'f t
  -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) t
val fn6
  :  'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e obs -> 'f obs -> 'g t
  -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) t

(** Generator for comparison functions; result is guaranteed to be a total order. *)
val compare_fn : 'a obs -> ('a -> 'a -> int) t

(** Generator for equality functions; result is guaranteed to be an equivalence
    relation. *)
val equal_fn : 'a obs -> ('a -> 'a -> bool) t

(** Generators for functions with sexps. *)
val compare_fn_with_sexp : 'a obs -> (('a -> 'a -> int)  * Sexp.t) t
val equal_fn_with_sexp   : 'a obs -> (('a -> 'a -> bool) * Sexp.t) t
val fn_with_sexp
  :  'a obs -> 'b t
  -> sexp_of_rng:('b -> Sexp.t)
  -> (('a -> 'b) * Sexp.t) t
val fn2_with_sexp
  :  'a obs -> 'b obs -> 'c t
  -> sexp_of_rng:('c -> Sexp.t)
  -> (('a -> 'b -> 'c) * Sexp.t) t
val fn3_with_sexp
  :  'a obs -> 'b obs -> 'c obs -> 'd t
  -> sexp_of_rng:('d -> Sexp.t)
  -> (('a -> 'b -> 'c -> 'd) * Sexp.t) t
val fn4_with_sexp
  :  'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e t
  -> sexp_of_rng:('e -> Sexp.t)
  -> (('a -> 'b -> 'c -> 'd -> 'e) * Sexp.t) t
val fn5_with_sexp
  :  'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e obs -> 'f t
  -> sexp_of_rng:('f -> Sexp.t)
  -> (('a -> 'b -> 'c -> 'd -> 'e -> 'f) * Sexp.t) t
val fn6_with_sexp
  :  'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e obs -> 'f obs -> 'g t
  -> sexp_of_rng:('g -> Sexp.t)
  -> (('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) * Sexp.t) t

(** [filter t ~f] produces [y] for every [x] in [t] such that [f x = Some y]. *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** [filter t ~f] produces every [x] in [t] such that [f x = true]. *)
val filter : 'a t -> f:('a -> bool) -> 'a t

(** Fixed-point generator.  For example, the following produces a naive generator for
    natural numbers:

    {[
      recursive (fun t ->
        union [ singleton 0 ; t >>| Int.succ ])
    ]}
*)
val recursive : ('a t -> 'a t) -> 'a t

(** Generator for lists of a given type.

    [list t] produces a generator for arbitrary lists of values from [t].

    Adding [~unique:true] guarantees that every value from [t] is included in each list at
    most once.

    Adding [~length:(`Exactly n)] produces only lists of length [n].
    Adding [~length:(`At_least n)] produces only lists of length [n] or greater.
    Adding [~length:(`At_most n)] produces only lists of length [n] or less.
    Adding [~length:(`Between_inclusive (m,n))] produces only lists of length [k] such
    that [m <= k] and [k <= n].

    Adding [~sorted:`Arbitrarily] produces lists that are sorted in a deterministic order
    based on the construction of [t], but not guaranteed to correspond to any specific
    comparison function.

    Adding [~sorted:(`By cmp)] produces lists that are sorted in ascending order by [cmp].

    The optional arguments can be combined; for example, the following expression creates
    lists of 10 to 20 integers each that are strictly sorted (no duplicates):

    {[
      list int ~unique:true ~sorted:(`By Int.compare) ~length:(`Between_inclusive (10,20))
    ]}

    Regardless of the options provided, the lists in the output of [list t] are generated
    uniquely, so long as the values in [t] are generated uniquely. *)
val list
  :  ?length:[ `Exactly           of int
             | `At_least          of int
             | `At_most           of int
             | `Between_inclusive of int * int ]
  -> ?unique:bool
  -> ?sorted:[ `Arbitrarily | `By of ('a -> 'a -> int) ]
  -> 'a t
  -> 'a list t

(** [permute list] generates all permutations of [list].  If [list] contains duplicate
    values, then [permute list] will produce duplicate lists. *)
val permute : 'a list -> 'a list t

(** [size] is a probability distribution of non-negative integers weighted toward small
    numbers, and therefore suitable for generating random data structures. *)
val size : int t

type 'a bound = [ `Inclusive of 'a | `Exclusive of 'a | `Unbounded ]

(** [int_between ~lower_bound ~upper_bound] produces integers within the given bounds.
    The distribution is *NOT* uniform, even when both ends are bounded; it is weighted
    toward potential boundary conditions. *)
val int_between
  :  lower_bound:int bound
  -> upper_bound:int bound
  -> int t

(** [float_between_inclusive ~with_nan ~lower_bound ~upper_bound] produces floating point
    numbers within the given bounds.

    If [with_nan=`None], no values are produced satisfying [Float.is_nan].  If
    [with_nan=`Single], only the single value [Float.nan] is produced satisfying
    [Float.is_nan].  If [with_nan=`All], the distribution includes all possible floating
    point values satisfying [Float.is_nan]. *)
val float_between
  :  with_nan:[ `None | `Single | `All ]
  -> lower_bound:float bound
  -> upper_bound:float bound
  -> float t
