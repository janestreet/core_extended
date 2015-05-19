open Core_kernel.Std

(** An ['a Observer.t] represents observations that can be made to distinguish values of
    type ['a]. *)

type 'a t   = 'a Janecheck_kernel.Std.Observer.t
type 'a gen = 'a Janecheck_kernel.Std.Generator.t

include module type of Janecheck_kernel.Std.Observer
  with type 'a t := 'a t

(** Observers for basic types. *)
val unit   : unit     t
val bool   : bool     t
val int    : int      t
val float  : float    t
val string : string   t
val char   : char     t
val sexp   : Sexp.t   t

(** [doubleton f] maps values to two buckets, depending on whether they satisfy [f]. *)
val doubleton : ('a -> bool) -> f_sexp:Sexp.t -> 'a t

(** [enum n ~f] maps values to [n] buckets, where [f] produces the index for a bucket
    from [0] to [n-1] for each value. *)
val enum : int -> f:('a -> int) -> f_sexp:Sexp.t -> 'a t

(** [of_list list ~equal] maps values in [list] to separate buckets, and compares
    observed values to the elements of [list] using [equal]. *)
val of_list : 'a list -> equal:('a -> 'a -> bool) -> sexp_of_elt:('a -> Sexp.t) -> 'a t

(** Observers for option and list types. *)
val option : 'a t -> 'a option t
val list   : 'a t -> 'a list   t

(** Fixed point observer. *)
val recursive : ('a t -> 'a t) -> 'a t

(** Observer for [Either.t]. *)
val either : 'a t -> 'b t -> ('a, 'b) Either.t t

(** Observers for polymorphic variants (similar to [variant], but n-ary). *)
val variant3
  :  'a t -> 'b t -> 'c t
  -> [ `A of 'a | `B of 'b | `C of 'c ] t
val variant4
  :  'a t -> 'b t -> 'c t -> 'd t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd ] t
val variant5
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e ] t
val variant6
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e | `F of 'f ] t

(** Observers for tuples (beyond [tuple] from [Observer]). *)
val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
val tuple6
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  -> ('a * 'b * 'c * 'd * 'e * 'f) t

(** [of_predicate t1 t2 ~f] combines [t1] and [t2], where [t1] observes values that
    satisfy [f] and [t2] observes values that do not satisfy [f]. *)
val of_predicate : 'a t -> 'a t -> f:('a -> bool) -> f_sexp:Sexp.t -> 'a t

(** [comparison ~compare ~eq ~lt ~gt] combines observers [lt] and [gt], where [lt]
    observes values less than [eq] according to [compare], and [gt] observes values
    greater than [eq] according to [compare]. *)
val comparison
  :  compare:('a -> 'a -> int)
  -> eq:'a
  -> lt:'a t
  -> gt:'a t
  -> compare_sexp:Sexp.t
  -> sexp_of_eq:('a -> Sexp.t)
  -> 'a t

(** Observer for function types, based on a generator for inputs and an observer for
    outputs. *)
val fn
  :  'a Generator.t
  -> 'b t
  -> sexp_of_dom:('a -> Sexplib.Sexp.t)
  -> ('a -> 'b) t

type 'a bound = [ `Inclusive of 'a | `Exclusive of 'a | `Unbounded ]

(** [int_between ~lower_bound ~upper_bound] observes integers within the given bounds. *)
val int_between
  :  lower_bound:int bound
  -> upper_bound:int bound
  -> int t
