(** Provides fast integer division for a fixed denominator. Note that there is a
    significant cost to creating the divisor and the speedup is woirth it only it the
    number of time [div] is called is large enough to justify the [create]. (The [id]
    benchmark is included to give a sense of the overhead implicit in each benchmark.)

    ┌──────────────────────────────────┬─────────────────┬─────────┬────────────┐
    │ Name                             │        Time/Run │ mWd/Run │ Percentage │
    ├──────────────────────────────────┼─────────────────┼─────────┼────────────┤
    │ [fast_int_div.ml:] id            │          3.14ns │         │            │
    │ [fast_int_div.ml:] Int.(/)       │         10.85ns │         │            │
    │ [fast_int_div.ml:] div           │          4.47ns │         │            │
    │ [fast_int_div.ml:] create        │        460.33ns │ 104.00w │            │
    └──────────────────────────────────┴─────────────────┴─────────┴────────────┘
*)
open Core.Std

type t with bin_io, sexp

(** See performance constraints in the comment above. *)
val create : denominator:int -> t

(** Gives the result of division given the numerator. *)
val div : int -> t -> int
