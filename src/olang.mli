(**
   The language of predicates over an ordered set.
*)

type 'a t = [
  | `GT of 'a * 'a           (** sexp [(x > y)] *)
  | `LT of 'a * 'a           (** sexp [(x < y)] *)
  | `GE of 'a * 'a           (** sexp [(x >= y)] *)
  | `LE of 'a * 'a           (** sexp [(x <= y)] *)
  | `EQ of 'a * 'a           (** sexp [(x = y)] *)
  | `NE of 'a * 'a           (** sexp [(x <> y)] *)
  | `One_of of 'a * 'a list  (** sexp [(x one-of (a b c))] *)
] [@@deriving bin_io, sexp, compare]

val eval : compare:('a -> 'a -> int) -> 'a t -> bool
