open Core.Std

(** Extensions to [Core.Float].*)

val pretty : ?on_negative:[ `Blow_up | `Normal | `Print_dir ]
  -> float
  -> string
(**
   pretty-print a float using no more than five characters, using abberviations
   k, m, g, t.

   if [on_negative] is not set to [`Normal] then the resulting is never over four
   chars but upon negative number we either:
   - raise a failure
   - or print ["<0"]
*)

val to_string_hum : float -> string

(**
   [order_of_magnitude_difference a b]
   by how many orders of magnitude do [a] and [b] differ?
   The return value is non-negative.

   examples:
   - order_of_magnitude_difference   11. 1001.     = 2
   - order_of_magnitude_difference 1001.   11.     = 2
   - order_of_magnitude_difference  131.   11.     = 1
   - order_of_magnitude_difference  200.    0.003  = 5
*)
val order_of_magnitude_difference : float -> float -> int

include Number.Verified_std with type repr = Float.t

module type Fraction = sig
  include S0 with type repr = Float.t
  val one : t
  val random : ?rng : Random.State.t -> unit -> t
end

module Fraction : Fraction with type t = private Float.t
module Fraction_unsafe : Fraction with type t = Float.t
