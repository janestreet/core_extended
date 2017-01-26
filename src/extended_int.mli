include Number.Verified_std with type repr = Core.Int.t

(** Computes greatest common divisor for the given two integers, with convention
    that [gcd 0 0 = 0].

    Returns a nonnegative value unless one of the arguments is [Int.min_value]
    and the other is [Int.min_value] or 0, in which case [Int.min_value] is returned.
*)
val gcd : int -> int -> int

