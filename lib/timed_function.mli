open Core.Std

(**
   Functions with timeouts

   This module is here to add timeouts to any functions.
*)
exception Timeout

(** Runs a function in a fork process to ensure a timeout.
    The function passed must not raise an exception not have any weird
    side effects.
*)
val run : timeout:float
  -> f:('a -> 'b)
  -> sexp_of:('b -> Sexp.t)
  -> of_sexp:(Sexp.t -> 'b)
  -> 'a
  -> 'b
