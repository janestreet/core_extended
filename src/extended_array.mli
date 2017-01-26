(** Extensions to [Core.Core_array]. *)
open! Core

(** makes a random split & subset of an array; p (the fraction that you want to split) is
   constrained to be [0, 1].  Note that the length of the first array will be the closest
   integer to the fraction you desired, meaning that each element is NOT selected with
   probability exactly p. *)
val random_split : ?random_state:Random.State.t -> 'a array -> p:float -> 'a array * 'a array
val random_sub   : ?random_state:Random.State.t -> 'a array -> p:float -> 'a array
