(** reservoir sampling *)

open! Core

type 'a t (** a random sample of ['a] values *)

(** [create ~random_state desired_sample_size] creates an empty sample of ['a] values.
    The sample will grow no larger than [desired_sample_size] when presented with more
    values by calling [add]. *)
val create : ?random_state:Random.State.t -> int -> 'a t

(** the desired sample size *)
val desired_sample_size : 'a t -> int

(** [maybe_add t x] will randomly either add [x] to [t] or ignore it. If adding [x]
    would grow the sample larger than [desired_sample_size t], some previously selected
    value will be discarded. *)
val maybe_add : 'a t -> 'a -> unit

(** the current selection from values previously seen by [t].  Of all previously seen
    values, each subset of size [desired_sample_size t] is equally likely to have
    been selected. *)
val to_list : 'a t -> 'a list

(** randomly select a subset of size [sample_size] from a stream of unknown length.
    Each possible subset is chosen with equal probability. *)
val select : ?random_state:Random.State.t -> next:(unit -> 'a option) -> int -> 'a list

(** number of things [maybe_add]ed so far, no matter they're actually added or ignored *)
val num_seen : _ t -> int

(** [clear t] clears contents of [t] and resets [num_seen]. It doesn not reset random
    state *)
val clear : _ t -> unit

