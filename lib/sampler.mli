open Core.Std

(** compiled finite discrete probability distributions supporting constant
    time sampling *)
type 'a t

(** [create dist] compiles a discrete probability distribution into a form
    supporting constant-time sampling.  The running time is O(N) where N =
    [List.length dist].  [dist] may be either a probability distribution
    (all floats are non-negative and sum to 1) or, more generally,
    a histogram in which frequencies are interpreted as probabilities. *)
val create : ('a * float) list -> 'a t

(** take a sample from the distribution in constant time *)
val sample : ?state:Random.State.t -> 'a t -> 'a

