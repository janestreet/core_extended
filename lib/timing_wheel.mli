(** A timing wheel is a data structure that maintains a clock with the current time and a
    set of alarms scheduled to fire in the future.  One can add and remove alarms, and
    advance the clock to cause alarms to fire.  There is nothing asynchronous about a
    timing wheel.  Alarms only fire in response to an [advance_clock] call.

    When one [create]s a timing wheel, one supplies an initial time, [start], and an
    [alarm_precision].  When an alarm [a] fires on a timing wheel [t], the implementation
    guarantees that:

    |  Alarm.at a <= now t

    That is, alarms never fire early.  Furthermore, the implementation guarantees that
    alarms don't go off too late.  There can be alarms in [t] such that [Alarm.at a < now
    t], but those alarms cannot be more than [alarm_precision] before [now t].  Although
    it is morally true that for all alarms [a]:

    |  Alarm.at a > now t - alarm_precision

    it is not quite true due to floating point imprecision.

    Also, an [advance_clock] call can advance the clock to arbitrary time in the future,
    and thus alarms may fire at a clock time arbitrarily far beyond the time for which
    they were set.  But the implementation has no control over the times supplied to
    [advance_clock]; it can only guarantee that alarms will fire when [advance_clock] is
    called with a time at least [alarm_precision] greater than their scheduled time.

    Representable times
    ===================
    A timing wheel [t] can only represent a (typically large) bounded range of times as
    determined by the [level_bits] and [alarm_precision] arguments supplied to create.
    Various functions return [Error] if they are supplied an unrepresentable time.  This
    likely indicates a misconfiguration of the [level_bits] and/or [alarm_precision].

    Implementation
    ==============
    The timing wheel is implemented using a specialized priority-queue in which
    [add_alarm] and [remove_alarm] are constant time, and [advance_clock] takes time
    proportional to the amount of time the clock is advanced.  With a sufficient number of
    alarms, this is more efficient than a log(N) heap implementation of a priority queue.

    In essence, all time from [start] onwards is broken into half-open intervals of size
    [alarm_precision], with the intervals assigned indices 0, 1, 2, etc.  The indices are
    used as keys in the priority queue.  All alarms with times in the same interval get
    the same key, and hence are fired at the same time.  More specifically, an alarm is
    fired when the clock reaches or passes the time at the start of the next interval.
*)

open Core.Std

module Alarm : sig
  type 'a t with sexp_of

  val at : _ t -> Time.t
  val value : 'a t -> 'a
end

type 'a t with sexp_of

include Invariant.S1 with type 'a t := 'a t

(** [create ~start ~alarm_precision ~dummy ()] creates a new timing wheel with current
    time [start].  The [dummy] value is a performance optimization; it would be a bug if
    the timing wheel ever supplied the [dummy] value to client code.

    [create] returns [Error] if [alarm_precision <= 0].

    One can use [level_bits] to trade off run time and space usage.  [create] will return
    [Error] if [level_bits] is invalid.  For details, see [Priority_queue.create]
    below.

    For a fixed [level_bits], a smaller (i.e. more precise) [alarm_precision] decreases
    the representable range of times and increases the constant factor for
    [advance_clock].
*)
val create
  :  ?level_bits:int list
  -> start:Time.t
  -> alarm_precision:Time.Span.t
  -> dummy:'a
  -> unit
  -> 'a t Or_error.t

(** Accessors *)
val alarm_precision : _ t -> Time.Span.t
val now             : _ t -> Time.t
val start           : _ t -> Time.t

(** One can think of a timing wheel as a set of alarms.  Here are various container
    functions along those lines. *)
val is_empty : _ t -> bool
val length : _ t -> int
val iter_alarms : 'a t -> f:('a Alarm.t -> unit) -> unit

(** [interval_start t time] returns the largest time not greater than [time] that is of
    the form [start t + k * alarm_precision t].  [interval_start] returns [Error] if [time
    < start t] or [time] is too far in the future to represent. *)
val interval_start : _ t -> Time.t -> Time.t Or_error.t

(** [advance_clock t ~to_ ~handle_fired] advances [t]'s clock to [to_].  It fires all
    alarms [a] in [t] with [Time.(<) (Alarm.at a) (interval_start t to_)] applying
    [handle_fired] to each such [a].

    If [Time.(<=) to_ (now t)], then [advance_clock] does nothing and returns [Ok ()].

    [advance_clock] returns [Error] if [to_] is too far in the future to represent.  *)
val advance_clock : 'a t -> to_:Time.t -> handle_fired:('a -> unit) -> unit Or_error.t

(** [max_alarm_at t] returns the maximum [at] that can be supplied to [add_alarm].
    [max_alarm_at] is not constant; its value increases as [now t] increases. *)
val max_alarm_at : _ t -> Time.t

(** [add_alarm t ~at value] adds a new alarm [a] to [t].  [add_alarm] returns
    [`Not_in_the_future] if [Time.(<=) at (now t)].  [add_alarm] returns [Error] if
    [at >= max_alarm_at t]. *)
val add_alarm
  :  'a t
  -> at:Time.t
  -> 'a
  -> [ `Added of 'a Alarm.t
     | `Not_in_the_future
     ] Or_error.t

val add_alarm_exn : 'a t -> at:Time.t -> 'a -> 'a Alarm.t

(** [remove_alarm t alarm] removes [alarm] from [t] and returns [Ok ()], if [alarm] is
    present in [t].  [remove_alarm] returns [Error] if [alarm] is not in [t]. *)
val remove_alarm : 'a t -> 'a Alarm.t -> unit Or_error.t

(** [next_alarm_at t] returns [Alarm.at a] of of the earliest alarm [a] in [t], or [None]
    if [t] has no alarms. *)
val next_alarm_at : _ t -> Time.t option


(** At the heart of a timing wheel is a priority queue in which the keys are non-negative
    integers corresponding to the intervals of time.  The priority queue is unlike a
    typical priority queue in that rather than having a "delete min" operation, it has a
    nondecreasing minimum allowed key, which corresponds to the current time, and an
    [increase_min_allowed_key] operation, which implements [advance_clock].
    [increase_min_allowed_key] as a side effect removes all elements from the timing wheel
    whose key is smaller than the new minimum, which implements firiing the alarms whose
    time has expired.

    Adding elements to and removing elements from a timing wheel takes constant time,
    unlike a heap-based priority queue which takes log(N), where N is the number of
    elements in the heap.  [increase_min_allowed_key] takes time proportional to the
    amount of increase in the min-allowed key, as compared to log(N) for a heap.  It is
    these performance differences that motivate the existence of timing wheels and make
    them a good choice for maintaing a set of alarms.  With a timing wheel, one can
    support any number of alarms paying constant overhead per alarm, while paying a small
    constant overhead per unit of time passed.

    This implementation of timing wheels uses "levels", where level i is an array of
    length [2^b_i] (the [b_i] are configurable as an argument to [create], or one can use
    the default baked into the code).  As the minimum allowed key increases, the timing
    wheel does a lazy radix sort of the element keys, with level 0 handling the least
    significant [b_0] bits in a key, and each subsequent level [i] handling the next most
    significant [b_i] bits.  The levels hold increasingly larger ranges of keys, where the
    union of all the levels can hold any key from [min_allowed_key t] to [max_allowed_key
    t].  When a key is added to the heap, it is added at the lowest possible level that
    can store the key.  As the minimum allowed key increases, timing-wheel elements move
    down levels until they reach level 0, and then are eventually removed.
*)
module Priority_queue : sig

  module Elt : sig
    (** An [Elt.t] represents an element that was added to a timing wheel. *)
    type 'a t with sexp_of

    val invariant : 'a Invariant.t -> 'a t Invariant.t

    val key : _ t -> int
    val value : 'a t -> 'a
  end

  type 'a t with sexp_of

  val invariant : ('a -> unit) -> 'a t -> unit

  (** [create ?level_bits ~dummy ()] creates a new empty timing wheel, [t], with [length t =
      0] and [min_allowed_key t = 0].

      [dummy] is a dummy value that will never be returned by any operation, but that
      allows the implementation to be more efficient.

      [level_bits] allows one to fine tune the performance constants of the timing wheel.
      See the description of levels above.  As the number of levels increases, the length of
      the levels decreases and the timing wheel uses less space, but the constant factor for
      the running time of [add] and [increase_min_allowed_key] increases.

      [level_bits] also determines the range of allowed keys in the timing wheel.

      It is an error if any of the [b_i] in [level_bits] has [b_i <= 0], or if the sum of
      the [b_i] in [level_bits] is greater than [Int.num_bits - 2].  The bound comes from
      the fact that nonnegative integers in OCaml need [Int.num_bits - 1] bits, and by
      disallowing an additional bit (the top bit), the implementation is simplified by not
      having to deal with overflow. *)
  val create
    :  ?level_bits:int list
    -> dummy:'a
    -> unit
    -> 'a t Or_error.t

  (** [length t] returns the number of elements in the timing wheel. *)
  val length : _ t -> int

  (** [is_empty t] is [length t = 0] *)
  val is_empty : _ t -> bool

  (** To avoid issues with arithmetic overflow, the implementation restricts keys to being
      between [0] and [max_representable_key], where [max_representable_key =
      Int.max_value lsr 1]. *)
  val max_representable_key : int

  (** [min_allowed_key t] is the minimum key that can be stored in [t].  This only
      indicates the possibility; there need not be an element [elt] in [t] with [Elt.key
      elt = min_allowed_key t].  This is not the same as the "min_key" operation in a
      typical priority queue.

      [min_allowed_key t] can increase over time, via calls to [increase_min_allowed_key].
      It is guaranteed that [min_allowed_key t <= max_representable_key]. *)
  val min_allowed_key : _ t -> int

  (** [max_allowed_key t] is the maximum allowed key that can be stored in [t].  As
      [min_allowed_key] increases, so does [max_allowed_key]; however it is not the case
      that [max_allowed_key t - min_allowed_key t] is a constant.  It is guaranteed that
      [max_allowed_key t >= min (max_representable_key, min_allowed_key t + 2^B - 1],
      where [B] is the sum of the b_i in [level_bits].  It is also guaranteed that
      [max_allowed_key t <= max_representable_key]. *)
  val max_allowed_key : _ t -> int

  (* [min_elt t] returns an element in [t] that has the minimum key, if [t] is nonempty.
     [min_elt] takes time proportional to the size of the timing-wheel data structure in
     the worst case.  It is implemented via a linear search. *)
  val min_elt : 'a t -> 'a Elt.t option
  val min_key :  _ t ->      int option

  (** [add t ~key value] adds a new element to [t].  The element returned can later be
      supplied to [remove] to remove the element from the timing wheel.  [add] returns
      [`Key_too_small] if [key < min_allowed_key t].

      [add] returns [Error] if [key > max_allowed_key t]. *)
  val add : 'a t -> key:int -> 'a -> [ `Added of 'a Elt.t | `Key_too_small ] Or_error.t

  (** [contains t elt] is true iff [elt] is in [t].*)
  val contains : 'a t -> 'a Elt.t -> bool

  (** [remove t elt] removes [elt] from [t].  It is an error if [not (contains t elt)]. *)
  val remove : 'a t -> 'a Elt.t -> unit Or_error.t

  (** [increase_min_allowed_key t ~key ~handle_removed] increases the minimum allowed key
      in [t] to [key], and removes all elements with keys less than [key], applying
      [handle_removed] to each element that is removed.  If [key <= min_allowed_key t],
      then [increase_min_allowed_key] does nothing and returns [Ok ()].  Otherwise, if
      [increase_min_allowed_key] returns successfully, [min_allowed_key t = key] and all
      removed elements satisfy [not (contains t elt)].

      [increase_min_allowed_key] returns [Error] if [key > max_representable_key].

      [increase_min_allowed_key] takes time proportional to [key - min_allowed_key t],
      although possibly less time. *)
  val increase_min_allowed_key
    : 'a t -> key:int -> handle_removed:('a Elt.t -> unit) -> unit Or_error.t

  val iter_elt : 'a t -> f:('a Elt.t -> unit) -> unit
  val iter     : 'a t -> f:('a       -> unit) -> unit
end
