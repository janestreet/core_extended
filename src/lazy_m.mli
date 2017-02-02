(** A non-threadsafe reimplementation of [Lazy] *)
open! Core.Interfaces

(** Lazy values reimplementation.

    There are two advantages to reimplementing the lazy module in pure
    Ocaml.  Advantage number one is speed: I've measured ~140 clocks to
    force a standard lazy value the first time, and ~80 clocks to force
    it the second time.  If the lazy computation you're avoiding is
    creating a simple cons cell, this is horribly expensive.  The following
    implementation is like ~30 clocks to force the lazy value the first
    time, and single-digit clocks to force it the second time.

    There are two downsides to doing it this way.  One, you can't use
    lazy keyword.  And two, this implementation uses a little more memory
    per lazy value (it currently uses 5 words/lazy value).

*)

type 'a t

(** Create a lazy value of a non-lazy value.  The lazy value created will
    be already forced, for obvious reasons. *)
val of_val : 'a -> 'a t

(** Create a lazy value of a function.  The function will not be executed
    until the lazy value is first forced, and will only be executed once.

    If the function raises an exception, all futures forces of the
    lazy value will also raise the same exception. *)
val of_fun : (unit -> 'a) -> 'a t

(** Force the lazy value.

    If the function that produces the value throws an exception, this
    function will throw the same exception. *)
val force : 'a t -> 'a

(** Returns true if the lazy value has been forced and did not throw an
    exception. *)
val is_val : _ t -> bool


include Monad with type 'a t := 'a t
