(** [Runtime_blockout_detector] is used detect bugs in C libraries that fail
    to release the OCaml runtime lock before making blocking calls. *)
open Core.Std

(** [start] starts a thread that watches for blockouts where the OCaml
    lock isn't released.  By default, it prints a message to stderr. *)
val start : ?callback:(elapsed:Time.Span.t -> unit) -> unit -> unit
