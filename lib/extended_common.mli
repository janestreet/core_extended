(** Pervasive functions. *)

val run_main : (unit -> unit) -> _

(** [write_wrap ~atomic ~f fname] Runs [f] on an [out_channel]. If [mode] is
    [`Atomic] or [`Atomic_update] is set all the changes will be written to a
    temporary file which will then be moved over [fname] otherwise we are
    writing straight to [fname].

    Values for [mode]:
    - [`Clobber]: clear the file on opening (this is the default value)
    - [`Append]: append to the file
    - [`Atomic]: replace the file atomically when we are done writing it
    - [`Atomic_update]: replace the file atomically when we are done writing it
    iff its content has been modified.
*)
val write_wrap :
 ?mode:[`Clobber|`Append|`Atomic|`Atomic_update]
 -> f:(out_channel -> 'a)
 -> string
 -> 'a
