(**
   File descriptor leak check.

   This mod
*)

(** Toggle to turn on/off checking for descriptor leaks at exit (default: off)
*)
val run_check_at_exit : bool ref

(** Fraction of maximum number of descriptors considered critical.
    Default: 0.9 *)
val critical : float ref

(** [report_open_files ()] prints a dump of open file descriptors to [stderr]
    in two formats, one using the proc file system, the other by executing
    [/usr/sbin/lsof] in a child process. *)
val report_open_files : unit -> unit

(** [report_on_exn exn] calls {!report_open_files} iff [exn] indicates a file
    descriptor leak (Unix error with code EMFILE or ENFILE). *)
val report_on_exn : exn -> unit

(** [percent_fds_in_use ()] reports the percentage of fds that are in use. *)
val percent_fds_in_use : unit -> float
