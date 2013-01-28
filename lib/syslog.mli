(** Syslog Interface

    @author Markus Mottl <mmottl\@janestreet.com>
*)

(** {2 Options for opening syslog} *)

type opt =
  | PID  (** Include PID with each message *)
  | CONS  (**  Write directly to system console if there is an error
               while sending to system logger *)
  | ODELAY  (** Delay opening of the connection until syslog is called *)
  | NDELAY  (** No delay opening connection to syslog daemon *)
  | NOWAIT  (** Do not wait for child processes while logging message *)
  | PERROR  (** Print to stderr as well *)


(** {2 Types of syslog messages (facilities)} *)

type fac =
  | KERN  (** Kernel messages *)
  | USER  (** Generic user-level message (default) *)
  | MAIL  (** Mail subsystem *)
  | DAEMON  (** System daemons without separate facility value *)
  | AUTH  (** Security/authorization messages (DEPRECATED, use AUTHPRIV) *)
  | SYSLOG  (** Messages generated internally by syslogd *)
  | LPR  (** Line printer subsystem *)
  | NEWS  (** USENET news subsystem *)
  | UUCP  (** UUCP subsystem *)
  | CRON  (** Clock daemon (cron and at) *)
  | AUTHPRIV  (** Security/authorization messages (private) *)
  | FTP  (** FTP daemon *)
  | LOCAL0 | LOCAL1 | LOCAL2 | LOCAL3
  | LOCAL4 | LOCAL5 | LOCAL6 | LOCAL7 (** LOCAL0-7 reserved for local use *)


(** {2 Types of and functions for logging levels} *)

type lev =
  | EMERG  (** System is unusable *)
  | ALERT  (** Action must be taken immediately *)
  | CRIT  (** Critical condition *)
  | ERR  (** Error conditions *)
  | WARNING  (** Warning conditions *)
  | NOTICE  (** Normal, but significant, condition *)
  | INFO  (** Informational message *)
  | DEBUG  (** Debug-level message *)

val all_levs : lev array
(** [all_levs] array of all logging levels sorted in ascending order. *)

val all_str_levs : string array
(** [all_str_levs] array of all logging levels as strings sorted in
    ascending order. *)

val compare_lev : lev -> lev -> int
(** [compare_lev lev1 lev2] compares logging levels [lev1] and [lev2]. *)

val string_of_lev : lev -> string
(** [string_of_lev lev] converts a level [lev] to a string. *)

val lev_of_string : string -> lev
(** [lev_of_string str] converts string [str] to a level.
    @raise Failure if level does not exist. *)

val setlogmask :
  ?levs : lev list -> ?from_lev : lev -> ?to_lev : lev -> unit -> unit
(** [setlogmask ?levs ?from_lev ?to_lev ()] sets the log mask.  All levels
    in [levs] will be allowed, and additionally all ranging from
    [from_lev] to [to_lev] (inclusive).

    @param levs default = none
    @param from_lev default = [DEBUG]
    @param to_lev default = [EMERG]
*)


(** {2 Logging functions} *)

val openlog : ?id : string -> ?opt : opt list -> ?fac : fac -> unit -> unit
(** [openlog ?id ?opt ?fac ()] opens a connection to the system logger
    (possibly delayed) using prefixed identifier [id], options [opt],
    and faculty [fac].

    WARNING: this function leaks memory!  No way around that if syslog
    is called in a multi-threaded environment!  Therefore it shouldn't
    be called too often.  What for, anyway?

    @param id default = Sys.argv.(0)
    @param opt default = [[]]
    @param fac default = [USER]
*)

val syslog : ?fac : fac -> ?lev : lev -> string -> unit
(** [syslog ?fac ?lev str] logs message [str] using syslog with faculty
    [fac] at level [lev].

    @param fac default = as passed by [openlog]
    @param lev default = [INFO]
*)

val esyslog : ?fac : fac -> ?lev : lev -> string -> unit
(** [esyslog ?fac ?lev str] same as {!Syslog.syslog}, but also prints to
    [stderr]. *)

val syslog_printf :
  ?fac : fac -> ?lev : lev -> ('b, unit, string, unit) format4 -> 'b
(** [syslog_printf ?fac ?lev fmt] same as {!Syslog.syslog}, but allows
    [printf]-style specification of the message using [fmt]. *)

val esyslog_printf :
  ?fac : fac -> ?lev : lev -> ('b, unit, string, unit) format4 -> 'b
(** [esyslog_printf ?fac ?lev fmt] same as {!Syslog.syslog_printf},
    but also prints to [stderr]. *)

external closelog : unit -> unit = "closelog_stub" "noalloc"
(** [closelog ()] closes the connection to the syslog daemon. *)
