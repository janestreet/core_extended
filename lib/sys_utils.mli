open Core.Std

(** Various system utility functions. *)

(** Get the default editor (program) for this user.
    This functions checks the EDITOR and VISUAL environment variables and then
    looks into a hard coded list of editors.
*)
val get_editor : unit -> string option

val get_editor_exn : unit -> string

(** Analogous to [get_editor], defaulting to a list including less and more. *)
val get_pager : unit -> string option

(** [page_contents ?pager ?tmp_name str] will show str in a pager. The optional
    [tmp_name] parameter gives the temporary file prefix. The temporary file is removed
    after running. *)
val page_contents :
  ?pager:string
  -> ?pager_options:string list
  -> ?tmp_name:string
  -> string
  -> unit

val pid_alive : int -> bool

val get_groups : string -> string list

(** [with_tmp pre suf f] creates a temp file, calls f with the file name, and removes the
    file afterwards.  *)
val with_tmp : pre:string -> suf:string -> (string -> 'a) -> 'a

(** [diff s1 s2] returns diff (1) output of the two strings. Identical files returns the
    empty string. *)
val diff : ?options : string list -> string -> string -> string

(** [get_ip_from_hostname ()] returns the IP of the current host by resolving the
    hostname. *)
val getbyname_ip : unit -> string

(** [ifconfig_ips ()] returns IPs of all active interfaces on the host by parsing ifconfig
    output. Note that this will include 127.0.0.1, assuming lo is up. *)
val ifconfig_ips : unit -> String.Set.t

(**
   [checked_edit ~check file]

   Edit a file in safe way, like [vipw(8)]. Launches your default editor on
   [file] and uses [check] to check its content.

   @param check a function returning a text representing the error in the file.
   @param create create the file if it doesn't exists. Default [true]
   @return [`Ok] or [`Abort]. If [`Abort] is returned the files was not modified
   (or created).
*)
val checked_edit :
  ?create:bool
  -> check:(string -> string option)
  -> string
  -> [ `Abort | `Ok ]

(** Edit files containing sexps. *)
module Sexp_checked_edit (S:Sexpable): sig
  val check : string -> string option
  val check_sexps : string -> string option

  val edit :
    ?create:bool
    -> string
    -> [ `Abort | `Ok ]

  val edit_sexps :
    ?create:bool
    -> string
    -> [ `Abort | `Ok ]

end

(** A module for getting a process's CPU use. *)
module Cpu_use : sig
  type t
  (** [get] returns a Cpu_use.t for the given pid or the current processes's pid, if none
      given. Note that the [cpu_use] given by this initial value will probably be
      invalid. *)
  val get : ?pid : Pid.t -> unit -> t
  (** [update_exn] updates a Cpu_use.t. It will fail if the process no longer exists. *)
  val update_exn : t -> unit
  (** [cpu_use] gives an estimated CPU usage (between 0 and 1) in the time period between
      the last 2 calls to [update_exn]. *)
  val cpu_use : t -> float
end
