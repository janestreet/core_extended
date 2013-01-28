open Core.Std
(** Extensions to [Core.Unix]. *)

val fork_exec :
  ?stdin:Unix.File_descr.t ->
  ?stdout:Unix.File_descr.t ->
  ?stderr:Unix.File_descr.t ->
  ?path_lookup:bool ->
  ?env:[ `Extend of (string * string) list
       | `Replace of (string * string) list ] ->
  ?working_dir:string ->
  ?setuid:int ->
  ?setgid:int ->
  string ->
  string list ->
  Pid.t
(** [fork_exec prog args ~stdin ~stdout ~stderr ~setuid ~setgid]
   forks a new process that executes the program
   in file [prog], with arguments [args]. The pid of the new
   process is returned immediately; the new process executes
   concurrently with the current process.

   The function raises EPERM if when using [set{gid,uid}] and the user id is
    not 0.

   The standard input and outputs of the new process are connected
   to the descriptors [stdin], [stdout] and [stderr].

   The close_on_exec flag is cleared from [stderr] [stdout] and [stdin] so it's
   safe to pass in fds with [close_on_exec] set.

   @param path_lookup if [true] than we use PATH to find the process to exec.
   @env specifies the environment the process runs in

   ERRORS:
    Unix.unix_error. This function should not raise EINTR; it will restart
    itself automatically.

   RATIONAL:
    [setuid] and [setgid] do not do a full id drop (e.g.: they save the id in
    saved id) when the user does not have the privileges required to setuid to
    anyone.

    By default all file descriptors should be set_closexec ASAP after being open
    to avoid being captured in parallel execution of fork_exec; resetting the
    closexec flag on the forked flag is a cleaner and more thread safe approach.

   BUGS:
    The capabilities for setuid in linux are not tied to the uid 0 (man 7
    capabilities). It is still fair to assume that under most system this
    capability is there IFF uid == 0. A more fine grain permissionning approach
    would make this function non-portable and be hard to implement in an
    async-signal-way.

    Because this function keeps the lock for most of its lifespan and restarts
    automatically on EINTR it might prevent the OCaml signal handlers to run in
    that thread.
*)

val seteuid : int -> unit

val setreuid : uid:int -> euid:int -> unit

val gettid : unit -> int

type statvfs = {
  bsize: int;                           (** file system block size *)
  frsize: int;                          (** fragment size *)
  blocks: int;                          (** size of fs in frsize units *)
  bfree: int;                           (** # free blocks *)
  bavail: int;                          (** # free blocks for non-root *)
  files: int;                           (** # inodes *)
  ffree: int;                           (** # free inodes *)
  favail: int;                          (** # free inodes for non-root *)
  fsid: int;                            (** file system ID *)
  flag: int;                            (** mount flags *)
  namemax: int;                         (** maximum filename length *)
} with sexp, bin_io

(** get file system statistics *)
external statvfs : string -> statvfs = "statvfs_stub"

(** get load averages *)
external getloadavg : unit -> float * float * float = "getloadavg_stub"

module Extended_passwd : sig
  open Unix.Passwd

  (** [of_passwd_line] parse a passwd-like line *)
  val of_passwd_line : string -> t option

  (** [of_passwd_line_exn] parse a passwd-like line *)
  val of_passwd_line_exn : string -> t

  (** [of_passwd_file] parse a passwd-like file *)
  val of_passwd_file : string -> t list option

  (** [of_passwd_file_exn] parse a passwd-like file *)
  val of_passwd_file_exn : string -> t list
end

