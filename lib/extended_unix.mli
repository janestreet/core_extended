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

(** Network to host order long, like C. Note that this is just a quick Binary_packing
pack/unpack of 32-bit unsigned ints, it raises for n > 4294967295 (0xFFFFFFFF) and n < 0.
This may be hilariously slow, bench it if you expect speed. *)
val ntohl : int -> int

(** Host to network order long, like C. Same caveats for ntohl above *)
val htonl : int -> int

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

external strptime : fmt:string -> string -> Unix.tm = "unix_strptime"


(** A representation of CIDR netmasks and functions to match if a given address is inside
 the range or not. *)
module Cidr : sig
  type t with sexp

  val address : t -> Unix.Inet_addr.t
  val bits : t -> int

  (** Generate a Cidr.t based on a string like "10.0.0.0/8". Addresses are not expanded
  (i.e. "10/8" is invalid. *)
  val of_string     : string -> t option
  val of_string_exn : string -> t

  val to_string : t -> string

  (** Is the given address inside the given Cidr.t? Note that the broadcast and network
  addresses are considered valid so match_ 10.0.0.0/8 10.0.0.0  is true. *)
  val match_    : t -> Unix.Inet_addr.t -> bool option
  val match_exn : t -> Unix.Inet_addr.t -> bool

  (* val inet_addr_to_int : Unix.Inet_addr.t -> int option *)
  val inet_addr_to_int_exn : Unix.Inet_addr.t -> int

  val address : t -> Unix.Inet_addr.t
  val bits : t -> int

  (** Some things (like the kernel) report addresses and ports as hex or decimal integers.
  Parse those . *)
  val inet4_addr_of_int_exn : int -> Unix.Inet_addr.t
end

(** Simple int wrapper to be explicit about ports. *)
module Inet_port : sig
  type t with sexp

  val of_int : int -> t option
  val of_int_exn : int -> t

  val of_string : string -> t option
  val of_string_exn : string -> t

  val to_int : t -> int
  val to_string: t -> string

end

(* MAC-48 (Ethernet) adddresses *)
module Mac_address : sig
  type t with sexp, bin_io
  val equal : t -> t -> bool
  (* Supports standard "xx:xx:xx:xx:xx:xx", "xx-xx-xx-xx-xx-xx", and cisco
     "xxxx.xxxx.xxxx" representations. *)
  val of_string : string -> t
  (* To standard representation "xx:xx:xx:xx:xx:xx".  Note the hex chars
     will be downcased! *)
  val to_string : t -> string
  (* To cisco representation "xxxx.xxxx.xxxx" *)
  val to_string_cisco : t -> string
end

module Quota : sig

  type bytes  = private Int63.t with sexp
  type inodes = private Int63.t with sexp

  val bytes  : Int63.t -> bytes
  val inodes : Int63.t -> inodes

  type 'units limit = {
    soft  : 'units option;
    hard  : 'units option;
    grace : Time.t option;
  } with sexp

  type 'units usage = private 'units

  val query
    :  [ `User | `Group ]
    -> id:int
    -> path:string
    -> ( bytes  limit * bytes usage
       * inodes limit * inodes usage) Or_error.t

  val set
    : [ `User | `Group ]
    -> id:int
    -> path:string
    -> bytes  limit
    -> inodes limit
    -> unit Or_error.t
end
