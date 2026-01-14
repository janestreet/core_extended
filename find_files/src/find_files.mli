(** Unix like [find]. * * Note: Unlike Unix [find], the functions in this module do not
    produce paths in * depth-first order. *)

(* Implements find (like the unix utility). Note that t is stateful both because
   filesystems themselves are highly stateful, and for performance reasons *)

open! Core
module Unix := Core_unix

type t
type file_info = Filename.t * Unix.stats

module Options : sig
  type error_handler =
    | Ignore
    | Print
    | Raise
    | Handle_with of (Filename.t -> exn -> unit)

  type t =
    { min_depth : int (** Equivalent to the [-minDepth] flag in unix [find]. *)
    ; max_depth : int option (** Equivalent to the [-maxDepth] flag in unix [find]. *)
    ; follow_links : bool (** Equivalent to [find -L]. *)
    ; on_open_errors : error_handler
    (** Applied to errors raised when calling {!Unix.opendir} on a file. *)
    ; on_stat_errors : error_handler
    (** Applied to errors raised when calling {!Unix.stat} or {!Unix.lstat} on a file. *)
    ; filter : (file_info -> bool) option
    (** Whether to include a given file or directory in output. For directories, this does
        not affect whether files under the directory are visited. *)
    ; skip_dir : (file_info -> bool) option
    (** Whether to visit the files under a given directory. *)
    ; relative_paths : bool
    (** Whether to return filepaths as relative to the base dir. *)
    }

  val default : t
  val ignore_errors : t
end

(** [create ?options dir] create a Find.t based in dir *)
val create : ?options:Options.t -> string -> t

(** [next t] return the next file from the collection of valid files in t or None if no
    more files remain *)
val next : t -> file_info option

(** [close t] drops all the resources associated with t. It is a mistake to attempt to use
    t again. Any Find.t will be automatically closed after the last file is read by any
    means. *)
val close : t -> unit

(** [iter t ~f] calls f on every file in t *)
val iter : t -> f:(file_info -> unit) -> unit

(** [fold t ~init ~f] folds f over the files in t *)
val fold : t -> init:'a -> f:('a -> file_info -> 'a) -> 'a

(** [to_list t] returns all of the remaining files in t as a list in the order they would
    have been returned by subsequent calls to next *)
val to_list : t -> file_info list

(** [find_all ?options dir] short for to_list (create ?options dir) *)
val find_all : ?options:Options.t -> string -> file_info list
