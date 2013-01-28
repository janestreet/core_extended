(** Utility functions concerning the OCaml-runtime

*)

open Core.Std;;

(** [running_byte_code ()] @return [true] when the program is being run
    in byte code, [false] when it is being executed as native code. *)
val running_byte_code : unit -> bool

val ls : string -> string list

val file_kind : string -> Unix.file_kind

(** Get the home of the effective user *)
val home : unit -> string

(** Get the names of the groups the user belongs to *)
val groups : unit -> string list

val hostname : unit -> string

(** [last_accessed path] returns the time [path] was last accessed.
  For files, the access time is updated whenever the file is read or executed.
  Note that some filesystems do not implement access time updates, or may allow
  mounting with access time updates disabled.
*)
val last_accessed     : ?follow_symlinks:bool -> string -> Time.t option
val last_accessed_exn : ?follow_symlinks:bool -> string -> Time.t

(** [last_modify path] returns the time the file at [path] was last modified.
  For files, the modify time is updated whenever [path] is written to, or if
  its status attributes are updated.
 *)
val last_modified     : ?follow_symlinks:bool -> string -> Time.t option
val last_modified_exn : ?follow_symlinks:bool -> string -> Time.t

(** [last_changed path] returns the time [path] last had its status changed.
  This is not the same as last modified, as the last status change will also
  be updated if [path]'s ownership or permissions change.
 *)
val last_changed      : ?follow_symlinks:bool -> string -> Time.t option
val last_changed_exn  : ?follow_symlinks:bool -> string -> Time.t

val file_size     : ?follow_symlinks:bool -> string -> Int64.t option
val file_size_exn : ?follow_symlinks:bool -> string -> Int64.t

(** [scroll_lock false] disables scroll locking. *)
val scroll_lock : bool -> unit ;;
