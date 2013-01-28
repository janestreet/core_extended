(**
   Atomically edit a file without long-term locking

   See documentation for the function [atomic_edit]
   *)
open Core.Std

(** the return value of [atomic_edit]
*)
type return_type =
  | Ok
  | Changed of string * string
  | Abort

(** [atomic_edit ~f filepath] edit [filepath] by making a unique copy (peer)
    that can be openly changed without holding any locks on the original. When
    the function returns, a short term exclusive lock is held while overwriting
    the original with the edited copy. If the mtime of the original is changed
    since the copy was made, the replacement is aborted (indicating another
    [atomic_edit] was first to update the file, or some other process) and
    Changed is returned with a tuple [(warning message * terd file)]. The terd
    file contains the edits and might be used to diff or re-edit.
*)
val atomic_edit : f:(string -> [ `Ok | `Abort ] ) -> string -> return_type
