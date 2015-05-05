(** Line-by-line reading of a file.  A line buffer allows one to read
    one line of a file at a time, blocking until a line is available.
    Line buffers are distinct from Pervasives.read_line in that they
    "notice" new data arriving in the file more quickly. *)

(** The type of a line buffer. *)
type t

type error_type = Null_retry | Too_many_nulls | Exception of string * exn

type lnum = Known of int | Unknown

type result =
  | Success of lnum * string
  | Nothing_available
  | Error of error_type
  | Fatal_error of string * exn

exception File_truncated_or_deleted

(** Open a line buffer from the passed filename.  If [close_on_eof] is
    set, when [eof] is read, the file will be closed and reopened if
    necessary. if [follow_deletes] is set, then when [eof] is read linebuf
    will stat the file, and if it has been deleted and recreated it
    will open the new file.  If [eprint_nulls] is set, then when nulls are found,
    a warning message will be printed to stderr. [null_hack] specifies the
    behaviour of the linebuf upon reception of null characters in the file (as
    seen when tailing files over CIFS).
    Null hack options:
     `Off: don't check for nulls, just keep going.
     `Retry: close and reopen file when nulls are read from the file.
      If max_null_retries is reached, then pass the line with nulls.
     `Retry_then_fail: the same as retry, except that an exception is raised
      once max_null_retries is reached.
*)
val create :
     ?pos:Int64.t
  -> ?close_on_eof:bool
  -> ?null_hack:[ `Off | `Retry_then_fail | `Retry ]
  -> ?eprint_nulls:bool
  -> ?follow_deletes:bool
  -> ?signal_on_truncate_or_delete:bool
  -> string
  -> t

(** Closes the line buffer (and the underlying file).  *)
val close : t -> unit

(** Returns whether or not the line buffer is closed *)
val is_closed : t -> bool

(** Tries to read a line from the file.  If no more lines are available,
    returns [None]. *)
val try_read : t -> string option

(** [try_read_lnum] is like [try_read] except also provides the line number of the
    read line. *)
val try_read_lnum : t -> (lnum * string) option

(** Like try_read, except that it returns more verbose errors *)
val try_read_lnum_verbose : t -> result

(** Calls try_read every 0.01 seconds and returns when a line is available. *)
val read : t -> string

(** Seeks to the end of the file and blocks until another line is available -- this new
    line is not returned.  Successive return values of [try_read_lnum] and
    [try_read_lnum] will return [Unknown] as the current line number until
    [reset] is called *)
val tail : t -> unit

(** Same as [tail] except it may return before a new line is available on the file
    i.e. it (usually) doesn't block.  Note that this does interact with files in
    a fairly naive way, so there's no guarantee that it absolutely doesn't block.

    Note that when this functions is called, the next line that's read may be a partial
    line.  After that first line, only full lines will be read.
*)
val unsafe_tail : t -> unit

val name : t -> string

(** reopens the file and seeks to the beginning.  Also recovers the ability to get line
    numbers if [tail] has been called *)
val reset : t -> unit
