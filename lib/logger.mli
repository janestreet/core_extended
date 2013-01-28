(** Another logging library.*)
open Core.Std;;

type message = [ `Fatal | `Error | `Warn | `Info | `Debug ] * string
type size = [ `Kb of int64 | `Mb of int64 | `Unlimited ]
type 'messagetype t
type default_t = message t

(** 
  - max_size - the maximum size of each log file (default 50Mb)
  - mode - mode to open the files in
  - max_archives - maximum number of archives to keep (default 4)
  - full_callback - called when the logger queue backs up so far that log items
                    may have been lost.  Defaults to doing nothing.
  - filter - if set, then every message is passed to filter before actually being
             logged.  If filter returns false the message is dropped.
  - message_to_string - called to convert your message type to a string for logging
*)
val create : ?max_size:size -> 
  ?mode:Unix.file_perm -> 
  ?max_archives:[`Max of int | `Unlimited] -> 
  ?full_callback:(string Squeue.t -> unit) ->
  ?filter:('messagetype -> bool) ->
  message_to_string:('messagetype -> string) -> 
  string -> 'messagetype t
  
(** creates a log using the default message type and a filter that drops 
  `Debug messages *)  
val create_default : ?max_size:size -> 
  ?mode:Unix.file_perm -> 
  ?max_archives:[`Max of int | `Unlimited] -> 
  ?full_callback:(string Squeue.t -> unit) -> 
  string -> message t

(** logs a message to log *)
val log : 'messagetype t -> 'messagetype -> unit

(** Sets the filter for a log *)
val filter : 'messagetype t -> ('messagetype -> bool) -> unit

(** removes the filter from a log *)
val clear_filter : 'messagetype t -> unit


(** misc helper functions *)

(** logs a message to Some log, returns silently if log is None *)
val maybe_log : 'messagetype t option -> 'messagetype -> unit

(** Returns a timestamp as a string suitable for log files *)
val timestamp : unit -> string

