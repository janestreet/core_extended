(** The objective of [Stats_reporting] is reporting numbers from a running process
    effeciently with minimum disruption to its execution time. The collected numbers can
    be analyzed offline after the process has exited.

    [Stats_reporting] acts as a fine-grained performance analysis tool by letting us
    measure user specified stats. For instance one might want to report the length of a
    list, the number of elements of a hashtable, the code path taken, the latency between
    two points in code etc. and understand how large or small these values get, various
    quantiles, histograms etc. The tool for offline analysis is called [stats.exe] and
    lives in [tool/stats].

    Production binaries should not be built with [Stats_reporting] enabled.
*)
open Core.Std

(** [init] should be called early in program execution, probably in [main]. Once called,
    the process will write out a data file when the program exits or when the
    [memory_limit] is reached. If another process writes to the same data file, the file
    will be corrupt. The data file is named [stats.data] by default and the [msg],
    typically a version string, is written out to it. *)
val init : msg:string -> ?memory_limit:int -> ?file_name:string -> unit -> unit

(** [adjust_internal_buffers_if_required] is meant to be called outside any performance
    critical parts of your code.  The point of this call is that it will do any upcoming
    housekeeping tasks so that they don't happen in performance critical bits of the
    code.

    [adjust_internal_buffers_if_required] will (1) allocate additional memory if the
    module's internal buffers are close to being full and (2) write out data collected
    thus far if it is close to the specified [memory_limit]. *)
val adjust_internal_buffers_if_required : unit -> unit

(** Values are reported using [Field.t]s. *)
module Field : sig
  type 'a t = private int with sexp, bin_io

  (** [create] creates an abstract field whose value will be reported at different points
      in time. Fields are typically global to a module. *)
  val create : desc:string -> type_desc:string -> name:string -> 'a t

  (** [add_datum] and [add_datum_float] report data changes. *)
  val add_datum       : int t -> int -> unit
  val add_datum_float : float t -> float -> unit
end

(** [Delta] makes it easy to record deltas of values. For instance, if one wants to record
    the number of minor words allocated in some part of code one would do first create a
    [Delta.t] and then write the following to report the value of [minor_words2 -
    minor_words1]:

    {[
        set_datum t minor_words1;

        ... code to test ...;

        add_delta t minor_words2;
    ]}
*)
module Delta : sig
  type 'a t

  val create : 'a Field.t -> 'a t

  (** [set t v1] sets the current value to [v]. [v] is not reported -- [add_delta] or
      [add_delta_and_set] must be subsequently called.  [add_delta t v2] causes [v2-v1] to
      be reported, for the latest [set t v1].  [add_delta_and_set t v2] is [add_delta
      t v2] followed by [set t v2]. *)
  val set               : int t -> int -> unit
  val add_delta         : int t -> int -> unit
  val add_delta_and_set : int t -> int -> unit

  (** Similar to the [int] functions above. *)
  val set_float               : float t -> float -> unit
  val add_delta_float         : float t -> float -> unit
  val add_delta_and_set_float : float t -> float -> unit
end

module Field_id : sig
  type t = private int

  include Identifiable with type t := t
  include Intable      with type t := t
end

(** [Reader] is meant to be used by code that reads [stats.data] files. *)
module Reader : sig
  type type_desc = string with sexp, bin_io
  type name = string with sexp, bin_io
  type t =
  | New_field   of Field_id.t * string * type_desc * name
  | Int_datum   of Field_id.t * int   * Time_stamp_counter.t
  | Float_datum of Field_id.t * float * Time_stamp_counter.t
  with sexp, bin_io

  val read_msg_and_snapshot : file:string -> string * Time_stamp_counter.Calibrator.t
  val fold : file:string -> init:'a -> f:('a -> t -> 'a) -> 'a
end
