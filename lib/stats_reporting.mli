open Core.Std

type field with sexp, bin_io

(* If memory limit is specified then Stats_reporting will limit its memory usage to the
   specified limit. When the amount of collected data exceeds the limit, the data is
   written to disk. If another process writes to the same data file, the file will be
   corrupt. *)
val init            : msg:string -> ?memory_limit:int -> ?file_name:string -> unit -> unit
val create_field    : desc:string -> type_desc:string -> name:string -> field
val add_datum       : field -> int -> unit
val add_datum_float : field -> float -> unit

(* The following functions make it easy to record deltas of values. For instance if one
   wants to record the number of minor words allocated in some part of code one would do
   first create a [Delta.t] and then:

   [[
      set_datum t minor_words1;
      ... code to test ...
      add_delta t minor_words2;
   ]]

   This will emit the value [minor_words2 - minor_words1] to the output output. If
   [add_delta] is not called, the value during [set_datum] will be lost. In essence, these
   functions abstract the subtraction and are convenient when [minor_words1] and
   [minor_words2] are not available in the same scope. Calling [add_delta] does not change
   the last set value.
*)
module Delta : sig
  type t
  val create : field -> t

  val set         : t -> int -> unit
  val add_delta         : t -> int -> unit
  val add_delta_and_set : t -> int -> unit

  val set_float         : t -> float -> unit
  val add_delta_float         : t -> float -> unit
  val add_delta_and_set_float : t -> float -> unit
end

(* This function is meant to be called outside any performance critical parts of your
   code.  This call will allocate additional memory if the stats buffer is slose to being
   full. This will also write out data will a memory limit is specified and the amount of
   collected data is close to the limit.  *)
val adjust_if_required : unit -> unit


(* The rest of this interface is meant to be used by code that reads stats.data files. *)
module Stored_data : sig
  type type_desc = string with sexp, bin_io
  type name = string with sexp, bin_io
  type t =
  | New_field   of int * string * type_desc * name
  | Int_datum   of int * int   * Time_stamp_counter.t
  | Float_datum of int * float * Time_stamp_counter.t
  with sexp, bin_io

  val read_msg_and_snapshot : file:string -> string * Time_stamp_counter.Calibrator.t
  val fold : file:string -> init:'a -> f:('a -> t -> 'a) -> 'a
end
