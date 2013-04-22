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
  | Int_datum   of int * int   * Time_stamp_counter.Cycles.t
  | Float_datum of int * float * Time_stamp_counter.Cycles.t
  with sexp, bin_io

  val read_msg_and_snapshot : file:string -> string * Time_stamp_counter.Cycles.snapshot
  val fold : file:string -> init:'a -> f:('a -> t -> 'a) -> 'a
end
