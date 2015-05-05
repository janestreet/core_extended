open Core.Std

(**
   Extensions to [Core.Core_int64]
*)

(** An int reprensting a file length

   Same as the int type but has a specific sexp converter so that you can
    write "10g". Suffixes are b,k,g,m...
    The sexp reader also accepts plain ints.*)
module Filesize : sig
  type t = Int64.t with bin_io,sexp
  val to_string : t -> string
  val of_string : string -> t
end

include Number.Verified_std with type repr = Core.Std.Int64.t
