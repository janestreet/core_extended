open Core.Std

module Style : sig
  type t

  val ansi : t
  val no_formatting : t
  (* no ansi formatting, but prints "not ok:" for yellow or red and "ok:" for green *)
  val ok_or_not : t
end

module type T = sig
  val println        : string -> unit
  val redprintln     : string -> unit
  val yellowprintln  : string -> unit
  val greenprintln   : string -> unit
  val blueprintln    : string -> unit
  val inverseprintln : string -> unit
  val printfln        : ('a, out_channel, unit) format -> 'a
  val redprintfln     : ('a, out_channel, unit) format -> 'a
  val yellowprintfln  : ('a, out_channel, unit) format -> 'a
  val greenprintfln   : ('a, out_channel, unit) format -> 'a
  val blueprintfln    : ('a, out_channel, unit) format -> 'a
  val inverseprintfln : ('a, out_channel, unit) format -> 'a
end

type t = (module T)

val create :
  style:Style.t
  -> oc:Out_channel.t
  -> t

(* ansi on standard out *)
module S : T

