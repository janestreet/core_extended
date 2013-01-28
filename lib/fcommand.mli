open Core.Std

(** This module is a wrapper around Command that allows you to
    do away with accumulators. Here's an example:

    Fcommand.cmd ~summary:"Frobnicate something"
      Fcommand.(
        flag "-n" (required int) ~doc:"N number of times to frobnicate"
        ++ anon (args "item" string))
      (fun n items -> ...)
*)

(** [('main_in, 'main_out) t] is a type of composable command-line
    specifications.

    Every combinator for building [t]-values is polymorphic in
    ['main_out].  In the final specification expected by [cmd] used
    to construct a command, ['main_out] is specialized to [unit]).
    Various primitive specifications add parameters one at a time,
    so the resulting type of [main] is

      [arg1 -> ... -> argN -> unit]

    It may help to think of [('a, 'b) t] as a function space ['a -> 'b]
    embellished with extra information about how to parse command line,
    including documentation about what various flags are for.
*)
type ('main_in, 'main_out) t

(** argument parsing *)

type 'a parse = string -> 'a
val string : string parse
val int    : int parse
val float  : float parse
val date   : Date.t parse
val sexp   : Sexp.t parse

(** flags *)

module Flag : sig
  type 'a t (** a flag specification *)
  val map : f:('a -> 'b) -> 'a t -> 'b t
end
val required : 'a parse -> 'a Flag.t
val optional : 'a parse -> 'a option Flag.t
val optional_with_default : 'a -> 'a parse -> 'a Flag.t
val listed : 'a parse -> 'a list Flag.t
val no_arg : [`Present | `Absent] Flag.t
val no_arg_bool : bool Flag.t
val capture_remaining_command_line : string list option Flag.t
val flag : string -> 'a Flag.t -> doc:string -> ('a -> 'm, 'm) t

(** anonymous arguments *)

module Anons : sig
  type 'a t (** anonymous args specification *)
  val map : f:('a -> 'b) -> 'a t -> 'b t
end
val (%:)  : string -> 'a parse -> 'a Anons.t
val many  : string -> 'a parse -> 'a list Anons.t
val maybe : 'a Anons.t -> 'a option Anons.t
val maybe_with_default : 'a -> 'a Anons.t -> 'a Anons.t
val zero  : unit Anons.t
val t2 : 'a Anons.t -> 'b Anons.t -> ('a * 'b) Anons.t
val t3 : 'a Anons.t -> 'b Anons.t -> 'c Anons.t -> ('a * 'b * 'c) Anons.t
val t4 :
  'a Anons.t -> 'b Anons.t -> 'c Anons.t -> 'd Anons.t -> ('a * 'b * 'c * 'd) Anons.t
val anon  : 'a Anons.t -> ('a -> 'm, 'm) t

(** various combinators *)

(* [empty ()] is a no-op *)
val empty : unit -> ('a, 'a) t

(* [const v] injects the value [v] into main's parameters *)
val const : 'a -> ('a -> 'm, 'm) t

(** [spec1 ++ spec2] composes command-line specifications [spec1] and
    [spec2].  Parameters specified by [spec1] will come before those
    specified by [spec2] in the eventual main function. *)
val (++) : ('m1, 'm2) t -> ('m2, 'm3) t -> ('m1, 'm3) t

(* [either name ++ spec1 ++ spec2] ensures that at most one spec is supplied *)
val either : string -> ('a option -> 'b, 'a option -> 'a option -> 'b) t

(** [step] allows you to transform the way parameters are applied. For
    example, if you want labelled arguments, you can do:

    Fcommand.cmd ~summary:"..."
      Fcommand.(
        step (fun main x y z -> main ~x ~y ~z)
        ++ flag "-x" (optional string) ~doc:"..."
        ++ flag "-y" (required string) ~doc:"..."
        ++ flag "-z" (optional int) ~doc:"..."
      )
      (fun ~x ~y ~z ->
        ...
      )
*)
val step : ('m1 -> 'm2) -> ('m1, 'm2) t

(** constructing the command we've specified *)

val cmd :
  summary:string
  -> ?readme:(unit -> string)
  -> ?autocomplete:Command.Autocomplete.t
  -> ?global_flags:(unit Command.Flag.t list)
  -> ('main, unit) t
  -> 'main
  -> Command.t
