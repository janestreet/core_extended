(* This is an internal module: it shouldn't be used outside of core_extended*)
open Core.Std

type t

val create : ?text:string -> string list -> t
val print : prompt:string -> map_out:(string -> string) -> t -> unit
val contents : t -> string

type input = [ `Backspace
| `Char of char
| `Delete
| `Down
| `End
| `Eof
| `Home
| `Left
| `Right
| `Tab
| `Unknown_escape of (string*int option*int option)
| `Up ]

val step : ?completion:(left:string -> right:string -> string list) ->
  t -> input -> t
