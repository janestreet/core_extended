(* This is an internal module: it shouldn't be used outside of core_extended *)
open Core.Std

type t = [
  | `Backspace
  | `Tab
  | `Newline
  | `Char of char
  | `Up
  | `Down
  | `Left
  | `Right
  | `Home
  | `End
  | `Delete
  | `Eof
  | `Unknown_escape of (string*int option*int option) ]

val get : unit -> t
