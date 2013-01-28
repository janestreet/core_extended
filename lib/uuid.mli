(* Implements universally unique identifiers based on version 3 of the UUID specification.
   Identifier generation is thread safe, and fast.
*)

open Core.Std

type t

include Sexpable             with type t := t
include Stringable           with type t := t
include Comparable.S_binable with type t := t
include Hashable.S_binable   with type t := t

val create : unit -> t
