open Core_extended.Std

type t

val create : lock_file:string -> name:string -> ('a, unit) Fcommand.t -> 'a -> t
val start   : t -> Command.t
val stop    : t -> Command.t
val status  : t -> Command.t
val restart : t -> Command.t
val group   : t -> Command.t

