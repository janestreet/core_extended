open Core_extended.Std

type t

val create : lock_file:string -> name:string -> ('a, unit) Deprecated_fcommand.t -> 'a -> t
val start   : t -> Deprecated_command.t
val stop    : t -> Deprecated_command.t
val status  : t -> Deprecated_command.t
val restart : t -> Deprecated_command.t
val group   : t -> Deprecated_command.t

