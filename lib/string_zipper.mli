open Core.Std

type t

val contents       : t -> string

val left_contents  : t -> string
val right_contents : t -> string

val insert_before  : t -> char -> t
val insert_after   : t -> char -> t
val drop_before    : t -> (char * t) option
val drop_after     : t -> (char * t) option
val drop_all_before : t -> (char list * t) option
val drop_all_after : t -> (char list * t) option
val previous       : t -> t option
val next           : t -> t option
val first          : t -> t
val last           : t -> t

val replace_left   : t -> string -> t
val replace_right  : t -> string -> t
val create         : string -> string -> t
