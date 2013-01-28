open Core.Std

type 'data t

val empty   : 'data t
val find    : 'data t -> string -> 'data option
val add     : 'data t -> key:string -> data:'data -> 'data t
val remove  : 'data t -> string -> 'data t
val iter    : 'data t -> f:(key:string -> data:'data -> unit) -> unit
val map     : 'data t -> f:('data -> 'b) -> 'b t
val fold    : 'data t -> init:'b -> f:(key:string -> data:'data -> 'b -> 'b) -> 'b
