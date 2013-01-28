(* heterogeneous multi-maps *)


module Key : sig
  type 'a t
  val create : unit -> 'a t
end

type t

val empty : t

val add : t -> 'a Key.t -> 'a -> t

val find : t -> 'a Key.t -> 'a option

val find_all : t -> 'a Key.t -> 'a list

