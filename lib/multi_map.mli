(**
   Maps with mutliple bindings.

   This is a map that allows multiple binding. Each key can have several values
   associated to it.
*)
open Core.Std

type ('k,'v) t
include Sexpable.S2 with type ('a,'b) t := ('a,'b) t

val empty     : ('k,'v) t
val singleton : 'k -> 'v -> ('k,'v) t
val is_empty  : ('k,'v) t -> bool
val add       : key:'k -> data:'v -> ('k, 'v) t -> ('k, 'v) t

val find      : ('k, 'v) t -> 'k -> 'v list
  (** [find m key] returns all the elements that where added to [key] in [m] in
      the reverse order in which they where added. If no element where added an
      empty list is returned.*)

val remove    : ('k, 'v) t -> 'k -> ('k, 'v) t
  (** [remove m key] Remove all the values associated the key [key] in [m]*)

val set       : key:'k -> data:'v list -> ('k, 'v) t -> ('k, 'v) t

val mem       : ('k, 'v) t -> 'k -> bool
  (** [mem m key] returns true if [key] has at last one value associated to it
      in [m] *)

val keys      : ('k, 'v) t -> 'k list
  (** Returns all the non-empty keys in [m]. *)

val iter      :
  f:(key:'k -> data:'v -> unit)
  -> ('k, 'v) t
  -> unit

val map: f:('a -> 'b) -> ('k,'a) t -> ('k,'b) t

val mapi: f:(key:'k -> data:'a -> 'b) -> ('k,'a) t -> ('k,'b) t

val fold      :
  f:(key:'k -> data:'v -> 'a -> 'a)
  -> ('k, 'v) t
  -> init:'a
  -> 'a

val filter : f:(key:'k -> data:'v -> bool) -> ('k,'v) t -> ('k,'v) t

val reduce : f:('v list -> 'r) -> ('k,'v) t -> ('k,'r) Map.Poly.t

(*
val data      : ('a, 'b) t -> 'b list
val to_alist  : ('a, 'b) t -> ('a * 'b list) list
val of_list   : ('a * 'b) list -> ('a, 'b) t
val for_all   : f:('a list -> bool) -> ('b, 'a) t -> bool
val exists    : f:('a list -> bool) -> ('b, 'a) t -> bool
val to_map    : ('a, 'b) t -> ('a, 'b list) Map.t
val of_map    : ('a, 'b list) Map.t -> ('a, 'b) t
  *)
