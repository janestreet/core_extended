open! Core

module type Key = Hashtbl2.Key

(* A pair of hashtbls, one keyed by 'key1 then 'key2, the other keyed by 'key2 then
   'key1. *)
type ('key1, 'key2, 'data) t [@@deriving sexp_of]

include Invariant.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

val clear : (_, _, _) t -> unit

val add_exn : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 -> 'data -> unit
val set     : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 -> 'data -> unit

val find : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 -> 'data option
val mem  : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 -> bool
val mem1 : ('key1, 'key2, 'data) t -> 'key1          -> bool
val mem2 : ('key1, 'key2, 'data) t          -> 'key2 -> bool

val iter : ('key1, 'key2, 'data) t -> f:('key1 -> 'key2 -> 'data -> unit) -> unit

val iter1
  : ('key1, 'key2, 'data) t
  -> f:('key1 -> ('key2, 'data) Hashtbl.t -> unit)
  -> unit

val iter2
  : ('key1, 'key2, 'data) t
  -> f:('key2 -> ('key1, 'data) Hashtbl.t -> unit)
  -> unit

val find1 : ('key1, 'key2, 'data) t -> 'key1 -> ('key2, 'data) Hashtbl.t option
val find2 : ('key1, 'key2, 'data) t -> 'key2 -> ('key1, 'data) Hashtbl.t option

val find1_iter2 : ('key1, 'key2, 'data) t -> 'key1 -> f:('key2 -> 'data -> unit) -> unit
val find2_iter1 : ('key1, 'key2, 'data) t -> 'key2 -> f:('key1 -> 'data -> unit) -> unit

val remove_all1 : ('key1, _, _) t -> 'key1 -> unit
val remove_exn  : ('key1, 'key2, _) t -> 'key1 -> 'key2 -> unit

module Make (Key1 : Key) (Key2 : Key) : sig

  type nonrec 'data t = (Key1.t, Key2.t, 'data) t
  [@@deriving sexp_of]

  include Equal.S1 with type 'a t := 'a t

  val create : unit -> 'data t

  val of_alist_exn : (Key1.t * Key2.t * 'data) list -> 'data t

end

