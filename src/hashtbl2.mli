open! Core

module type Key = sig
  type t [@@deriving compare, sexp_of]
  val hash : t -> int
end

(* A hashtbl keyed by 'key1 and then 'key2. *)
type ('key1, 'key2, 'data) t [@@deriving sexp_of]

include Invariant.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

val clear : (_, _, _) t -> unit

val add_exn    : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 -> 'data -> unit
val set        : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 -> 'data -> unit
val remove_exn : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 ->          unit

val remove_all1 : ('key1, 'key2, 'data) t -> 'key1 -> unit

val find : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 -> 'data option

val find1 : ('key1, 'key2, 'data) t -> 'key1 -> ('key2, 'data) Hashtbl.t option

val mem : ('key1, 'key2, 'data) t -> 'key1 -> 'key2 -> bool

(** [mem1 t key1] is true iff \exists key2 s.t. [find t key1 key2] is not None **)
val mem1 : ('key1, 'key2, 'data) t -> 'key1 -> bool

val iter : ('key1, 'key2, 'data) t -> f:('key1 -> 'key2 -> 'data -> unit) -> unit

val iter1
  : ('key1, 'key2, 'data) t
  -> f:('key1 -> ('key2, 'data) Hashtbl.t -> unit)
  -> unit

(** [iter_key2 t key1 ~f] is a no-op unless [mem1 t key1] **)
val iter_key2
  :  ('key1, 'key2, 'data) t -> 'key1 -> f:('key2 -> 'data -> unit) -> unit

module Make (Key1 : Key) (Key2 : Key) : sig

  type nonrec 'data t = (Key1.t, Key2.t, 'data) t
  [@@deriving sexp_of]

  include Equal.S1 with type 'a t := 'a t

  val create : unit -> 'data t

end
