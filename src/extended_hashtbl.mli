open! Core

module Access_control : sig
  type ('key,'data,-'z) any
  module Immutable : sig
    type ('key,'data) t = ('key,'data,immutable) any
    include Sexpable.S2 with type ('key,'data) t := ('key,'data) t
   include Binable.S2 with type ('key,'data) t := ('key,'data) t
  end
  module Read_only : sig
    type ('key,'data) t = ('key,'data,read) any
    include Sexpable.S2 with type ('key,'data) t := ('key,'data) t
    include Binable.S2 with type ('key,'data) t := ('key,'data) t
  end
  module Read_write : sig
    type ('key,'data) t = ('key,'data, read_write) any
    include Sexpable.S2 with type ('key,'data) t := ('key,'data) t
    include Binable.S2 with type ('key,'data) t := ('key,'data) t
  end
  val of_hashtbl : ('key,'data) Hashtbl.t -> ('key,'data, [< _ perms]) any

  val clear : (_, _) Read_write.t -> unit
  val copy : ('a, 'b, [> read]) any -> ('a, 'b, [< _ perms]) any
  val fold :
    ('a, 'b, [> read]) any -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c

  val iter_keys : ('a, 'b, [> read]) any -> f:('a -> unit) -> unit
  val iter  : ('a, 'b, [> read]) any -> f:('b -> unit) -> unit
  val iteri : ('a, 'b, [> read]) any -> f:(key:'a -> data:'b -> unit) -> unit
  val iter_vals : ('a, 'b, [> read]) any -> f:('b -> unit) -> unit
    [@@deprecated "[since 2016-04] Use iter instead"]
  val existsi : ('a, 'b, [> read]) any -> f:(key: 'a -> data:'b -> bool) -> bool
  val exists : ('a, 'b, [> read]) any -> f:('b -> bool) -> bool
  val length : (_, _, _) any -> int
  val is_empty : (_, _, _) any -> bool
  val mem : ('a, _, [> read]) any -> 'a -> bool
  val remove : ('a, _) Read_write.t -> 'a -> unit
  val replace : ('a, 'b) Read_write.t -> key:'a -> data:'b -> unit
    [@@deprecated "[since 2017-06] Use set instead"]
  val set : ('a, 'b) Read_write.t -> key:'a -> data:'b -> unit
  val add : ('a, 'b) Read_write.t -> key:'a -> data:'b -> [ `Ok | `Duplicate ]
  val add_exn : ('a, 'b) Read_write.t -> key:'a -> data:'b -> unit
  val change : ('a, 'b) Read_write.t -> 'a -> f:('b option -> 'b option) -> unit
  val update : ('a, 'b) Read_write.t -> 'a -> f:('b option -> 'b) -> unit
  val add_multi : ('a, 'b list) Read_write.t -> key:'a -> data:'b -> unit
  val remove_multi : ('a, _ list) Read_write.t -> 'a -> unit
  val map : ('a, 'b, [> read]) any -> f:('b -> 'c) -> ('a, 'c, [< _ perms]) any
  val mapi
    :  ('a, 'b, [> read]) any
    -> f:(key:'a -> data:'b -> 'c)
    -> ('a, 'c, [< _ perms]) any

  val filter_map
    :  ('a, 'b, [> read]) any
    -> f:('b -> 'c option)
    -> ('a, 'c, [< _ perms]) any

  val filter_mapi
    :  ('a, 'b, [> read]) any
    -> f:(key:'a -> data:'b -> 'c option)
    -> ('a, 'c, [< _ perms]) any

  val filter_keys : ('a, 'b, [> read]) any -> f:('a -> bool) -> ('a, 'b, [< _ perms]) any
  val filter      : ('a, 'b, [> read]) any -> f:('b -> bool) -> ('a, 'b, [< _ perms]) any
  val filteri
    : ('a, 'b, [> read]) any -> f:(key:'a -> data:'b -> bool) -> ('a, 'b, [< _ perms]) any
  val find_or_add : ('a, 'b) Read_write.t -> 'a -> default:(unit -> 'b) -> 'b
  val find : ('a, 'b, [> read]) any -> 'a -> 'b option
  val find_exn : ('a, 'b, [> read]) any -> 'a -> 'b
  val merge :
    ('k, 'a, [> read]) any
    -> ('k, 'b, [> read]) any
    -> f:(key:'k -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c option)
    -> ('k, 'c, [< _ perms]) any

  type 'a merge_into_action = Remove | Set_to of 'a

  val merge_into
    :  src:('a, 'b, [> read]) any
    -> dst:('a, 'b) Read_write.t
    -> f:(key:'a -> 'b -> 'b option -> 'b merge_into_action)
    -> unit
  val keys : ('a, 'b, [> read]) any -> 'a list
  val data : ('a, 'b, [> read]) any -> 'b list
  val filter_inplace : ('a, 'b) Read_write.t -> f:('b -> bool) -> unit
  val filteri_inplace : ('a, 'b) Read_write.t -> f:(key:'a -> data:'b -> bool) -> unit
  val equal
    :  ('a, 'b, [> read]) any
    -> ('a, 'b, [> read]) any
    -> ('b -> 'b -> bool)
    -> bool
  val to_alist : ('a, 'b, [> read]) any -> ('a * 'b) list
  val incr : ?by:int -> ?remove_if_zero:bool -> ('a, int) Read_write.t -> 'a -> unit
end
