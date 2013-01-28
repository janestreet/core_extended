open Core.Std

module Access_control : sig
  type ('key,'data,+'z) any
  module Immutable : sig
    type ('key,'data) t = ('key,'data,immutable) any
    include Sexpable.S2 with type ('key,'data) t := ('key,'data) t
    include Binable.S2 with type ('key,'data) t := ('key,'data) t
  end
  module Read_only : sig
    type ('key,'data) t = ('key,'data,read_only) any
    include Sexpable.S2 with type ('key,'data) t := ('key,'data) t
    include Binable.S2 with type ('key,'data) t := ('key,'data) t
  end
  module Read_write : sig
    type ('key,'data) t = ('key,'data, read_write) any
    include Sexpable.S2 with type ('key,'data) t := ('key,'data) t
    include Binable.S2 with type ('key,'data) t := ('key,'data) t
  end
  val of_hashtbl : ('key,'data) Hashtbl.t -> ('key,'data,_) any

  val clear : (_, _) Read_write.t -> unit
  val copy : ('a, 'b, _) any -> ('a, 'b, _) any
  val fold :
    ('a, 'b,_) any -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val iter : ('a, 'b, _) any -> f:(key:'a -> data:'b -> unit) -> unit
  val existsi : ('a, 'b, _) any -> f:(key: 'a -> data:'b -> bool) -> bool
  val exists : ('a, 'b, _) any -> f:('b -> bool) -> bool
  val length : (_, _, _) any -> int
  val is_empty : (_, _, _) any -> bool
  val mem : ('a, _, _) any -> 'a -> bool
  val remove : ('a, _) Read_write.t -> 'a -> unit
  val remove_one : ('a, _ list) Read_write.t -> 'a -> unit
  val replace : ('a, 'b) Read_write.t -> key:'a -> data:'b -> unit
  val set : ('a, 'b) Read_write.t -> key:'a -> data:'b -> unit
  val add : ('a, 'b) Read_write.t -> key:'a -> data:'b -> [ `Ok | `Duplicate ]
  val add_exn : ('a, 'b) Read_write.t -> key:'a -> data:'b -> unit
  val change : ('a, 'b) Read_write.t -> 'a -> ('b option -> 'b option) -> unit
  val add_multi : ('a, 'b list) Read_write.t -> key:'a -> data:'b -> unit
  val map : ('a, 'b, _) any -> f:('b -> 'c) -> ('a, 'c, _) any
  val mapi : ('a, 'b, _) any -> f:(key:'a -> data:'b -> 'c) -> ('a, 'c, _) any
  val filter_map : ('a, 'b, _) any -> f:('b -> 'c option) -> ('a, 'c, _) any
  val filter_mapi : ('a, 'b, _) any -> f:(key:'a -> data:'b -> 'c option) -> ('a, 'c, _) any
  val filter : ('a, 'b, _) any -> f:('b -> bool) -> ('a, 'b, _) any
  val filteri : ('a, 'b, _) any -> f:(key:'a -> data:'b -> bool) -> ('a, 'b, _) any
  val find_or_add : ('a, 'b, _) any -> 'a -> default:(unit -> 'b) -> 'b
  val find : ('a, 'b, _) any -> 'a -> 'b option
  val find_exn : ('a, 'b, _) any -> 'a -> 'b
  val iter_vals : ('a, 'b, _) any -> f:('b -> unit) -> unit
  val merge :
    ('k, 'a, _) any
    -> ('k, 'b, _) any
    -> f:(key:'k -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c option)
    -> ('k, 'c, _) any
  val merge_into:
    f:(key:'a -> 'b -> 'b option -> 'b option)
    -> src:('a, 'b, _) any
    -> dst:('a, 'b) Read_write.t
    -> unit
  val keys : ('a, 'b, _) any -> 'a list
  val data : ('a, 'b, _) any -> 'b list
  val filter_inplace : ('a, 'b) Read_write.t -> f:('b -> bool) -> unit
  val filteri_inplace : ('a, 'b) Read_write.t -> f:('a -> 'b -> bool) -> unit
  val equal : ('a, 'b, _) any -> ('a, 'b, _) any -> ('b -> 'b -> bool) -> bool
  val to_alist : ('a, 'b, _) any -> ('a * 'b) list
  val incr : ?by:int -> ('a, int) Read_write.t -> 'a -> unit
end
