(** Extensions to [Core.Core_array]. *)
open Core.Std

val foldi : init:'a -> 'b array -> f:(int -> 'a -> 'b -> 'a) -> 'a

(** makes a random split & subset of an array; p (the fraction that you want to split) is
   constrained to be [0, 1].  Note that the length of the first array will be the closest
   integer to the fraction you desired, meaning that each element is NOT selected with
   probability exactly p. *)
val random_split : ?random_state:Random.State.t -> 'a array -> p:float -> 'a array * 'a array
val random_sub   : ?random_state:Random.State.t -> 'a array -> p:float -> 'a array

module Access_control : sig
  type ('a,-'z) any with sexp, bin_io
  module Immutable : sig
    type 'a t = ('a, immutable) any
    include Sexpable.S1 with type 'a t := 'a t
    include Binable.S1 with type 'a t := 'a t
  end
  module Read_only : sig
    type 'a t = ('a, read) any
    include Sexpable.S1 with type 'a t := 'a t
    include Binable.S1 with type 'a t := 'a t
  end
  module Read_write : sig
    type 'a t = ('a, read_write) any
    include Sexpable.S1 with type 'a t := 'a t
    include Binable.S1 with type 'a t := 'a t
  end
  type 'a t = 'a Immutable.t
  include Sexpable.S1 with type 'a t := 'a t
  include Binable.S1 with type 'a t := 'a t

  val create : len:int -> 'a -> ('a, [< _ perms]) any
  val init : int -> f:(int -> 'a) -> ('a, [< _ perms]) any
  val of_array : 'a array -> 'a Read_write.t
  val of_array_copy : 'a array -> ('a, [< _ perms]) any
  val to_array_copy : ('a,[> read]) any -> 'a array
  val get : ('a,[> read]) any -> int -> 'a
  val set : 'a Read_write.t -> int -> 'a -> unit

  val append: ('a, [> read]) any -> ('a, [> read]) any -> ('a, [< _ perms]) any
  val copy : ('a, [> read]) any -> ('a, [< _ perms]) any
  val map : f:('a -> 'b) -> ('a, [> read]) any -> ('b, [< _ perms]) any
  val mapi : f:(int -> 'a -> 'b) -> ('a, [> read]) any -> ('b, [< _ perms]) any
  val iteri : f:(int -> 'a -> unit) -> ('a, [> read]) any -> unit
  val filter_opt : ('a option, [> read]) any -> ('a, [< _ perms]) any
  val filter_map : ('a, [> read]) any -> f:('a -> 'b option) -> ('b, [< _ perms]) any
  val filter_mapi
    :  ('a, [> read]) any
    -> f:(int -> 'a -> 'b option)
    -> ('b, [< _ perms]) any

  val map2_exn
    :  ('a, [> read]) any
    -> ('b, [> read]) any
    -> f:('a -> 'b -> 'c)
    -> ('c, [< _ perms]) any

  val findi : ('a, [> read]) any -> f:(int -> 'a -> bool) -> (int * 'a) option
  val blito : (('a, [> read]) any, 'a Read_write.t) Blit.blito

  val permute : ?random_state:Random.State.t -> _ Read_write.t -> unit
  val fill : 'a Read_write.t -> pos:int -> len:int -> 'a -> unit
  val of_list : 'a list -> ('a, [< _ perms]) any

  include Container.S1_phantom_permissions
    with type ('a, 'phantom) t := ('a, 'phantom) any
end
