open Core.Std

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
  type ('a,+'z) any with sexp, bin_io
  module Immutable : sig
    type 'a t = ('a, immutable) any
    include Sexpable.S1 with type 'a t := 'a t
    include Binable.S1 with type 'a t := 'a t
  end
  module Read_only : sig
    type 'a t = ('a, read_only) any
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

  val create : int -> 'a -> ('a,_) any
  val init : int -> f:(int -> 'a) -> ('a,_) any
  val of_array : 'a array -> 'a Read_write.t
  val to_array : 'a Read_write.t -> 'a array
  val of_array_copy : 'a array -> ('a,_) any
  val to_array_copy : ('a,_) any -> 'a array
  val get : ('a,_) any -> int -> 'a
  val set : 'a Read_write.t -> int -> 'a -> unit

  val append: ('a,_) any -> ('a,_) any -> ('a,_) any
  val copy : ('a,_) any -> ('a,_) any
  val map : f:('a -> 'b) -> ('a,_) any -> ('b,_) any
  val mapi : f:(int -> 'a -> 'b) -> ('a,_) any -> ('b,_) any
  val iter : f:('a -> unit) -> ('a,_) any -> unit
  val iteri : f:(int -> 'a -> unit) -> ('a,_) any -> unit
  val filter_opt : ('a option,_) any -> ('a,_) any
  val filter_map : ('a,_) any -> f:('a -> 'b option) -> ('b,_) any
  val filter_mapi : ('a,_) any -> f:(int -> 'a -> 'b option) -> ('b,_) any
  val map2 : ('a,_) any -> ('b,_) any -> f:('a -> 'b -> 'c) -> ('c,_) any
  val findi : ('a,_) any -> f:(int -> 'a -> bool) -> (int * 'a) option
  val blit : src:('a,_) any -> src_pos:int -> dst:'a Read_write.t -> dst_pos:int ->
  len:int -> unit

  val permute : ?random_state:Random.State.t -> _ Read_write.t -> unit
  val fill : 'a Read_write.t -> pos:int -> len:int -> 'a -> unit
  val of_list : 'a list -> ('a,_) any

  include Container.S1_phantom
    with type ('a, 'phantom) t := ('a, 'phantom) any
end
