open Core

module List = Core.List

module type S = sig
  include Monad.S

  (* Like [List.map] but for functions which return monads *)
  val map_monad : 'a list -> f : ('a -> 'b t) -> 'b list t

  (* Like [map_monad] but ignores the outputs from the function. *)
  val map_monad_ignore : 'a list -> f : ('a -> unit t) -> unit t
end

module Make (M : Monad.Basic) : S with type 'a t := 'a M.t = struct
  include Monad.Make (M)

  let map_monad list ~f = all (List.map ~f list)

  let map_monad_ignore list ~f = all_unit (List.map ~f list)
end

module type S2 = sig
  include Monad.S2

  val map_monad : 'a list -> f : ('a -> ('b, 'c) t) -> ('b list, 'c) t

  val map_monad_ignore : 'a list -> f : ('a -> (unit, 'b) t) -> (unit, 'b) t
end

module Make2 (M : Monad.Basic2) : S2 with type ('a,'b) t := ('a,'b) M.t = struct
  include Monad.Make2 (M)

  let map_monad list ~f = all (List.map ~f list)

  let map_monad_ignore list ~f = all_unit (List.map ~f list)
end
