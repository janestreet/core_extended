module List = Core.Std.List

module type S = sig
  include Core.Monad.S

  (* Like [List.map] but for functions which return monads *)
  val map_monad : 'a list -> f : ('a -> 'b t) -> 'b list t

  (* Like [map_monad] but ignores the outputs from the function. *)
  val map_monad_ignore : 'a list -> f : ('a -> unit t) -> unit t
end

module Make (M : Core.Monad.Basic) : S with type 'a t := 'a M.t = struct
  include Core.Monad.Make (M)

  let map_monad list ~f = all (List.map ~f list)

  let map_monad_ignore list ~f = all_ignore (List.map ~f list)
end

module type S2 = sig
  include Core.Monad.S2

  val map_monad : 'a list -> f : ('a -> ('b, 'c) t) -> ('b list, 'c) t

  val map_monad_ignore : 'a list -> f : ('a -> (unit, 'b) t) -> (unit, 'b) t
end

module Make2 (M : Core.Monad.Basic2) : S2 with type ('a,'b) t := ('a,'b) M.t = struct
  include Core.Monad.Make2 (M)

  let map_monad list ~f = all (List.map ~f list)

  let map_monad_ignore list ~f = all_ignore (List.map ~f list)
end
