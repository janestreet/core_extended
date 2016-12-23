open! Core.Std
open! Int.Replace_polymorphic_compare

type nil = unit
type ('head, 'tail) cons = 'head * 'tail
type 'elements t = 'elements

let empty = ()
let cons x xs = x, xs

let head = fst
let tail = snd

type 'elements hlist = 'elements t

module Suffix_index = struct
  type ('outer, 'inner) t =
    | Whole_list : ('outer, 'outer) t
    | Tail_of : ('outer, ('head, 'inner) cons) t -> ('outer, 'inner) t

  let whole_list = Whole_list
  let tail_of x = Tail_of x
end

module Path = struct
  type ('from, 'res) t =
    | Stop : ('elements, 'elements hlist) t
    | Head : (('element, _) cons, 'element) t
    | Tail : ('tail, 'res) t -> (('head, 'tail) cons, 'res) t

  let rec follow : 'from 'res. ('from, 'res) t -> 'from hlist -> 'res
    = fun (type from res) (t : (from, res) t) (list : from hlist) : res ->
      match t with
      | Stop -> list
      | Head -> head list
      | Tail and_then -> follow and_then (tail list)
  ;;

  let rec within : 'outer 'inner 'res. ('inner, 'res) t -> suffix:('outer, 'inner) Suffix_index.t -> ('outer, 'res) t
    = fun (type outer inner res) (t : (inner, res) t) ~(suffix : (outer, inner) Suffix_index.t) : (outer, res) t ->
      match suffix with
      | Suffix_index.Whole_list -> t
      | Suffix_index.Tail_of suffix -> within (Tail t) ~suffix
end

let drop t suffix = Path.(follow (within ~suffix Stop) t)
let nth t path = Path.follow path t

module Element_index = struct
  type ('a, 'b) t = ('a, 'b) Path.t

  let first_element = Path.Head
  let of_tail x = Path.Tail x

  let within t ~suffix = Path.within t ~suffix
end
