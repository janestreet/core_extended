open Core.Std

module type Key = sig
  type t
  include Comparable with type t := t
end

module type T = sig
  type 'a t
  type key
  val empty : 'a t
  val is_empty : 'a t -> bool
  val concat : 'a t -> 'a t -> 'a t
  val sandwich : 'a t -> key -> 'a -> 'a t -> 'a t
  val splay : 'a t -> key -> 'a t * 'a option * 'a t
  val splay' : 'a t -> key -> 'a t * (key * 'a) option * 'a t
  val delete_min : 'a t -> (key * 'a * 'a t) option
  val delete_max : 'a t -> (key * 'a * 'a t) option
end

module type S = sig
  include T
  val mem : 'a t -> key -> 'a t * bool
  val find : 'a t -> key -> 'a t * 'a option
  val set : 'a t -> key -> 'a -> 'a t
  val delete : 'a t -> key -> 'a t
end

module Kernel (Key : Key) : (T with type key = Key.t) = struct

  type key = Key.t

  type 'a t = Empty | Node of 'a t * key * 'a * 'a t (* binary search tree *)

  (* zipper type representing the context of a sub-[t] within a larger [t] *)
  type 'a ctx =
    | Top
    | Fst of 'a ctx * key * 'a * 'a t
    | Snd of 'a t * key * 'a * 'a ctx

  (* [plug t ctx] restores the overall tree from the subtree [t] and its context [ctx].

     NOTE: this definition is used nowhere in the remainder of this file.
     It serves only to indicate what a context /means/. *)
  let rec plug t = function
    | Top -> t
    | Fst (ctx, k, v, r) -> plug (Node (t, k, v, r)) ctx
    | Snd (l, k, v, ctx) -> plug (Node (l, k, v, t)) ctx

  let _ = plug

  (* [find_in_ctx t x] finds the subtree of [t] where the binary-search-tree property
     would have the key [x] sit.  It also returns the context of this subtree.
     This subtree is Empty iff [x] is not in [t].  If [x] is in [t], the returned
     subtree will have key [x] at its root. *)
  let find_in_ctx t x =
    let rec loop ctx this =
      match this with
      | Empty -> (ctx, this)
      | Node (l, y, yv, r) ->
        let cmp = Key.compare x y in
        if cmp < 0 then
          loop (Fst (ctx, y, yv, r)) l
        else if cmp > 0 then
          loop (Snd (l, y, yv, ctx)) r
        else
          (ctx, this)
    in
    loop Top t

  (* [zip l r ctx = (l', r')] is the second pass of a splay operation.
     It pulls a phantom node [x] from its position at [ctx] up to the
     top of the tree by doing double and single rotations.

                          (ctx)       (top)
                           ...
                           [x]   ==>   [x]
                           / \         / \
                          l   r       l'  r'
  *)
  let rec zip l r = function
    | Top ->
        (l, r)
    | Fst (Top, y, yv, c) ->
        let a = l in
        let b = r in
        (*
                y         [x]
               / \        / \
             [x]  c  =>  a   y
             / \            / \
            a   b          b   c
        *)
        (a, Node (b, y, yv, c))
    | Snd (a, y, yv, Top) ->
        let b = l in
        let c = r in
        (*
                y             [x]
               / \            / \
              a  [x]   =>    y   c
                 / \        / \
                b   c      a   b
        *)
        (Node (a, y, yv, b), c)
    | Fst (Fst (ctx, z, zv, d), y, yv, c) ->
        let a = l in
        let b = r in
        (*
                z         [x]
               / \        / \
              y   d      a   y
             / \     =>     / \
           [x]  c          b   z
           / \                / \
          a   b              c   d
        *)
        zip a (Node (b, y, yv, Node (c, z, zv, d))) ctx
    | Snd (b, y, yv, Snd (a, z, zv, ctx)) ->
        let c = l in
        let d = r in
        (*
            z                 [x]
           / \                / \
          a   y              y   d
             / \     =>     / \
            b  [x]         z   c
               / \        / \
              c   d      a   b
        *)
        zip (Node (Node (a, z, zv, b), y, yv, c)) d ctx
    | ( Snd (a, y, yv, Fst (ctx, z, zv, d))
      | Fst (Snd (a, y, yv, ctx), z, zv, d) )
      ->
        let b = l in
        let c = r in
        (*
                z                             y
               / \           [x]             / \
              y   d         /   \           a   z
             / \     =>    y     z    <=       / \
            a  [x]        / \   / \          [x]  d
               / \       a   b c   d         / \
              b   c                         b   c
        *)
        zip (Node (a, y, yv, b)) (Node(c, z, zv, d)) ctx

  let splay' t x =
    let result v (l, r) = (l, v, r) in
    match find_in_ctx t x with
    | (ctx, Node (l, k, xv, r)) -> result (Some (k, xv)) (zip l r ctx)
    | (ctx, Empty) -> result None (zip Empty Empty ctx)

  let splay t x =
    let (l, kv, r) = splay' t x in
    (l, Option.map ~f:snd kv, r)

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | Node _ -> false

  let sandwich l x xv r = Node (l, x, xv, r)

  let find_rightmost t =
    let rec loop ctx t =
      match t with
      | Empty -> ctx
      | Node (l, y, yv, r) -> loop (Snd (l, y, yv, ctx)) r
    in
    loop Top t

  let find_leftmost t =
    let rec loop t ctx =
      match t with
      | Empty -> ctx
      | Node (l, y, yv, r) -> loop l (Fst (ctx, y, yv, r))
    in
    loop t Top

  let delete_min t =
    match find_leftmost t with
    | Top -> None
    | Snd _ ->
      (* find_leftmost only accumulates Top and Fst constructors *)
      assert false
    | Fst (ctx, x, xv, r) ->
      match zip Empty r ctx with
      | (Empty, r) -> Some (x, xv, r)
      | _ ->
        (* when [ctx] contains only Top and Fst constructors, as it
           does here since it was returned by [find_leftmost], then
           [fst (zip Empty t ctx)] will always be [Empty] for all [t]. *)
        assert false

  let delete_max t =
    match find_rightmost t with
    | Top -> None
    | Fst _ ->
      (* find_rightmost only accumulates Top and Snd constructors *)
      assert false
    | Snd (l, x, xv, ctx) ->
      match zip l Empty ctx with
      | (l, Empty) ->
        (* order reversed here to give the same type as [delete_min] *)
        Some (x, xv, l)
      | _ ->
        (* when [ctx] contains only Top and Snd constructors, as it
           does here since it was returned by [find_rightmost], then
           [snd (zip Empty t ctx)] will always be [Empty] for all [t]. *)
        assert false

  let concat l r =
    match delete_min r with
    | None -> l
    | Some (x, xv, r) -> sandwich l x xv r

end

module Make (Key : Key) : (S with type key = Key.t) = struct

  include Kernel (Key)

  let set t x xv = match splay t x with (l, _, r) -> sandwich l x xv r
  let delete t x = match splay t x with (l, _, r) -> concat l r

  let mem t x =
    match splay t x with
    | (l, None, r) -> (concat l r, false)
    | (l, Some xv, r) -> (sandwich l x xv r, true)

  let find t x =
    match splay t x with
    | (l, None, r) -> (concat l r, None)
    | (l, Some xv, r) -> (sandwich l x xv r, Some xv)

end

