open Core.Std

module type Key = sig
  type t [@@deriving sexp]
  include Comparable with type t := t
end

module type S = sig
  type 'a t [@@deriving sexp]
  type key [@@deriving sexp]
  val empty : 'a t
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val keys : 'a t -> key list
  val data : 'a t -> 'a list
  val to_alist : 'a t -> (key * 'a) list
  val mem : 'a t -> key -> 'a t * bool
  val find : 'a t -> key -> 'a t * 'a option
  val set : 'a t -> key:key -> data:'a -> 'a t
  val delete : 'a t -> key -> 'a t
  val delete_min : 'a t -> (key * 'a * 'a t) option
  val delete_max : 'a t -> (key * 'a * 'a t) option
  val delete_after  : 'a t -> key -> (key * 'a * 'a t) option
  val delete_before : 'a t -> key -> (key * 'a * 'a t) option
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val map_range
    :  'a t
    -> min_key:key
    -> max_key:key
    -> f:((key * 'a) list -> (key * 'a) list)
    -> 'a t
  val split : 'a t -> key -> 'a t * 'a option * 'a t
end

module Make (Key : Key) : (S with type key = Key.t) = struct

  type key = Key.t [@@deriving sexp]

  (* [Kernel] ensures that no Node can be constructed with an incorrect size *)
  module Kernel : sig

    type size (* tree size *)

    type 'a t = private
      | Empty
      | Node of 'a t * key * 'a * 'a t * size
    [@@deriving sexp]

    val length : 'a t -> int
    val node   : 'a t -> key -> 'a -> 'a t -> 'a t
    val empty  : 'a t
  end = struct

    type size = int [@@deriving sexp]

    type 'a t =
      | Empty
      | Node of 'a t * key * 'a * 'a t * size
    [@@deriving sexp]

    let length = function
      | Empty -> 0
      | Node (_left, _key, _value, _right, size) -> size

    let node left key value right =
      Node (left, key, value, right, length left + length right + 1)

    let empty = Empty
  end

  include Kernel

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
    | Fst (ctx, k, v, r) -> plug (node t k v r) ctx
    | Snd (l, k, v, ctx) -> plug (node l k v t) ctx

  let _ = plug

  let fold_right t ~init ~f =
    let rec loop acc = function
      | [] -> acc
      | `Elem (key, data) :: to_visit ->
        loop (f ~key ~data acc) to_visit
      | `Tree Empty :: to_visit ->
        loop acc to_visit
      | `Tree (Node (l, key, data, r, _)) :: to_visit ->
        loop acc (`Tree r :: `Elem (key, data) :: `Tree l :: to_visit)
    in
    loop init [`Tree t]

  (* this is in CPS so that it is tail-recursive *)
  let rec map_cps : 'r 'a 'b. 'a t -> f:('a -> 'b) -> ('b t -> 'r) -> 'r =
    fun t ~f k ->
      match t with
      | Empty -> k empty
      | Node (l, key, data, r, _ ) ->
        map_cps l ~f (fun l ->
          map_cps r ~f (fun r ->
            k (node l key (f data) r)
          ))

  let map t ~f = map_cps t ~f Fn.id

  (* [find_in_ctx t x] finds the subtree of [t] where the binary-search-tree property
     would have the key [x] sit.  It also returns the context of this subtree.
     This subtree is Empty iff [x] is not in [t].  If [x] is in [t], the returned
     subtree will have key [x] at its root. *)
  let find_in_ctx t x =
    let rec loop ctx this =
      match this with
      | Empty -> (ctx, this)
      | Node (l, y, yv, r, _) ->
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
        (a, node b y yv c)
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
        (node a y yv b, c)
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
        zip a (node b y yv (node c z zv d)) ctx
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
        zip (node (node a z zv b) y yv c) d ctx
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
        zip (node a y yv b) (node c z zv d) ctx

  let splay' t x =
    let result v (l, r) = (l, v, r) in
    match find_in_ctx t x with
    | (ctx, Node (l, k, xv, r, _)) -> result (Some (k, xv)) (zip l r ctx)
    | (ctx, Empty) -> result None (zip empty empty ctx)

  let splay t x =
    let (l, kv, r) = splay' t x in
    (l, Option.map ~f:snd kv, r)

  let is_empty = function
    | Empty -> true
    | Node _ -> false

  let set t k v = match splay t k with (l, _, r) -> node l k v r

  let find_rightmost t =
    let rec loop ctx t =
      match t with
      | Empty -> ctx
      | Node (l, y, yv, r, _) -> loop (Snd (l, y, yv, ctx)) r
    in
    loop Top t

  let find_leftmost t =
    let rec loop t ctx =
      match t with
      | Empty -> ctx
      | Node (l, y, yv, r, _) -> loop l (Fst (ctx, y, yv, r))
    in
    loop t Top

  let delete_min t =
    match find_leftmost t with
    | Top -> None
    | Snd _ ->
      (* find_leftmost only accumulates Top and Fst constructors *)
      assert false
    | Fst (ctx, x, xv, r) ->
      match zip empty r ctx with
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
      match zip l empty ctx with
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
    | Some (x, xv, r) -> node l x xv r

  let data     t = fold_right t ~init:[] ~f:(fun ~key:_ ~data   acc ->       data  :: acc)
  let keys     t = fold_right t ~init:[] ~f:(fun ~key   ~data:_ acc ->  key        :: acc)
  let to_alist t = fold_right t ~init:[] ~f:(fun ~key   ~data   acc -> (key, data) :: acc)

  let delete t x = match splay t x with (l, _, r) -> concat l r

  let mem t x =
    match splay t x with
    | (l, None, r) -> (concat l r, false)
    | (l, Some xv, r) -> (node l x xv r, true)

  let find t x =
    match splay t x with
    | (l, None, r) -> (concat l r, None)
    | (l, Some xv, r) -> (node l x xv r, Some xv)

  let splay_just_before t k =
    let (before, at, after) = splay t k in
    Option.map (delete_max before) ~f:(fun (k, v, before) ->
      let after = Option.fold at ~init:after ~f:(fun t v -> set t k v) in
      (before, k, v, after))

  let splay_just_after t k =
    let (before, at, after) = splay t k in
    Option.map (delete_min after) ~f:(fun (k, v, after) ->
      let before = Option.fold at ~init:before ~f:(fun t v -> set t k v) in
      (before, k, v, after))

  let delete_before t k =
    Option.map (splay_just_before t k)
      ~f:(fun (before, k, v, after) -> (k, v, concat before after))

  let delete_after t k =
    Option.map (splay_just_after t k)
      ~f:(fun (before, k, v, after) -> (k, v, concat before after))

  let map_range t ~min_key ~max_key ~f =
    let (old_range, t) =
      let (before, t) =
        match splay t min_key with
        | (before, None,         after) -> (before, after)
        | (before, Some min_val, after) -> (before, set after min_key min_val)
      in
      let (t, after) =
        match splay t max_key with
        | (before, None,         after) -> (before,                     after)
        | (before, Some max_val, after) -> (set before max_key max_val, after)
      in
      (to_alist t, concat before after)
    in
    let new_range = f old_range in
    List.fold new_range ~init:t ~f:(fun t (key, data) -> set t key data)

  let set t ~key ~data = set t key data

  let split = splay
end

