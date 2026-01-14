open! Core
open! Import

(* A Stack that supports an efficient push_list operation *)
module List_stack : sig
  type 'a t

  val is_empty : _ t -> bool
  val pop_exn : 'a t -> 'a
  val push_list : 'a t -> 'a list -> unit
  val singleton : 'a -> 'a t
end = struct
  type 'a t = 'a list Stack.t

  let _invariant t = Stack.iter t ~f:(fun l -> assert (not (List.is_empty l)))
  let singleton x = Stack.singleton [ x ]
  let is_empty t = Stack.is_empty t

  let push_list t = function
    | [] -> ()
    | _ :: _ as list -> Stack.push t list
  ;;

  let rec pop_exn t =
    match Stack.pop_exn t with
    | [] -> pop_exn t
    | hd :: tl ->
      push_list t tl;
      hd
  ;;
end

type +'a t =
  | Empty
  | Singleton of 'a
  | List of 'a * 'a * 'a list
  | Node of 'a t * 'a t * 'a t list
[@@deriving sexp_of]

let empty = Empty

let fold_left =
  let rec go todo ~init ~f =
    match todo with
    | [] -> init
    | Empty :: todo -> go todo ~init ~f
    | Singleton a :: todo -> go todo ~init:(f init a) ~f
    | List (a, b, cs) :: todo -> go todo ~init:(List.fold ~f ~init:(f (f init a) b) cs) ~f
    | Node (a, b, cs) :: todo -> go (a :: b :: (cs @ todo)) ~init ~f
  in
  fun t ~init ~f -> go [ t ] ~init ~f [@nontail]
;;

let%template[@alloc a @ m = (heap_global, stack_local)] fold_right =
  (* [todo] is the stack of the remaining work, viewed from the right, so [hd todo] is the
     rightmost node that was not processed yet.

     Right-to-left evaluation of the original appendable list is then accomplished by
     traversing [todo] from head to tail. We still need to traverse each element of [todo]
     from right to left. *)
  let rec go todo ~init ~f =
    match[@exclave_if_stack a] todo with
    | [] -> init
    | Empty :: todo -> go todo ~init ~f
    | Singleton x :: todo -> go todo ~init:(f x init) ~f
    | List (a, b, cs) :: todo ->
      let init = (List.fold_right [@mode m m]) ~f cs ~init in
      let init = f b init in
      let init = f a init in
      go todo ~init ~f
    | Node (a, b, cs) :: todo ->
      go ((List.rev_append [@alloc a]) cs (b :: a :: todo)) ~init ~f
  in
  fun t ~init ~f -> go [ t ] ~init ~f [@exclave_if_stack a]
;;

let fold = fold_left
let iter t ~f = Container.iter_via_fold ~fold ~f t

let is_empty = function
  | Empty -> true
  | Singleton _ | List _ -> false
  | Node _ as t ->
    with_return (fun return ->
      iter t ~f:(fun _ -> return.return false);
      true)
;;

let fold_until t ~init ~f ~finish = Container.fold_until ~fold ~init ~f t ~finish
let fold_result t ~init ~f = Container.fold_result ~fold_until ~init ~f t
let iter_until t ~f ~finish = Container.iter_until ~fold_until t ~f ~finish
let length t = fold t ~init:0 ~f:(fun acc _ -> Int.succ acc)

let%template to_list t =
  (fold_right [@alloc a]) t ~init:[] ~f:(fun x acc -> x :: acc [@exclave_if_stack a])
  [@exclave_if_stack a]
[@@alloc a @ m = (heap_global, stack_local)]
;;

(* Mauro Jaskelioff and Exequiel Rivas. 2015. Functional pearl: a smart view on datatypes.
   SIGPLAN Not. 50, 9 (August 2015), 355-361.
   DOI=http://dx.doi.org/10.1145/2858949.2784743
   https://www.fceia.unr.edu.ar/~mauro/pubs/smartviews/smartviews.pdf *)
let rec next = function
  | Empty -> None
  | Singleton a -> Some (a, Empty)
  | List (a, b, cs) ->
    Some
      ( a
      , match cs with
        | [] -> Singleton b
        | hd :: tl -> List (b, hd, tl) )
  | Node (Node (a, b, cs), d, es) ->
    (* amortize the traversal of [a] *)
    next
      (match cs with
       | [] -> Node (a, b, d :: es)
       | [ x ] -> Node (a, b, x :: d :: es)
       | x :: y :: tl -> Node (a, b, Node (x, y, tl) :: d :: es))
  | Node (Empty, b, cs) ->
    next
      (match cs with
       | [] -> b
       | hd :: tl -> Node (b, hd, tl))
  | Node (Singleton a, b, cs) ->
    Some
      ( a
      , match cs with
        | [] -> b
        | hd :: tl -> Node (b, hd, tl) )
  | Node (List (a, b, cs), d, es) ->
    let b =
      match cs with
      | [] -> Singleton b
      | hd :: tl -> List (b, hd, tl)
    in
    Some (a, Node (b, d, es))
;;

let to_sequence t = Sequence.unfold ~init:t ~f:next

let concat = function
  | [] -> Empty
  | [ t ] -> t
  | a :: b :: c -> Node (a, b, c)
;;

let append t1 t2 = Node (t1, t2, [])
let add_front x t = Node (Singleton x, t, [])
let add_back t x = Node (t, Singleton x, [])

let of_list = function
  | [] -> Empty
  | [ x ] -> Singleton x
  | a :: b :: c -> List (a, b, c)
;;

let singleton x = Singleton x

let rec non_closure_apply_and_tail_rec_map nodes acc ~f =
  if List_stack.is_empty nodes
  then acc
  else (
    let acc =
      match List_stack.pop_exn nodes with
      | Empty -> acc
      | Singleton x -> append acc (Singleton (f x))
      | List (a, b, cs) ->
        let a = f a in
        let b = f b in
        let cs = List.map cs ~f in
        append acc (List (a, b, cs))
      | Node (a, b, cs) ->
        List_stack.push_list nodes (a :: b :: cs);
        acc
    in
    non_closure_apply_and_tail_rec_map nodes acc ~f)
;;

let map t ~f = non_closure_apply_and_tail_rec_map (List_stack.singleton t) empty ~f

let rec non_closure_apply_and_tail_rec_bind nodes acc ~f =
  if List_stack.is_empty nodes
  then acc
  else (
    let acc =
      match List_stack.pop_exn nodes with
      | Empty -> acc
      | Singleton x -> append acc (f x)
      | List (a, b, cs) ->
        let a = f a in
        let b = f b in
        let cs = List.map cs ~f in
        Node (acc, a, b :: cs)
      | Node (a, b, cs) ->
        List_stack.push_list nodes (a :: b :: cs);
        acc
    in
    non_closure_apply_and_tail_rec_bind nodes acc ~f)
;;

let bind t ~f = non_closure_apply_and_tail_rec_bind (List_stack.singleton t) empty ~f

let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) (t : a t) =
  to_list t |> [%sexp_of: a list]
;;

let t_of_sexp (type a) (a_of_sexp : Sexp.t -> a) sexp =
  sexp |> [%of_sexp: a list] |> of_list
;;

(* Below: we use a pattern that makes explicit how to complete the interfaces with
   functions that do not have a more efficient implementation in that module already *)

include struct
  module Container_gen = Container.Make (struct
      type nonrec 'a t = 'a t

      let fold_until t ~init ~f ~finish = Container.fold_until ~fold t ~init ~f ~finish
      let fold = `Custom fold
      let iter_until = `Define_using_fold_until
      let iter = `Custom iter
      let length = `Custom length
    end)

  open Container_gen

  let count = count
  let exists = exists
  let find = find
  let find_map = find_map
  let for_all = for_all
  let min_elt = min_elt
  let max_elt = max_elt
  let mem = mem
  let to_array = to_array
  let sum = sum
end

include struct
  module Monad_gen = Monad.Make (struct
      type nonrec 'a t = 'a t

      let return = singleton
      let map = `Custom map
      let bind = bind
    end)

  open Monad_gen
  module Monad_infix = Monad_infix
  module Let_syntax = Let_syntax

  let ignore_m = ignore_m
  let join = join
  let bind = bind
  let ( >>= ) t f = bind t ~f
  let ( >>| ) = ( >>| )
  let return = return
  let all = all
  let all_unit = all_unit
end

module For_testing = struct
  module Element = struct
    (* Enough information to uniquely identify the location of the element with some
       appendable list. Leaks implementation details, hence it being in a [For_testing]
       module. *)
    type t =
      | S
      | L of int
      | N of int * t
    [@@deriving compare ~localize, sexp_of, quickcheck]
  end

  (* A reference implementation of [map] that is perhaps not as efficient but more
     obviously correct. *)
  let rec map_simple t ~f =
    match t with
    | Empty -> Empty
    | Singleton a -> Singleton (f a)
    | List (a, b, cs) -> List (f a, f b, List.map cs ~f)
    | Node (a, b, cs) ->
      Node (map_simple a ~f, map_simple b ~f, List.map cs ~f:(map_simple ~f))
  ;;

  (* Generates appendable lists where each element uniquely identifies its location in the
     data structure.

     Termination is guaranteed, because the expected number of children for the whole
     thing is 0.9, which is less than 1. Apart from that one condition, the actual values
     chosen are fairly arbitrary. *)
  let quickcheck_generator =
    let open Quickcheck.Let_syntax in
    Quickcheck.Generator.weighted_recursive_union
      [ 1., return Empty
      ; 2., return (Singleton Element.S)
      ; ( 1.
        , let%map n = Quickcheck.Generator.small_non_negative_int in
          List (Element.L 0, Element.L 1, List.init n ~f:(fun i -> Element.L (i + 2))) )
      ]
      ~f:(fun quickcheck_generator ->
        [ ( 1.
          , let%map a = quickcheck_generator
            and b = quickcheck_generator
            and cs =
              let%bind n = Int.gen_incl 0 5 in
              Quickcheck.Generator.all (List.init n ~f:(fun _ -> quickcheck_generator))
            in
            Node
              ( map_simple a ~f:(fun e -> Element.N (0, e))
              , map_simple b ~f:(fun e -> Element.N (1, e))
              , List.mapi cs ~f:(fun i c ->
                  map_simple c ~f:(fun e -> Element.N (i + 2, e))) ) )
        ])
  ;;
end

module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type nonrec 'a t = 'a t

    (* Binable.V2 requires a uuid. It makes sense for most uses of Binable, where the
       functions can change behavior across versions. But for us, the functions are the
       identity (modulo change of representation), so no reason to have a uuid. *)
    include
      Binable.Of_binable1.V1
        (List.V1)
        (struct
          type nonrec 'a t = 'a t

          let to_binable = to_list
          let of_binable = of_list
        end) [@@alert "-legacy"]

    include
      Sexpable.Of_sexpable1.V1
        (List.V1)
        (struct
          type nonrec 'a t = 'a t

          let to_sexpable = to_list
          let of_sexpable = of_list
        end)

    let stable_witness (type a) (witness : a Stable_witness.t) : a t Stable_witness.t =
      let module Stable_witness =
        Stable_witness.Of_serializable1
          (List.V1)
          (struct
            type nonrec 'a t = 'a t
          end)
      in
      Stable_witness.of_serializable List.V1.stable_witness of_list to_list witness
    ;;

    let compare compare_a t1 t2 = compare_list compare_a (to_list t1) (to_list t2)

    let%template[@mode local] compare compare_a t1 t2 =
      (compare_list [@mode local])
        compare_a
        ((to_list [@alloc stack]) t1)
        ((to_list [@alloc stack]) t2) [@nontail]
    ;;

    let%expect_test _ =
      assert (Core.String.( = ) [%bin_digest: unit t] [%bin_digest: unit list])
    ;;
  end
end
