
open Core

type 'a t =
| Nil
| Lazy of (unit -> 'a t)
| Cons of 'a * (unit -> 'a t)
| Protect of (unit -> unit) * 'a t

let empty = Nil
let (==>) x tail = Cons (x, tail)
let (==>>) lst tail =
  match lst with
  | [] -> Lazy tail
  | lst -> List.fold_right lst ~init:tail ~f:(fun x tail ->
    (fun () -> Cons (x, tail))) ()

let initialize tail = Lazy tail
let protect ~finally f = Protect (finally, Lazy f)


let execute_finallys finallys =
  let exns =
    List.filter_map finallys ~f:(fun finally ->
      try finally (); None with exn -> Some exn)
  in
  match List.reduce exns ~f:(fun x y -> Exn.Finally (x,y)) with
  | None -> ()
  | Some exn -> raise exn

(* CREATING A LAZY SEQUENCE --------------------------------------------- *)

let init f =
  let rec loop n =
    match f n with
    | None -> Nil
    | Some x -> x ==> fun () -> loop (n+1)
  in
  Lazy (fun () -> loop 0)

let of_list list =
  let rec of_list list =
    match list with
    | [] -> Nil
    | x :: tail -> x ==> fun () -> of_list tail
  in
  Lazy (fun () -> of_list list)

let of_array arr =
  let arr = Array.copy arr in
  let len = Array.length arr in
  let rec loop idx =
    if idx >= len
    then Nil
    else arr.(idx) ==> fun () -> loop (idx+1)
  in
  Lazy (fun () -> loop 0)

let read_lines filename =
  initialize (fun () ->
    let ic = In_channel.create filename in
    protect ~finally:(fun () -> In_channel.close ic) (fun () ->
      let rec loop () =
        match In_channel.input_line ic with
        | None -> empty
        | Some line -> line ==> loop
      in
      loop ()
    ))

(* LAZY OPERATIONS --------------------------------------------------- *)

let rec map t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> map (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> map tail ~f))
  | Cons (x, tail) -> Cons (f x, (fun () -> map (tail ()) ~f))

let mapi t ~f =
  let rec mapi t i ~f =
    match t with
    | Nil -> Nil
    | Lazy tail -> Lazy (fun () -> mapi (tail ()) i ~f)
    | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> mapi tail i ~f))
    | Cons (x, tail) -> Cons (f i x, (fun () -> mapi (tail ()) (i+1) ~f))
  in
  mapi t 0 ~f

let rec filter_map t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> filter_map (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> filter_map tail ~f))
  | Cons (x, tail) ->
    match f x with
    | None -> Lazy (fun () -> filter_map (tail ()) ~f)
    | Some y -> Cons (y, (fun () -> filter_map (tail ()) ~f))

let filter_mapi t ~f =
  let rec filter_mapi t i ~f =
    match t with
    | Nil -> Nil
    | Lazy tail -> Lazy (fun () -> filter_mapi (tail ()) i ~f)
    | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> filter_mapi tail i ~f))
    | Cons (x, tail) ->
      match f i x with
      | None -> Lazy (fun () -> filter_mapi (tail ()) (i+1) ~f)
      | Some y -> Cons (y, (fun () -> filter_mapi (tail ()) (i+1) ~f))
  in
  filter_mapi t 0 ~f

let filter t ~f =
  filter_map t ~f:(fun x -> if f x then Some x else None)

let rec folding_filter_map t ~init ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> folding_filter_map (tail ()) ~init ~f)
  | Protect (finally, tail) ->
    Protect (finally, Lazy (fun () -> folding_filter_map tail ~init ~f))
  | Cons (x, tail) ->
    let (state,y) = f init x in
    match y with
    | None -> Lazy (fun () -> folding_filter_map (tail ()) ~init:state ~f)
    | Some y -> Cons (y, (fun () -> folding_filter_map (tail ()) ~init:state ~f))

let filter_fold_map = folding_filter_map

let folding_map t ~init ~f =
  folding_filter_map t ~init ~f:(fun state x ->
    let (state,y) = f state x in
    (state, Some y))

let fold_map = folding_map

let rec filter_map_partial t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> filter_map_partial (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> filter_map_partial tail ~f))
  | Cons (x, tail) ->
    match f x with
    | `Stop -> Nil
    | `Continue None -> Lazy (fun () -> filter_map_partial (tail ()) ~f)
    | `Continue (Some y) -> Cons (y, (fun () -> filter_map_partial (tail ()) ~f))

let rec zip_full t1 t2 =
  match t1, t2 with
  | Nil, Nil -> Nil
  (* Warning: The lazy and protect cases need to be paired like this so that if there
     is a sequence of lazies that lead to a protect, they all get executed atomically.
     Otherwise, there could be an interleaving term that raises an exception and
     causes the protect not to be reached, leaking a resource *)
  | Lazy tail, _ -> Lazy (fun () -> zip_full (tail ()) t2)
  | Protect (finally, tail), _ -> Protect (finally, Lazy (fun () -> zip_full tail t2))
  (* Similarly, these two need to be paired *)
  | _, Lazy tail -> Lazy (fun () -> zip_full t1 (tail ()))
  | _, Protect (finally, tail) -> Protect (finally, Lazy (fun () -> zip_full t1 tail))

  | Cons (x,tail1), Cons (y,tail2) ->
    Cons ((Some x, Some y), (fun () -> zip_full (tail1 ()) (tail2 ())))
  | Cons (x,tail1), Nil -> Cons ((Some x, None), (fun () -> zip_full (tail1 ()) Nil))
  | Nil, Cons (y,tail2) -> Cons ((None, Some y), (fun () -> zip_full Nil (tail2 ())))

let rec concat_list_seq t =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> concat_list_seq (tail ()))
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> concat_list_seq tail))
  | Cons (xlist, tail) -> xlist ==>> (fun () -> concat_list_seq (tail ()))

let rec concat_map t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> concat_map (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> concat_map tail ~f))
  | Cons (x, tail) -> f x ==>> (fun () -> concat_map (tail ()) ~f)

(* NONLAZY OPERATIONS ------------------------------------------------ *)

let add elt listref = listref := elt :: !listref
let wrap_finallys finallys f =
  Exn.protect ~f ~finally:(fun () -> execute_finallys !finallys)

let iter t ~f =
  let finallys = ref [] in
  let rec iter t ~f =
    match t with
    | Nil -> ()
    | Lazy tail -> iter (tail ()) ~f
    | Protect (finally, tail) -> add finally finallys; iter tail ~f
    | Cons (x, tail) -> f x; iter (tail ()) ~f
  in
  wrap_finallys finallys (fun () -> iter t ~f)

let iteri t ~f =
  let finallys = ref [] in
  let rec iteri t i ~f =
    match t with
    | Nil -> ()
    | Lazy tail -> iteri (tail ()) i ~f
    | Protect (finally, tail) -> add finally finallys; iteri tail i ~f
    | Cons (x, tail) -> f i x; iteri (tail ()) (i+1) ~f
  in
  wrap_finallys finallys (fun () -> iteri t 0 ~f)


let fold t ~init ~f =
  let finallys = ref [] in
  let rec fold t ~init ~f =
    match t with
    | Nil -> init
    | Lazy tail -> fold (tail ()) ~init ~f
    | Protect (finally, tail) -> add finally finallys; fold tail ~init ~f
    | Cons (x, tail) ->
      let next = f init x in (* Make sure [f] is called before [tail] *)
      fold (tail ()) ~init:next ~f
  in
  wrap_finallys finallys (fun () -> fold t ~init ~f)

let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t

let foldi t ~init ~f =
  let finallys = ref [] in
  let rec foldi t i ~init ~f =
    match t with
    | Nil -> init
    | Lazy tail -> foldi (tail ()) i ~init ~f
    | Protect (finally, tail) -> add finally finallys; foldi tail i ~init ~f
    | Cons (x, tail) ->
      let next = f i init x in (* Make sure [f] is called before [tail] *)
      foldi (tail ()) (i+1) ~init:next ~f
  in
  wrap_finallys finallys (fun () -> foldi t 0 ~init ~f)

let find_map t ~f =
  let finallys = ref [] in
  let rec find_map t ~f =
    match t with
    | Nil -> None
    | Lazy tail -> find_map (tail ()) ~f
    | Protect (finally, tail) -> add finally finallys; find_map tail ~f
    | Cons (x, tail) ->
      match f x with
      | None -> find_map (tail ()) ~f
      | Some y -> Some y
  in
  wrap_finallys finallys (fun () -> find_map t ~f)

let length_if_at_most ~max t =
  with_return (fun {return} ->
    if max < 0 then return None;
    Some (fold t ~init:0 ~f:(fun len _ ->
      if len + 1 > max then return None else len + 1))
  )

let length_bounded_by ?min ?max t =
  match min, max with
  | None, None -> true
  | Some min, None ->
    Option.is_none (length_if_at_most ~max:(min - 1) t)
  | None, Some max ->
    Option.is_some (length_if_at_most ~max t)
  | Some min, Some max ->
    match length_if_at_most ~max t with
    | None -> false
    | Some len -> len >= min

let find t ~f = find_map t ~f:(fun x -> if f x then Some x else None)

let exists t ~f =
  Option.is_some (find_map t ~f:(fun x -> if f x then Some () else None))

let for_all t ~f = not (exists t ~f:(fun x -> not (f x)))

let is_empty t = not (exists t ~f:(fun _ -> true))

let mem t elt ~equal = exists t ~f:(fun x -> equal x elt)

let length t = Container.length ~fold t
let count t ~f = Container.count ~fold t ~f
let sum m t ~f = Container.sum m ~fold t ~f
let min_elt t ~compare = Container.min_elt ~fold t ~compare
let max_elt t ~compare = Container.max_elt ~fold t ~compare
let to_list t = Container.to_list ~fold t
let to_array t = Container.to_array ~fold t

let force t = of_list (to_list t)


(* ITERATOR -------------------------------------------------------------- *)

module Iterator = struct
  type 'a seq = 'a t

  type 'a shared_data = {
    mutable tail: 'a seq;
    mutable finallys: (unit -> unit) list;
    mutable num_iters: int;
  }

  (* Shared linked list that allows iterator copies to be at different points
     in the sequence without loading the sequence more than once.
     Declare special type rather than using ('a * 'a node) option, as a
     performance hack to avoid an allocation. *)
  type 'a node = {
    mutable next: 'a nextnode
  }
  and 'a nextnode =
  | Next of 'a * 'a node
  | Nothing

  type 'a t = {
    mutable node: 'a node;
    shared: 'a shared_data;
    mutable closed: bool;

    (* Terrible hack to avoid allocation: Whenever [get] steps forward in the linked list,
       it stores the node it just stepped from here.
       Whenever peek needs to request a new element from the sequence and add a node to
       the linked list, if there is only one open iterator, rather than allocate a new
       node, it uses this stored node.

       Proof of correctness:

       Invariant: If an iterator is at the end of the list, stored_node for that iterator
       is either a node not in the linked list, or it is a node in the linked list
       strictly behind the current position of the iterator.
         Proof: If this invariant ever holds, it will continue to hold until possibly
           1. A new node is stored.
           2. A node that could potentially be phys_equal to stored_node is appended to
              the list.
           3. The iterator advances to the end of the list.
         (1) only happens in [get], and the stored node is always behind the new
         position of the iterator. (3) only happens in [get], and in all cases where [get]
         advances, it does store a node. (2) only happens in [peek], and in that case,
         after the node is appended, no iterators are at the end of the list.

       The invariant holds at the creation of any iterator - created or copied iterators
       always set stored_node to be a newly allocated node. Therefore, it will always hold.

       [peek] only uses the stored node rather than allocating a new node when there is
       only one open iterator and peek is at the end of the list. Then, by the invariant,
       it must be behind the current iterator or not in the list at all. Therefore, absent
       the stored_node field itself, the node cannot have any references, so it would have
       been garbage-collectable. Therefore, it is safe to use it in place of allocating a
       new node.
    *)
    mutable stored_node: 'a node;
  }

  let close_shared shared =
    shared.tail <- Nil;
    let finallys = shared.finallys in
    shared.finallys <- [];
    execute_finallys finallys

  let next_shared shared =
    let rec next shared tail =
      match tail with
      | Nil -> close_shared shared; None
      | Lazy tail ->
        let tail = try tail () with exn -> close_shared shared; raise exn in
        next shared tail
      | Cons (x, tail) -> shared.tail <- Lazy tail; Some x
      | Protect (finally, tail) ->
        shared.finallys <- finally :: shared.finallys;
        next shared tail
    in
    next shared shared.tail

  let create seq = {
    node = { next = Nothing; };
    shared = {
      tail = seq;
      finallys = [];
      num_iters = 1;
    };
    closed = false;
    stored_node = { next = Nothing; };
  }

  let close t =
    if not t.closed
    then begin
      t.closed <- true;
      t.node <- { next = Nothing; };
      t.shared.num_iters <- t.shared.num_iters - 1;
      if t.shared.num_iters = 0
      then close_shared t.shared
    end

  let copy t =
    t.shared.num_iters <- t.shared.num_iters + 1;
    { node = t.node;
      shared = t.shared;
      closed = t.closed;
      stored_node = { next = Nothing; };
    }

  let get t =
    match t.node.next with
    | Next (x,next) ->
      t.stored_node <- t.node;
      t.node <- next;
      Some x
    | Nothing ->
      if t.closed
      then None
      else begin
        let next = next_shared t.shared in
        (* Performance hack - if only one iterator, we can not bother updating the
           linked list for iterators behind us and simply return the next value *)
        if t.shared.num_iters = 1
        then next
        else
          match next with
          | None -> None
          | Some x ->
            let new_node = { next = Nothing; } in
            t.node.next <- Next (x, new_node);
            t.stored_node <- t.node;
            t.node <- new_node;
            Some x
      end

  let peek t =
    match t.node.next with
    | Next (x,_) -> Some x
    | Nothing ->
      if t.closed
      then None
      else begin
        match next_shared t.shared with
        | None -> None
        | Some x ->
          if t.shared.num_iters = 1
          then begin
            t.stored_node.next <- Nothing;
            t.node.next <- Next (x, t.stored_node);
          end
          else begin
            let new_node = { next = Nothing; } in
            t.node.next <- Next (x, new_node);
          end;
          Some x
      end

  let with_sequence seq ~f =
    Exn.protectx (create seq) ~f
      ~finally:(fun t -> close_shared t.shared)

  let get_exn t = Option.value_exn (get t)

  let has_next t = Option.is_some (peek t)

  let iter t ~f =
    let rec loop () =
      match get t with
      | None -> ()
      | Some x -> f x; loop ()
    in
    Exn.protect ~f:loop ~finally:(fun () -> close t)

  let fold t ~init ~f =
    let rec loop state =
      match get t with
      | None -> state
      | Some x -> loop (f state x)
    in
    Exn.protect ~f:(fun () -> loop init) ~finally:(fun () -> close t)

end

let concat tt =
  initialize (fun () ->
    let tt_iter = Iterator.create tt in
    match Iterator.get tt_iter with
    | None ->
      Iterator.close tt_iter;
      Nil
    | Some first_t ->
      let t_iter = ref (Iterator.create first_t) in
      let rec loop () =
        match Iterator.get !t_iter with
        | None -> begin
          Iterator.close !t_iter;
          match Iterator.get tt_iter with
          | None -> Nil
          | Some next_t ->
            t_iter := Iterator.create next_t;
            loop ()
        end
        | Some x -> Cons (x,loop)
      in
      protect loop ~finally:(fun () ->
        execute_finallys [
          (fun () -> Iterator.close !t_iter);
          (fun () -> Iterator.close tt_iter);
        ]
      )
  )

let concat_seq_list tlist =
  concat (of_list tlist)

let append t1 t2 = concat_seq_list [t1; t2]

let sub t ~pos ~len =
  if len < 0 then failwithf "Lazy_sequence.sub: ~len < 0 (value was %d)" len ();
  if pos < 0 then failwithf "Lazy_sequence.sub: ~pos < 0 (value was %d)" pos ();
  let rec sub t i =
    match t with
    | Nil -> Nil
    | Lazy tail -> Lazy (fun () -> sub (tail ()) i)
    | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> sub tail i))
    | Cons (x, tail) ->
      if i >= pos
      then begin
        if i >= pos + len
        then Nil
        else Cons (x, (fun () -> sub (tail ()) (i+1)))
      end
      else Lazy (fun () -> sub (tail ()) (i+1))
  in
  sub t 0

let nth t n =
  if n < 0 then failwithf "Lazy_sequence.nth: n < 0 (value was %d)" n ();
  match to_list (sub t ~pos:n ~len:1) with
  | [] -> None
  | [x] -> Some x
  | _ :: _ :: _ -> assert false

let hd t = nth t 0

let take t n = sub t ~pos:0 ~len:n

let drop t n =
  if n < 0 then failwithf "Lazy_sequence.drop: n < 0 (value was %d)" n ();
  let rec drop t n =
    match t with
    | Nil -> Nil
    | Lazy tail -> Lazy (fun () -> drop (tail ()) n)
    | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> drop tail n))
    | Cons (_, tail) ->
      if n <= 0
      then t
      else Lazy (fun () -> drop (tail ()) (n-1))
  in
  drop t n

let tl t = drop t 1

let last t =
  Iterator.with_sequence t ~f:(fun iter ->
    let rec loop prev =
      match Iterator.get iter with
      | None -> prev
      | Some _ as cur -> loop cur
    in
    loop None
  )

(* TESTS------------------------------------------------------------------------------ *)

let infinite_ints () = init (fun x -> Some x)
let lazy_assert () = initialize (fun () -> assert false)

let with_ctr n f =
  let ctr = ref 0 in
  f ctr;
  assert (!ctr = n)

let iseven x = x % 2 = 0
let is4or6 x = (x = 4) || (x = 6)
let div2 x = if x % 2 = 0 then Some (x / 2) else None
let running_sum sum x = (sum + x, sum + x)
let prev_even prev x = (x, if prev % 2 = 0 then Some x else None)
let running_sum_geq8 sum x = (sum + x, if sum + x >= 8 then Some (sum + x) else None)
let even_until_8 x =
  if x >= 8 then `Stop else if x % 2 = 0 then `Continue (Some x) else `Continue None

let lst = to_list
let seq = of_list

let%test _ = lst (seq []) = []
let%test _ = lst (seq [2]) = [2]
let%test _ = lst (seq [2;4;6;8]) = [2;4;6;8]
let%test _ = to_array (seq [2;4;6;8]) = [|2;4;6;8|]

let%test _ = lst (map (seq [2;4;6;8]) ~f:(fun x -> x + 1)) = [3;5;7;9]
let%test _ = lst (mapi (seq [2;4;6;8]) ~f:(fun i x -> x + i)) = [2;5;8;11]

let%test _ = lst (filter_map (seq [2;4;6;8]) ~f:(fun _ -> None)) = []
let%test _ = lst (filter_map (seq [2;4;6;8]) ~f:(fun x -> Some (x + 1))) = [3;5;7;9]
let%test _ = lst (filter_map (seq [3;4;6;9;10]) ~f:div2) = [2;3;5]
let%test _ = lst (filter_mapi (seq [2;4;6;8]) ~f:(fun i x -> Some (x + i))) = [2;5;8;11]

let%test _ = lst (filter (seq [2;4;6;8]) ~f:(fun _ -> false)) = []
let%test _ = lst (filter (seq [2;4;6;8]) ~f:(fun _ -> true)) = [2;4;6;8]
let%test _ = lst (filter (seq [2;4;6;8]) ~f:is4or6) = [4;6]

let%test _ = lst (filter_fold_map (seq [3;4;6;9;10]) ~init:0 ~f:prev_even) = [3;6;9]
let%test _ = lst (filter_fold_map (seq [3;4;6;9;10]) ~init:0 ~f:running_sum_geq8) = [13;22;32]
let%test _ = lst (filter_fold_map (seq [3;4;6;9;10]) ~init:1 ~f:running_sum_geq8) = [8;14;23;33]

let%test _ = lst (fold_map (seq [3;4;6;9;10]) ~init:0 ~f:running_sum) = [3;7;13;22;32]
let%test _ = lst (fold_map (seq [3;4;6;9;10]) ~init:1 ~f:running_sum) = [4;8;14;23;33]

let%test _ = lst (filter_map_partial (seq [3;4;6;9;10]) ~f:even_until_8) = [4;6]
let%test _ = lst (filter_map_partial (infinite_ints ()) ~f:even_until_8) = [0;2;4;6]

let%test _ = lst (concat_map (seq [3;4;6;9;10]) ~f:(fun x -> [x;x+1]))
       = [3;4;4;5;6;7;9;10;10;11]
let%test _ = lst (concat_map (seq [[3;4];[];[6;9];[10]]) ~f:Fn.id)
       = [3;4;6;9;10]
let%test _ = lst (concat_list_seq (seq [[3;4];[];[6;9];[10]]))
       = [3;4;6;9;10]
let%test _ = lst (concat_seq_list [seq [3;4]; seq []; seq [6;9]; seq [10]])
       = [3;4;6;9;10]
let%test _ = lst (concat (seq [seq [3;4]; seq []; seq [6;9]; seq [10]]))
       = [3;4;6;9;10]

let%test _ = lst (zip_full (seq [1;2;3]) (seq [2;4;6])) =
  [(Some 1,Some 2);(Some 2,Some 4);(Some 3, Some 6)]
let%test _ = lst (zip_full (seq [1;2;3]) (seq [])) =
  [(Some 1,None);(Some 2,None);(Some 3,None)]
let%test _ = lst (zip_full (seq [1;2;3]) (seq [4;5])) =
  [(Some 1,Some 4);(Some 2,Some 5);(Some 3,None)]

let%test _ = hd (seq []) = None
let%test _ = hd (seq [1;2;3]) = Some 1
let%test _ = hd (seq [6]) = Some 6
let%test _ = hd (infinite_ints ()) = Some 0
let%test _ = last (seq []) = None
let%test _ = last (seq [1;2;3]) = Some 3
let%test _ = last (seq [6]) = Some 6
let%test _ = nth (seq []) 0 = None
let%test _ = nth (seq []) 1 = None
let%test _ = nth (seq [1;2;3]) 0 = Some 1
let%test _ = nth (seq [1;2;3]) 1 = Some 2
let%test _ = nth (seq [1;2;3]) 2 = Some 3
let%test _ = nth (seq [1;2;3]) 3 = None
let%test _ = nth (infinite_ints ()) 10 = Some 10

let%test _ = lst (tl (seq [])) = []
let%test _ = lst (tl (seq [1])) = []
let%test _ = lst (tl (seq [1;2;3])) = [2;3]
let%test _ = lst (take (seq []) 0) = []
let%test _ = lst (take (seq []) 2) = []
let%test _ = lst (take (seq [1;2;3]) 0) = []
let%test _ = lst (take (seq [1;2;3]) 1) = [1]
let%test _ = lst (take (seq [1;2;3]) 2) = [1;2]
let%test _ = lst (take (seq [1;2;3]) 3) = [1;2;3]
let%test _ = lst (take (seq [1;2;3]) 4) = [1;2;3]
let%test _ = lst (take (infinite_ints ()) 6) = [0;1;2;3;4;5]
let%test _ = lst (drop (seq []) 0) = []
let%test _ = lst (drop (seq []) 1) = []
let%test _ = lst (drop (seq [1;2;3]) 0) = [1;2;3]
let%test _ = lst (drop (seq [1;2;3]) 1) = [2;3]
let%test _ = lst (drop (seq [1;2;3]) 2) = [3]
let%test _ = lst (drop (seq [1;2;3]) 3) = []
let%test _ = lst (drop (seq [1;2;3]) 4) = []
let%test _ = lst (append (seq []) (seq [])) = []
let%test _ = lst (append (seq [1]) (seq [])) = [1]
let%test _ = lst (append (seq []) (seq [2])) = [2]
let%test _ = lst (append (seq [1]) (seq [2])) = [1;2]
let%test _ = lst (append (seq [1;2;3]) (seq [])) = [1;2;3]
let%test _ = lst (append (seq [1;2;3]) (seq [4;5;6])) = [1;2;3;4;5;6]
let%test _ = lst (sub (seq []) ~pos:0 ~len:0) = []
let%test _ = lst (sub (seq []) ~pos:0 ~len:1) = []
let%test _ = lst (sub (seq []) ~pos:1 ~len:0) = []
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:0 ~len:0) = []
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:0 ~len:1) = [1]
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:0 ~len:2) = [1;2]
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:1 ~len:2) = [2;3]
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:1 ~len:3) = [2;3;4]
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:2 ~len:3) = [3;4;5]
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:2 ~len:4) = [3;4;5]
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:3 ~len:3) = [4;5]
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:3 ~len:1) = [4]
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:3 ~len:0) = []
let%test _ = lst (sub (seq [1;2;3;4;5]) ~pos:5 ~len:1) = []
let%test _ = lst (sub (infinite_ints ()) ~pos:5 ~len:4) = [5;6;7;8]


let%test_unit _ = with_ctr 6 (fun ctr -> iter (seq [1;2;3]) ~f:(fun x -> ctr := !ctr + x))
let%test_unit _ = with_ctr 9 (fun ctr -> iteri (seq [1;2;3]) ~f:(fun i x -> ctr := !ctr + x + i))
let%test _ = fold ~init:1 (seq [1;2;3]) ~f:(+) = 7
let%test _ = foldi ~init:1 (seq [1;2;3]) ~f:(fun i sum x -> i + sum + x) = 10
let%test _ = find_map (seq [3;4;6;9;10]) ~f:div2 = Some 2
let%test _ = find_map (infinite_ints ()) ~f:div2 = Some 0
let%test _ = find (seq [1;2;5]) ~f:is4or6 = None
let%test _ = find (seq [1;2;6;5]) ~f:is4or6 = Some 6
let%test _ = find (seq [1;2;5;6;4]) ~f:is4or6 = Some 6
let%test _ = find (seq [1;2;5;4;6]) ~f:is4or6 = Some 4
let%test _ = find (infinite_ints ()) ~f:is4or6 = Some 4
let%test _ = exists (seq []) ~f:(fun _ -> assert false) = false
let%test _ = exists (seq [1;3;5;6;9]) ~f:iseven = true
let%test _ = exists (seq [2;4;6;7;8]) ~f:iseven = true
let%test _ = exists (seq [2;4;6;6;8]) ~f:iseven = true
let%test _ = exists (infinite_ints ()) ~f:iseven = true
let%test _ = for_all (seq []) ~f:(fun _ -> assert false) = true
let%test _ = for_all (seq [1;3;5;6;9]) ~f:iseven = false
let%test _ = for_all (seq [2;4;6;7;8]) ~f:iseven = false
let%test _ = for_all (seq [2;4;6;6;8]) ~f:iseven = true
let%test _ = for_all (infinite_ints ()) ~f:iseven = false
let%test _ = is_empty (seq []) = true
let%test _ = is_empty (seq [1]) = false
let%test _ = is_empty (seq [1;2;3]) = false
let%test _ = is_empty (infinite_ints ()) = false
let%test _ = mem (seq [3;4;5]) 6 ~equal:Int.equal = false
let%test _ = mem (seq [3;6;5]) 6 ~equal:Int.equal = true
let%test _ = mem (infinite_ints ()) 10 ~equal:Int.equal = true
let%test _ = length (seq []) = 0
let%test _ = length (seq [3;4;6;9;10]) = 5
let%test _ = count (seq []) ~f:iseven = 0
let%test _ = count (seq [3;4;6;9;10]) ~f:iseven = 3
let%test _ = count (seq [3;4;6;9;10]) ~f:(fun x -> not (iseven x)) = 2
let%test _ = length_bounded_by ~min:(-1) (seq []) = true
let%test _ = length_bounded_by ~min:0 (seq []) = true
let%test _ = length_bounded_by ~min:1 (seq []) = false
let%test _ = length_bounded_by ~max:(-1) (seq []) = false
let%test _ = length_bounded_by ~max:0 (seq []) = true
let%test _ = length_bounded_by ~max:1 (seq []) = true
let%test _ = length_bounded_by ~min:2 ~max:3 (seq [5]) = false
let%test _ = length_bounded_by ~min:2 ~max:3 (seq [5;6]) = true
let%test _ = length_bounded_by ~min:2 ~max:3 (seq [5;6;7]) = true
let%test _ = length_bounded_by ~min:2 ~max:3 (seq [5;6;7;8]) = false
let%test _ = length_bounded_by ~min:2 ~max:10 (infinite_ints ()) = false
let%test _ = length_if_at_most ~max:(-1) (seq []) = None
let%test _ = length_if_at_most ~max:0 (seq []) = Some 0
let%test _ = length_if_at_most ~max:1 (seq []) = Some 0
let%test _ = length_if_at_most ~max:0 (seq [4]) = None
let%test _ = length_if_at_most ~max:1 (seq [4]) = Some 1
let%test _ = length_if_at_most ~max:3 (seq [4;5]) = Some 2
let%test _ = length_if_at_most ~max:3 (seq [4;5;6]) = Some 3
let%test _ = length_if_at_most ~max:3 (seq [4;5;6;7]) = None
let%test _ = length_if_at_most ~max:10 (infinite_ints ()) = None

(* Test laziness *)
let%test_unit _ =
  (* Run a bunch of functions that should be lazy and do nothing
     Note that for sequences that actually begin with some element right away,
     (Cons(x,tail)), these functions will actually operate on the first element. *)
  let run_lazy s =
    ignore (map s ~f:(fun _ -> assert false) : int t);
    ignore (filter_map s ~f:(fun _ -> assert false) : int t);
    ignore (filter s ~f:(fun _ -> assert false) : int t);
    ignore (filter_fold_map s ~init:0 ~f:(fun _ _ -> assert false) : int t);
    ignore (fold_map s ~init:0 ~f:(fun _ _ -> assert false) : int t);
    ignore (filter_map_partial s ~f:(fun _ -> assert false) : int t);
    ignore (zip_full s s : (int option * int option) t);
    ignore (concat_map s ~f:(fun _ -> assert false) : int t);
    ignore (concat_seq_list [s;s] : int t);
    ignore (concat_list_seq (map s ~f:(fun _ -> assert false)) : int t);
    ignore (tl s : int t);
    ignore (take s 3 : int t);
    ignore (drop s 3 : int t);
    ignore (append s s : int t);
    ignore (sub s ~pos:3 ~len:3 : int t);
    ()
  in
  (* All of these should be behind an initialize, so should be perfectly lazy *)
  run_lazy (of_list [1;2;3]);
  run_lazy (infinite_ints ());
  run_lazy (lazy_assert ());
  ()

let mutable_ints_protected start stop incr_on_open incr_on_close =
  initialize (fun () ->
    incr incr_on_open;
    let x = ref start in
    protect ~finally:(fun () -> incr incr_on_close) (fun () ->
      let rec loop () =
        if !x > stop then Nil
        else !x ==> fun () -> incr x; loop ()
      in
      loop ()
    ))

(* Test that mutable state for generating sequences works and doesn't interfere
   with itself, and that protect's finally gets called *)
let%test_unit _ =
  let opens = ref 0 in
  let closes = ref 0 in
  let s = mutable_ints_protected 1 4 opens closes in
  assert ((!opens,!closes) = (0,0));
  ignore (map s ~f:(fun _x -> assert false) : int t);
  assert ((!opens,!closes) = (0,0)); (* Lazy map should do nothing *)
  assert (lst (map s ~f:(fun x -> x + 1)) = [2;3;4;5]);
  assert ((!opens,!closes) = (1,1)); (* Map and forcing to list should open and close *)
  assert (fold s ~init:100 ~f:(+) = 110);
  assert ((!opens,!closes) = (2,2)); (* Fold should open and close *)
  assert (lst (zip_full s s)
          = [(Some 1,Some 1);(Some 2,Some 2);(Some 3,Some 3);(Some 4,Some 4)]);
  assert ((!opens,!closes) = (4,4)); (* Zip with self should open and close twice *)

  (* Creating an iterator should do nothing *)
  let iter = Iterator.create s in
  let iter2 = Iterator.create s in
  assert ((!opens,!closes) = (4,4));

  (* But forcing the first element should open the sequence *)
  assert (Iterator.get iter = Some 1);
  assert ((!opens,!closes) = (5,4));
  assert (Iterator.peek iter = Some 2);
  assert ((!opens,!closes) = (5,4));

  (* Should not interfere with the other iterator *)
  assert (Iterator.get iter2 = Some 1);
  assert ((!opens,!closes) = (6,4));
  assert (Iterator.peek iter2 = Some 2);
  assert ((!opens,!closes) = (6,4));

  assert (Iterator.get iter = Some 2);
  assert (Iterator.get iter2 = Some 2);

  (* And now close them *)
  assert (Iterator.get iter = Some 3);
  assert ((!opens,!closes) = (6,4));
  Iterator.close iter;
  assert ((!opens,!closes) = (6,5));

  assert (Iterator.get iter2 = Some 3);
  assert ((!opens,!closes) = (6,5));
  Iterator.close iter2;
  assert ((!opens,!closes) = (6,6));

  (* Closing twice should do nothing *)
  Iterator.close iter;
  Iterator.close iter2;
  assert ((!opens,!closes) = (6,6));

  (* And we should not be able to get anything now *)
  assert (Iterator.get iter = None);
  assert (Iterator.get iter2 = None);
  assert (Iterator.peek iter = None);
  assert (Iterator.peek iter2 = None);
  assert ((!opens,!closes) = (6,6));

  (* Raising an exception in the middle of a map should also trigger the close *)
  begin
    try ignore (lst (map s ~f:(fun _ -> failwith "fail"))); assert false
    with Failure "fail" -> ()
  end;
  assert ((!opens,!closes) = (7,7));

  (* Same with a fold *)
  begin
    try ignore (fold s ~init:0 ~f:(fun _ _ -> failwith "fail")); assert false
    with Failure "fail" -> ()
  end;
  assert ((!opens,!closes) = (8,8));

  (* Open a new iterator and make sure that falling off the end closes things *)
  let iter3 = Iterator.create s in
  assert (Iterator.get iter3 = Some 1);
  assert ((!opens,!closes) = (9,8));
  assert (Iterator.get iter3 = Some 2);
  assert ((!opens,!closes) = (9,8));
  assert (Iterator.get iter3 = Some 3);
  assert ((!opens,!closes) = (9,8));
  assert (Iterator.get iter3 = Some 4);
  assert ((!opens,!closes) = (9,8));
  assert (Iterator.get iter3 = None);
  assert ((!opens,!closes) = (9,9));
  Iterator.close iter3;
  assert ((!opens,!closes) = (9,9));

  (* Do a simple test that Iterator.copy works *)
  let iter4 = Iterator.create s in
  let iter5 = Iterator.copy iter4 in
  assert ((!opens,!closes) = (9,9));
  assert (Iterator.get iter4 = Some 1);
  assert ((!opens,!closes) = (10,9));
  assert (Iterator.get iter4 = Some 2);
  assert (Iterator.get iter5 = Some 1);
  assert ((!opens,!closes) = (10,9));
  let iter6 = Iterator.copy iter4 in
  assert (Iterator.get iter6 = Some 3);
  assert (Iterator.get iter4 = Some 3);
  Iterator.close iter4;
  assert ((!opens,!closes) = (10,9));
  assert (Iterator.get iter4 = None);
  assert ((!opens,!closes) = (10,9));
  assert (Iterator.get iter6 = Some 4);
  assert (Iterator.get iter6 = None);
  assert ((!opens,!closes) = (10,10));
  assert (Iterator.get iter5 = Some 2);
  assert (Iterator.get iter5 = Some 3);
  assert (Iterator.get iter5 = Some 4);
  assert ((!opens,!closes) = (10,10));
  assert (Iterator.get iter5 = None);
  assert ((!opens,!closes) = (10,10));

  (* Some more functions to test *)
  assert (length_if_at_most ~max:2 s = None);
  assert ((!opens,!closes) = (11,11)); (* Length if at most should open and close *)
  ()
