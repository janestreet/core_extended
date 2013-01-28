
open Core.Std

type 'a t =
| Nil
| Lazy of (unit -> 'a t)
| Cons of 'a * (unit -> 'a t)
| Protect of (unit -> unit) * 'a t

let stop = Nil
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


(* LAZY OPERATIONS --------------------------------------------------- *)

let rec map t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> map (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> map tail ~f))
  | Cons (x, tail) -> Cons (f x, (fun () -> map (tail ()) ~f))

let rec filter_map t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> filter_map (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> filter_map tail ~f))
  | Cons (x, tail) ->
    match f x with
    | None -> Lazy (fun () -> filter_map (tail ()) ~f)
    | Some y -> Cons (y, (fun () -> filter_map (tail ()) ~f))

let filter t ~f =
  filter_map t ~f:(fun x -> if f x then Some x else None)

let rec filter_fold_map t ~init ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> filter_fold_map (tail ()) ~init ~f)
  | Protect (finally, tail) ->
    Protect (finally, Lazy (fun () -> filter_fold_map tail ~init ~f))
  | Cons (x, tail) ->
    let (state,y) = f init x in
    match y with
    | None -> Lazy (fun () -> filter_fold_map (tail ()) ~init:state ~f)
    | Some y -> Cons (y, (fun () -> filter_fold_map (tail ()) ~init:state ~f))

let fold_map t ~init ~f =
  filter_fold_map t ~init ~f:(fun state x ->
    let (state,y) = f state x in
    (state, Some y))

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

let mem ?(equal = (=)) t elt = exists t ~f:(fun x -> equal x elt)

let length t = fold t ~init:0 ~f:(fun acc _ -> acc + 1)

let count t ~f = fold t ~init:0 ~f:(fun acc x -> if f x then acc + 1 else acc)

let to_list t = List.rev (fold t ~init:[] ~f:(fun acc x -> x :: acc))

let to_array t = Array.of_list (to_list t)


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

let read_lines filename =
  initialize (fun () ->
    let ic = In_channel.create filename in
    protect ~finally:(fun () -> In_channel.close ic) (fun () ->
      let rec loop () =
        match In_channel.input_line ic with
        | None -> stop
        | Some line -> line ==> loop
      in
      loop ()
    ))

module Iterator = struct
  type 'a seq = 'a t
  type 'a t = {
    mutable tail: 'a seq;
    mutable finallys: (unit -> unit) list;
  }

  let create seq =
    { tail = seq; finallys = []; }

  let close t =
    t.tail <- Nil;
    let finallys = t.finallys in
    t.finallys <- [];
    execute_finallys finallys

  let with_sequence seq ~f =
    Exn.protectx (create seq) ~finally:close ~f

  let rec next t =
    match t.tail with
    | Nil -> close t; (None, (fun () -> Nil))
    | Lazy tail ->
      begin
        try t.tail <- tail () with exn -> close t; raise exn
      end;
      next t
    | Cons (x, tail) -> (Some x, tail)
    | Protect (finally, tail) ->
      t.finallys <- finally :: t.finallys;
      t.tail <- tail;
      next t

  let get t =
    let (x,tail) = next t in
    t.tail <- Lazy tail;
    x

  let peek t =
    fst (next t)

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
        Iterator.close !t_iter;
        Iterator.close tt_iter;
      )
  )

let concat_seq_list tlist =
  concat (of_list tlist)

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

TEST = lst (seq []) = []
TEST = lst (seq [2]) = [2]
TEST = lst (seq [2;4;6;8]) = [2;4;6;8]
TEST = to_array (seq [2;4;6;8]) = [|2;4;6;8|]

TEST = lst (map (seq [2;4;6;8]) ~f:(fun x -> x + 1)) = [3;5;7;9]

TEST = lst (filter_map (seq [2;4;6;8]) ~f:(fun _ -> None)) = []
TEST = lst (filter_map (seq [2;4;6;8]) ~f:(fun x -> Some (x + 1))) = [3;5;7;9]
TEST = lst (filter_map (seq [3;4;6;9;10]) ~f:div2) = [2;3;5]

TEST = lst (filter (seq [2;4;6;8]) ~f:(fun _ -> false)) = []
TEST = lst (filter (seq [2;4;6;8]) ~f:(fun _ -> true)) = [2;4;6;8]
TEST = lst (filter (seq [2;4;6;8]) ~f:is4or6) = [4;6]

TEST = lst (filter_fold_map (seq [3;4;6;9;10]) ~init:0 ~f:prev_even) = [3;6;9]
TEST = lst (filter_fold_map (seq [3;4;6;9;10]) ~init:0 ~f:running_sum_geq8) = [13;22;32]
TEST = lst (filter_fold_map (seq [3;4;6;9;10]) ~init:1 ~f:running_sum_geq8) = [8;14;23;33]

TEST = lst (fold_map (seq [3;4;6;9;10]) ~init:0 ~f:running_sum) = [3;7;13;22;32]
TEST = lst (fold_map (seq [3;4;6;9;10]) ~init:1 ~f:running_sum) = [4;8;14;23;33]

TEST = lst (filter_map_partial (seq [3;4;6;9;10]) ~f:even_until_8) = [4;6]
TEST = lst (filter_map_partial (infinite_ints ()) ~f:even_until_8) = [0;2;4;6]

TEST = lst (concat_map (seq [3;4;6;9;10]) ~f:(fun x -> [x;x+1]))
       = [3;4;4;5;6;7;9;10;10;11]
TEST = lst (concat_map (seq [[3;4];[];[6;9];[10]]) ~f:Fn.id)
       = [3;4;6;9;10]
TEST = lst (concat_list_seq (seq [[3;4];[];[6;9];[10]]))
       = [3;4;6;9;10]
TEST = lst (concat_seq_list [seq [3;4]; seq []; seq [6;9]; seq [10]])
       = [3;4;6;9;10]
TEST = lst (concat (seq [seq [3;4]; seq []; seq [6;9]; seq [10]]))
       = [3;4;6;9;10]

TEST = lst (zip_full (seq [1;2;3]) (seq [2;4;6])) =
  [(Some 1,Some 2);(Some 2,Some 4);(Some 3, Some 6)]
TEST = lst (zip_full (seq [1;2;3]) (seq [])) =
  [(Some 1,None);(Some 2,None);(Some 3,None)]
TEST = lst (zip_full (seq [1;2;3]) (seq [4;5])) =
  [(Some 1,Some 4);(Some 2,Some 5);(Some 3,None)]

TEST_UNIT = with_ctr 6 (fun ctr -> iter (seq [1;2;3]) ~f:(fun x -> ctr := !ctr + x))
TEST = fold ~init:1 (seq [1;2;3]) ~f:(+) = 7
TEST = find_map (seq [3;4;6;9;10]) ~f:div2 = Some 2
TEST = find (seq [1;2;5]) ~f:is4or6 = None
TEST = find (seq [1;2;6;5]) ~f:is4or6 = Some 6
TEST = find (seq [1;2;5;6;4]) ~f:is4or6 = Some 6
TEST = find (seq [1;2;5;4;6]) ~f:is4or6 = Some 4
TEST = exists (seq []) ~f:(fun _ -> assert false) = false
TEST = exists (seq [1;3;5;6;9]) ~f:iseven = true
TEST = exists (seq [2;4;6;7;8]) ~f:iseven = true
TEST = exists (seq [2;4;6;6;8]) ~f:iseven = true
TEST = for_all (seq []) ~f:(fun _ -> assert false) = true
TEST = for_all (seq [1;3;5;6;9]) ~f:iseven = false
TEST = for_all (seq [2;4;6;7;8]) ~f:iseven = false
TEST = for_all (seq [2;4;6;6;8]) ~f:iseven = true
TEST = is_empty (seq []) = true
TEST = is_empty (seq [1]) = false
TEST = is_empty (seq [1;2;3]) = false
TEST = mem (seq [3;4;5]) 6 = false
TEST = mem (seq [3;6;5]) 6 = true
TEST = length (seq []) = 0
TEST = length (seq [3;4;6;9;10]) = 5
TEST = count (seq []) ~f:iseven = 0
TEST = count (seq [3;4;6;9;10]) ~f:iseven = 3
TEST = count (seq [3;4;6;9;10]) ~f:(fun x -> not (iseven x)) = 2
TEST = length_bounded_by ~min:(-1) (seq []) = true
TEST = length_bounded_by ~min:0 (seq []) = true
TEST = length_bounded_by ~min:1 (seq []) = false
TEST = length_bounded_by ~max:(-1) (seq []) = false
TEST = length_bounded_by ~max:0 (seq []) = true
TEST = length_bounded_by ~max:1 (seq []) = true
TEST = length_bounded_by ~min:2 ~max:3 (seq [5]) = false
TEST = length_bounded_by ~min:2 ~max:3 (seq [5;6]) = true
TEST = length_bounded_by ~min:2 ~max:3 (seq [5;6;7]) = true
TEST = length_bounded_by ~min:2 ~max:3 (seq [5;6;7;8]) = false
TEST = length_if_at_most ~max:(-1) (seq []) = None
TEST = length_if_at_most ~max:0 (seq []) = Some 0
TEST = length_if_at_most ~max:1 (seq []) = Some 0
TEST = length_if_at_most ~max:0 (seq [4]) = None
TEST = length_if_at_most ~max:1 (seq [4]) = Some 1
TEST = length_if_at_most ~max:3 (seq [4;5]) = Some 2
TEST = length_if_at_most ~max:3 (seq [4;5;6]) = Some 3
TEST = length_if_at_most ~max:3 (seq [4;5;6;7]) = None

(* Test laziness *)
TEST_UNIT =
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
    ignore (concat (map s ~f:(fun _ -> assert false)) : int t);
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
TEST_UNIT =
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
  ()

