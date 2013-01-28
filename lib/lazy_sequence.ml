
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
let protect ~finally t = Protect (finally, t)


let execute_finallys finallys =
  let exns =
    List.filter_map finallys ~f:(fun finally ->
      try finally (); None with exn -> Some exn)
  in
  match List.reduce exns ~f:(fun x y -> Exn.Finally (x,y)) with
  | None -> ()
  | Some exn -> raise exn

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

  let rec get t =
    match t.tail with
    | Nil -> close t; None
    | Lazy tail -> t.tail <- tail (); get t
    | Cons (x, tail) -> t.tail <- Lazy tail; Some x
    | Protect (finally, tail) ->
      t.finallys <- finally :: t.finallys;
      t.tail <- tail;
      get t

  let rec peek t =
    match t.tail with
    | Nil -> close t; None
    | Lazy tail -> t.tail <- tail (); peek t
    | Cons (x, _) -> Some x
    | Protect (finally, tail) ->
      t.finallys <- finally :: t.finallys;
      t.tail <- tail;
      peek t

  let get_exn t = Option.value_exn (get t)

  let has_next t = Option.is_some (peek t)

  let iter t ~f =
    let rec loop () =
      match get t with
      | None -> ()
      | Some x -> f x; loop ()
    in
    loop ();
    close t

  let fold t ~init ~f =
    let rec loop state =
      match get t with
      | None -> state
      | Some x -> loop (f state x)
    in
    let result = loop init in
    close t;
    result
end

(* LAZY OPERATIONS --------------------------------------------------- *)

let rec map t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> map (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> map tail ~f))
  | Cons (x, tail) -> Cons (f x, (fun () -> map (tail ()) ~f))

let rec filter t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> filter (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> filter tail ~f))
  | Cons (x, tail) ->
    match f x with
    | false -> Lazy (fun () -> filter (tail ()) ~f)
    | true -> Cons (x, (fun () -> filter (tail ()) ~f))

let rec filter_map t ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> filter_map (tail ()) ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> filter_map tail ~f))
  | Cons (x, tail) ->
    match f x with
    | None -> Lazy (fun () -> filter_map (tail ()) ~f)
    | Some y -> Cons (y, (fun () -> filter_map (tail ()) ~f))

let rec fold_map t ~init ~f =
  match t with
  | Nil -> Nil
  | Lazy tail -> Lazy (fun () -> fold_map (tail ()) ~init ~f)
  | Protect (finally, tail) -> Protect (finally, Lazy (fun () -> fold_map tail ~init ~f))
  | Cons (x, tail) ->
    let (state,y) = f init x in
    Cons (y, (fun () -> fold_map (tail ()) ~init:state ~f))

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

let find t ~f = find_map t ~f:(fun x -> if f x then Some x else None)

let exists t ~f =
  Option.is_some (find_map t ~f:(fun x -> if f x then Some () else None))

let for_all t ~f = not (exists t ~f:(fun x -> not (f x)))

let is_empty t = exists t ~f:(fun _ -> true)

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

let rec of_list list =
  match list with
  | [] -> Nil
  | x :: rest -> x ==> fun () -> of_list rest
