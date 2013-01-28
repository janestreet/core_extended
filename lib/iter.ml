open Core.Std

type 'a t = {
  next : (unit -> 'a option);
  progress : (unit -> float option);
}

let next t = t.next ()
let next_exn t = match t.next () with
  | Some e -> e
  | None -> failwith "Iter.next_exn None"

let progress t = t.progress ()
let progress_string = function None -> ""
  | Some x -> sprintf " (%.0f%%)" (100.0 *. x)

let rec i t ~f = match t.next () with
  | Some e -> f e; i t ~f
  | None -> ()

let make ?(progress = (fun () -> None)) f = { next = f; progress = progress; }

let empty = make ~progress:(fun () -> Some 1.0) (fun () -> None)

let concat l =
  let remaining = ref l in
  let current = ref empty in
  let rec loop () =
    match !current.next () with
    | None -> (match !remaining with
      | [] -> None
      | h :: t ->
          current := h;
          remaining := t;
          loop ())
    | some -> some in
  make loop

let reduce t ~init ~f =
  let rec loop acc = match t.next () with
    | Some e -> loop (f acc e)
    | None -> acc in
  loop init

let map t ~f = make ~progress:t.progress (fun () -> Option.map ~f (t.next ()))

let fold = reduce

let unfold ~init ~f ~stop =
  let state = ref init in
  let rec loop () =
    if !state = stop then None
    else (
      let e, i = f !state in
      state := i;
      Some e
    ) in make loop

let rec find t ~f = match t.next () with
  | Some e -> if f e then e else find t ~f
  | None -> raise Not_found

let filter t ~f =
  let rec loop () =
    match t.next () with
    | None -> None
    | Some e -> if f e then Some e else loop ()
  in make loop

let rec for_all t ~f =
  match t.next () with
  | None -> true
  | Some e -> if f e then for_all t ~f else false

let rec exists t ~f =
  match t.next () with
  | None -> false
  | Some e -> if f e then true else exists t ~f

let t = make

let of_opt o =
  let state = ref o in {
    next = (fun () ->
      let ret = !state in
      state := None;
      ret);
    progress = (fun () ->
      match !state with
      | Some _ -> Some 0.0
      | None -> Some 1.0);
  }

let of_list l =
  unfold ~f:(function
    | h::t -> h,t
    | [] -> failwith "Iter.of_list -> unfold [] (can't happen)")
    ~init:l ~stop:[]

let to_list t ~f =
  List.rev (fold t ~init:[] ~f:(fun acc e -> (f e)::acc))

let to_list_opt t ~f =
  List.rev (fold t ~init:[] ~f:(fun acc e -> match f e with None -> acc
  | Some x -> x::acc))

let of_array a =
  let len = Array.length a and pos = ref 0 in {
    next = (fun () -> if !pos = len then None
      else ( let r = a.(!pos) in incr pos; Some r ));
    progress = (fun () -> Some ((float !pos) /. (float len)));
  }

let to_array t ~f = Array.of_list (to_list t ~f)
let to_array_opt t ~f = Array.of_list (to_list_opt t ~f)

let channel_progress ?total c =
  try
    let total =
      match total with
      | Some t -> Float.of_int64 t
      | None -> Float.of_int64 (In_channel.length c)
    in
      fun () ->
        try
          Some ((Float.of_int64 (In_channel.pos c)) /. total)
        with _ -> None
  with _ -> (fun () -> None)    (* if in_channel_length fails *)

let of_channel ?total c ~f = {
  next = (fun () -> try Some (f c) with End_of_file -> None);
  progress = channel_progress ?total c;
}

let channel c ~f = try while true do f c; done with End_of_file -> ()
