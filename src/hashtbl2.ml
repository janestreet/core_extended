open Core

module type Key = sig
  type t [@@deriving compare, sexp_of]
  val hash : t -> int
end

type ('key1, 'key2, 'data) t =
  { create2      : unit -> ('key2, 'data) Hashtbl.t
  ; by_key1      : ('key1, ('key2, 'data) Hashtbl.t) Hashtbl.t
  ; sexp_of_key1 : 'key1 -> Sexp.t
  ; sexp_of_key2 : 'key2 -> Sexp.t
  }

type ('key1, 'key2, 'data) sexp_repr = ('key1 * 'key2 * 'data) list [@@deriving sexp_of]

let to_sexp_repr t =
  List.concat_map (Hashtbl.to_alist t.by_key1)
    ~f:(fun (key1, table2) ->
      List.map (Hashtbl.to_alist table2) ~f:(fun (key2, data) ->
        (key1, key2, data)))
;;

let sexp_of_t sexp_of_key1 sexp_of_key2 sexp_of_data t =
  to_sexp_repr t |> [%sexp_of: (key1, key2, data) sexp_repr]
;;

let clear t =
  Hashtbl.clear t.by_key1
;;

let mem1 t key1 =
  Hashtbl.mem t.by_key1 key1
;;

let mem t key1 key2 =
  match Hashtbl.find_exn t.by_key1 key1 with
  | exception _ -> false
  | by_key2 -> Hashtbl.mem by_key2 key2
;;

let iter t ~f =
  Hashtbl.iteri t.by_key1 ~f:(fun ~key:key1 ~data:by_key2 ->
    Hashtbl.iteri by_key2 ~f:(fun ~key:key2 ~data ->
      f key1 key2 data))
;;

let iter1 t ~f = Hashtbl.iteri t.by_key1 ~f:(fun ~key ~data -> f key data)

let remove_all1 t key1 = Hashtbl.remove t.by_key1 key1

let iter_key2 t key1 ~f =
  match Hashtbl.find t.by_key1 key1 with
  | None -> ()
  | Some by_key2 -> Hashtbl.iteri by_key2 ~f:(fun ~key:key2 ~data -> f key2 data)
;;

let invariant invariant_key1 invariant_key2 invariant_data t =
  Hashtbl.iteri t.by_key1 ~f:(fun ~key:_ ~data:by_key2 ->
    assert (not (Hashtbl.is_empty by_key2)));
  iter t ~f:(fun key1 key2 data ->
    invariant_key1 key1;
    invariant_key2 key2;
    invariant_data data)
;;

let find1 t key1 = Hashtbl.find t.by_key1 key1

let find t key1 key2 =
  match Hashtbl.find t.by_key1 key1 with
  | None -> None
  | Some table2 -> Hashtbl.find table2 key2
;;

let add_exn t key1 key2 data =
  let table2 =
    match Hashtbl.find t.by_key1 key1 with
    | Some table2 -> table2
    | None ->
      let by_key2 = t.create2 () in
      Hashtbl.add_exn t.by_key1 ~key:key1 ~data:by_key2;
      by_key2
  in
  Hashtbl.add_exn table2 ~key:key2 ~data
;;

let set t key1 key2 data =
  let table2 =
    match Hashtbl.find t.by_key1 key1 with
    | Some table2 -> table2
    | None ->
      let by_key2 = t.create2 () in
      Hashtbl.set t.by_key1 ~key:key1 ~data:by_key2;
      by_key2
  in
  Hashtbl.set table2 ~key:key2 ~data
;;

let remove_error t key1 key2 =
  let sexp_of_key1 = t.sexp_of_key1 in
  let sexp_of_key2 = t.sexp_of_key2 in
  failwiths "Hashtbl2.remove_exn of absent keys" (key1, key2) [%sexp_of: key1 * key2]
;;

let remove_exn t key1 key2 =
  match Hashtbl.find t.by_key1 key1 with
  | None -> remove_error t key1 key2
  | Some by_key2 ->
    if Hashtbl.mem by_key2 key2
    then begin
      Hashtbl.remove by_key2 key2;
      if Hashtbl.is_empty by_key2
      then Hashtbl.remove t.by_key1 key1
    end
    else remove_error t key1 key2
;;

module Make (Key1 : Key) (Key2 : Key) = struct

  module Table1 = Hashtbl.Make (struct include Key1 let t_of_sexp _ = assert false end)
  module Table2 = Hashtbl.Make (struct include Key2 let t_of_sexp _ = assert false end)

  type nonrec 'data t = (Key1.t, Key2.t, 'data) t [@@deriving sexp_of]

  let create () =
    { create2      = Table2.create
    ; by_key1      = Table1.create ()
    ; sexp_of_key1 = [%sexp_of: Key1.t]
    ; sexp_of_key2 = [%sexp_of: Key2.t]
    }
  ;;

  let equal equal_data t1 t2 =
    Hashtbl.equal t1.by_key1 t2.by_key1 (fun by_key2_1 by_key2_2 ->
      Hashtbl.equal by_key2_1 by_key2_2 equal_data)
  ;;
end
