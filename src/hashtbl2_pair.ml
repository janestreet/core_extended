open Core

module type Key = Hashtbl2.Key

type ('key1, 'key2, 'data) t =
  { table1 : ('key1, 'key2, 'data) Hashtbl2.t
  ; table2 : ('key2, 'key1, 'data) Hashtbl2.t
  }

let clear t =
  Hashtbl2.clear t.table1;
  Hashtbl2.clear t.table2;
;;

let sexp_of_t sexp_of_key1 sexp_of_key2 sexp_of_data t =
  t.table1 |> [%sexp_of: (key1, key2, data) Hashtbl2.t]
;;

let invariant invariant_key1 invariant_key2 invariant_data t =
  Hashtbl2.invariant invariant_key1 invariant_key2 invariant_data t.table1;
  Hashtbl2.invariant invariant_key2 invariant_key1 invariant_data t.table2;
  let check_contained_in table1 table2 =
    Hashtbl2.iter table1 ~f:(fun key1 key2 data1 ->
      match Hashtbl2.find table2 key2 key1 with
      | None -> assert false
      | Some data2 -> assert (phys_equal data1 data2))
  in
  check_contained_in t.table1 t.table2;
  check_contained_in t.table2 t.table1;
;;

let iter t ~f = Hashtbl2.iter t.table1 ~f

let mem1 t key1 = Hashtbl2.mem1 t.table1 key1
let mem2 t key2 = Hashtbl2.mem1 t.table2 key2

let find1 t key1 = Hashtbl2.find1 t.table1 key1
let find2 t key2 = Hashtbl2.find1 t.table2 key2

let iter1 t ~f = Hashtbl2.iter1 t.table1 ~f
let iter2 t ~f = Hashtbl2.iter1 t.table2 ~f

let mem t key1 key2 =
  match Hashtbl2.find1 t.table1 key1 with
  | None -> false
  | Some table2 -> Hashtbl.mem table2 key2
;;

let find1_iter2 t key1 ~f =
  Hashtbl2.iter_key2 t.table1 key1 ~f
;;

let find2_iter1 t key2 ~f =
  Hashtbl2.iter_key2 t.table2 key2 ~f
;;

let find t key1 key2 = Hashtbl2.find t.table1 key1 key2

let add_exn t key1 key2 data =
  Hashtbl2.add_exn t.table1 key1 key2 data;
  Hashtbl2.add_exn t.table2 key2 key1 data;
;;

let set t key1 key2 data =
  Hashtbl2.set t.table1 key1 key2 data;
  Hashtbl2.set t.table2 key2 key1 data;
;;

let remove_all1 t key1 =
  Hashtbl2.iter_key2 t.table1 key1 ~f:(fun key2 _ ->
    Hashtbl2.remove_exn t.table2 key2 key1);
  Hashtbl2.remove_all1 t.table1 key1;
;;

let remove_exn t key1 key2 =
  Hashtbl2.remove_exn t.table1 key1 key2;
  Hashtbl2.remove_exn t.table2 key2 key1;
;;

module Make (Key1 : Key) (Key2 : Key) = struct

  module Table1 = Hashtbl2.Make (Key1) (Key2)
  module Table2 = Hashtbl2.Make (Key2) (Key1)

  type nonrec 'data t = (Key1.t, Key2.t, 'data) t [@@deriving sexp_of]

  let create () =
    { table1 = Table1.create ()
    ; table2 = Table2.create ()
    }
  ;;

  let of_alist_exn alist =
    let t = create () in
    List.iter alist ~f:(fun (key1, key2, data) -> add_exn t key1 key2 data);
    t
  ;;

  let equal equal_data t1 t2 = Table1.equal equal_data t1.table1 t2.table1

end

let%test_module _ = (module struct
  module M = Make(String)(Int)
  let key1 = "key1"
  let key2 = 2

  let%test_unit _ =
    let t = M.create () in
    add_exn t key1 key2 ();
    assert (mem t key1 key2);
    assert (mem1 t key1);
    assert (mem2 t key2);
    remove_exn t key1 key2;
    assert (not (mem t key1 key2));
    assert (not (mem1 t key1));
    assert (not (mem2 t key2))
  ;;

  let%test_unit _ =
    let t_empty = M.create () in
    let t_empty2 = M.create () in
    add_exn t_empty2 key1 key2 ();
    remove_exn t_empty2 key1 key2;
    assert (M.equal Unit.equal t_empty t_empty2);
    let t_nonempty = M.create () in
    add_exn t_nonempty key1 key2 ();
    assert (not (M.equal Unit.equal t_empty t_nonempty))
  ;;
end)
