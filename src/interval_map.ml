open Core_kernel.Std

module type Key_intf = Core_kernel.Core_map_intf.Key

module type Key = Core_kernel.Core_map_intf.Key

type ('k, 'v, 'cmp) t = {
  left_of_leftmost  : 'v; (* default *)
  value_right_of    : ('k, 'v, 'cmp) Map.t;
} with fields

let create = Fields.create

let always left_of_leftmost ~comparator = {
  left_of_leftmost; value_right_of = Map.empty ~comparator; }

let change t ~at:key data = {
  t with value_right_of = Map.add t.value_right_of ~key ~data; }

let value_immediately_left_of t key =
  match Map.prev_key t.value_right_of key with
  | None -> t.left_of_leftmost
  | Some (_, v) -> v

let find t key = value_immediately_left_of t (Left_boundary.Exc key)

module type S = Interval_map_intf.S
  with type ('k, 'v, 'cmp) interval_map := ('k, 'v, 'cmp) t

module type S_with_boundary = Interval_map_intf.S_with_boundary
  with type ('k, 'v, 'cmp) interval_map := ('k, 'v, 'cmp) t

module Make(T : Interval_map_intf.Type_with_map_module) = struct
  module Key = T.Map.Key

  let create = create

  type nonrec 'a t = (Key.t, 'a, Key.comparator_witness) t

  let t_of_sexp a_of_sexp sexp =
    match sexp with
    | Sexp.Atom _ | Sexp.List [] ->
      raise (Sexplib.Conv.Of_sexp_error (
        Failure "t_of_sexp: non-empty list needed", sexp))
    | Sexp.List (left_of_leftmost :: value_right_of) -> {
        left_of_leftmost = a_of_sexp left_of_leftmost;
        value_right_of = <:of_sexp< a T.Map.t >> (Sexp.List value_right_of);
      }
  ;;

  let sexp_of_t sexp_of_a t =
    let f ~key ~data acc =
      Sexp.List [T.sexp_of_t key; sexp_of_a data] :: acc
    in
    Sexp.List (
      sexp_of_a t.left_of_leftmost :: Map.fold_right t.value_right_of ~f ~init:[])
  ;;

  let always = always ~comparator:Key.comparator
end

module Make_with_boundary (Key : Key_intf) = struct
  module Left_boundary = struct
    module T = struct
      type t = Key.t Left_boundary.t
      with sexp, compare
    end
    include T
    include Comparable.Make(T)
  end

  let find = find

  include Make(Left_boundary)
end

TEST_MODULE "Check construction from standard map." = struct
  (* Try making some interval maps to check it can be done. *)

  module Test_existing_w_String = Make(String)
  module Test_existing_w_Int = Make(Int)

  module Make_via_map_key (Key : Key_intf) = struct
    include Make(struct
        include Key
        module Map = Map.Make(Key)
      end)
  end

  module Make_via_comparable (Key : Key_intf) = struct
    include Make(struct
        include Key
        include Comparable.Make(Key)
      end)
  end
end

TEST_MODULE "Janecheck tests." = struct
  module Int_key = Make(Int)
  type t = string Int_key.t
  with sexp

  let compare
        { left_of_leftmost = xi; value_right_of = xc; }
        { left_of_leftmost = yi; value_right_of = yc; } =
    <:compare< string * string Int.Map.t >> (xi, xc) (yi, yc)

  open Janecheck.Std

  let t_gen =
    let open Generator in
    list int ~unique:true
    >>= fun changes_keys ->
    tuple string (list string ~length:(`Exactly (List.length changes_keys)))
    >>| fun (init, changes_vals) ->
    create
      ~left_of_leftmost:init
      ~value_right_of:(Int.Map.of_alist_exn (List.zip_exn changes_keys changes_vals))

  TEST_UNIT "Induction principle" =
    Janecheck.test t_gen ~sexp_of:sexp_of_t
      ~f:(fun t ->
        <:test_result< t >> ~expect:t (
          let { left_of_leftmost = i; value_right_of = c; } = t in
          Map.fold c ~init:(always i ~comparator:(Map.comparator c))
            ~f:(fun ~key:at ~data i_map -> change i_map ~at data)))
  ;;

  TEST_UNIT "sexp round-trip" =
    Janecheck.test t_gen ~sexp_of:sexp_of_t
      ~f:(fun t ->
        <:test_result< t >> ~expect:t (t_of_sexp (sexp_of_t t)));
  ;;

  TEST_UNIT "change interchange" =
    let this_test_with gen =
      Janecheck.test
        gen
        ~sexp_of:<:sexp_of< t * (int * string) * (int * string) >>
        ~f:(fun (t, (k, v), (k', v')) ->
          <:test_result< t >>
            ~expect:(change (change t ~at:k v) ~at:k' v')
            begin
              if Int.equal k k' then
                (change t ~at:k v')
              else
                (change (change t ~at:k' v') ~at:k v)
            end);
    in
    this_test_with begin
      let open Generator in
      (* where the two keys are not equal *)
      int
      >>=/<> fun k k_gen' ->
      tuple t_gen (tuple k_gen' (tuple string string))
      >>| fun (t, (k', (v, v'))) ->
      t, (k, v), (k', v')
    end;
    this_test_with begin
      let open Generator in
      (* where the two keys are equal *)
      tuple t_gen (tuple int (tuple string string))
      >>| fun (t, (k, (v, v'))) ->
      t, (k, v), (k, v')
    end;
  ;;

end

TEST_MODULE "Int test" = struct
  module For_int = Make_with_boundary(Int)

  let create (left_of_leftmost, values) =
    For_int.create
      ~left_of_leftmost
      ~value_right_of:(For_int.Left_boundary.Map.of_alist_exn values)

  TEST_UNIT "check sexp example" =
    <:test_result< Sexp.t >>
      ~expect:(Sexp.of_string "(Pre ((Exc 1) A) ((Inc 2) B))")
      (<:sexp_of< [ `A | `B | `Pre ] For_int.t >> (
         create (`Pre, [Exc 1, `A; Inc 2, `B])));
    ;;

  let l1 = [
    Left_boundary.Exc 13, Some "e";
    Inc 20, None;
    Inc 1, Some "a";
    Exc 1, None;
    Exc 3, Some "b";
    Inc 5, Some "c";
    Exc 10, None;
    Exc 11, None;
    Inc 12, Some "d";
  ]

  let t1 = create (None, l1)

  let test map key expected =
    (For_int.find map key) = expected

  TEST "1/-10" = test t1 (-10) None
  TEST "1/-4" = test t1 (-4) None
  TEST "1/0" = test t1 0 None
  TEST "1/1" = test t1 1 (Some "a")
  TEST "1/2" = test t1 2 None
  TEST "1/3" = test t1 3 None
  TEST "1/4" = test t1 4 (Some "b")
  TEST "1/5" = test t1 5 (Some "c")
  TEST "1/6" = test t1 6 (Some "c")
  TEST "1/9" = test t1 9 (Some "c")
  TEST "1/10" = test t1 10 (Some "c")
  TEST "1/11" = test t1 11 None
  TEST "1/12" = test t1 12 (Some "d")
  TEST "1/13" = test t1 13 (Some "d")
  TEST "1/14" = test t1 14 (Some "e")
  TEST "1/15" = test t1 15 (Some "e")
  TEST "1/19" = test t1 19 (Some "e")
  TEST "1/20" = test t1 20 None
  TEST "1/47" = test t1 47 None

  let l2 = l1 @ [
    Exc (-1), Some "x";
    Inc (-8), Some "y";
    Exc (-4), None;
  ]

  let t2 = create (Some "left", l2)
  TEST "2/-10" = test t2 (-10) (Some "left")
  TEST "2/-4" = test t2 (-4) (Some "y")
  TEST "2/-3" = test t2 (-3) None
  TEST "2/0" = test t2 0 (Some "x")
  TEST "2/1" = test t2 1 (Some "a")
  TEST "2/2" = test t2 2 None
  TEST "2/3" = test t2 3 None
  TEST "2/4" = test t2 4 (Some "b")
  TEST "2/5" = test t2 5 (Some "c")
  TEST "2/6" = test t2 6 (Some "c")
  TEST "2/9" = test t2 9 (Some "c")
  TEST "2/10" = test t2 10 (Some "c")
  TEST "2/11" = test t2 11 None
  TEST "2/12" = test t2 12 (Some "d")
  TEST "2/13" = test t2 13 (Some "d")
  TEST "2/14" = test t2 14 (Some "e")
  TEST "2/15" = test t2 15 (Some "e")
  TEST "2/19" = test t2 19 (Some "e")
  TEST "2/20" = test t2 20 None
  TEST "2/47" = test t2 47 None


  let cont1 = create (
    "x", [
      Left_boundary.
        Exc 40, "a";
      Exc (-2), "c";
      Inc 3, "d";
      Inc 10, "e";
      Exc (-20), "b";
    ])

  let test_cont map key expected =
    (For_int.find map key) = expected

  TEST "cont -100" = test_cont cont1 (-100) "x"
  TEST "cont -40" = test_cont cont1 (-40) "x"
  TEST "cont -21" = test_cont cont1 (-21) "x"
  TEST "cont -20" = test_cont cont1 (-20) "x"
  TEST "cont -19" = test_cont cont1 (-19) "b"
  TEST "cont -4" = test_cont cont1 (-4) "b"
  TEST "cont -3" = test_cont cont1 (-3) "b"
  TEST "cont -2" = test_cont cont1 (-2) "b"
  TEST "cont -1" = test_cont cont1 (-1) "c"
  TEST "cont 0" = test_cont cont1 (0) "c"
  TEST "cont 1" = test_cont cont1 (1) "c"
  TEST "cont 2" = test_cont cont1 (2) "c"
  TEST "cont 3" = test_cont cont1 (3) "d"
  TEST "cont 4" = test_cont cont1 (4) "d"
  TEST "cont 9" = test_cont cont1 (9) "d"
  TEST "cont 10" = test_cont cont1 (10) "e"
  TEST "cont 11" = test_cont cont1 (11) "e"
  TEST "cont 99" = test_cont cont1 (99) "a"

  TEST "cont always" = test_cont (For_int.always "a") (-100) "a"
end

