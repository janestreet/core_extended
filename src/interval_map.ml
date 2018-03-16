open Core_kernel
open Interval_map_intf

type ('k, 'v, 'cmp) t = {
  left_of_leftmost  : 'v; (* default *)
  value_right_of    : ('k, 'v, 'cmp) Map.t;
} [@@deriving fields]

module type S = S
  with type ('k, 'v, 'cmp) interval_map := ('k, 'v, 'cmp) t
module type S_with_boundary = S_with_boundary
  with type ('k, 'v, 'cmp) interval_map := ('k, 'v, 'cmp) t

let comparator t = Map.comparator t.value_right_of
let comparing_with t = (comparator t).Comparator.compare

let compare =
  let rec compare_tail compare_value sign const_val_left right =
    match right with
    | None -> 0
    | Some ((_, val_right), right') ->
      let c = compare_value const_val_left val_right in
      if c <> 0 then c * sign else
        compare_tail compare_value sign const_val_left (Sequence.next right')
  in
  let rec compare compare_key compare_value left_prev left right_prev right =
    match left with
    | None ->
      compare_tail compare_value 1 left_prev right
    | Some ((left_key, left_val), left') ->
      match right with
      | None ->
        compare_tail compare_value (-1) right_prev left
      | Some ((right_key, right_val), right') ->
        let c_key = compare_key left_key right_key in
        if c_key < 0 then
          let c_val = compare_value left_val right_prev in
          if c_val <> 0 then c_val else
            compare compare_key compare_value
              left_val (Sequence.next left') right_prev right
        else if c_key > 0 then
          let c_val = compare_value left_prev right_val in
          if c_val <> 0 then c_val else
            compare compare_key compare_value
              left_prev left right_val (Sequence.next right')
        else (* c_key = 0 *)
          let c_val = compare_value left_val right_val in
          if c_val <> 0 then c_val else
            compare compare_key compare_value
              left_val (Sequence.next left') right_val (Sequence.next right')
  in
  fun compare_value left right ->
    let c_leftmost = compare_value left.left_of_leftmost right.left_of_leftmost in
    if c_leftmost <> 0 then c_leftmost else
      compare (comparing_with left) compare_value
        left.left_of_leftmost (Map.to_sequence left.value_right_of |> Sequence.next)
        right.left_of_leftmost (Map.to_sequence right.value_right_of |> Sequence.next)
;;

let create = Fields.create

let always left_of_leftmost ~comparator = {
  left_of_leftmost; value_right_of = Map.Using_comparator.empty ~comparator; }

let is_always x = Map.is_empty x.value_right_of

let change t ~at:key data = {
  t with value_right_of = Map.set t.value_right_of ~key ~data; }

let find t key =
  match Map.closest_key t.value_right_of `Less_or_equal_to key with
  | Some (_, v) -> v
  | None -> t.left_of_leftmost

let map t ~f = {
  left_of_leftmost = f t.left_of_leftmost;
  value_right_of = Map.map t.value_right_of ~f;
}

let rec map2 lval rval left right init ~f =
  match left with
  | [] -> List.fold right ~init ~f:(fun init (key, rval) ->
    Map.set init ~key ~data:(f lval rval))
  | ((lnext, lval') :: left') ->
    match right with
    | [] -> List.fold left ~init ~f:(fun init (key, lval) ->
      Map.set init ~key ~data:(f lval rval))
    | ((rnext, rval') :: right') ->
      match (Map.comparator init).Comparator.compare lnext rnext with
      | 0 ->
        map2 lval' rval' left' right' ~f
          (Map.set init ~key:rnext ~data:(f lval' rval'))
      | n when n < 0 ->
        map2 lval' rval left' right ~f
          (Map.set init ~key:lnext ~data:(f lval' rval))
      | _ ->
        map2 lval rval' left right' ~f
          (Map.set init ~key:rnext ~data:(f lval rval'))

let map2 x_val y_val x_changes y_changes ~f =
  map2 ~f x_val y_val
    (Map.to_alist x_changes)
    (Map.to_alist y_changes)
    (Map.Using_comparator.empty ~comparator:(Map.comparator x_changes))

let map2 x y ~f =
  if is_always x then
    if is_always y then
      always (f x.left_of_leftmost y.left_of_leftmost)
        ~comparator:(comparator x)
    else
      map y ~f:(fun y -> f x.left_of_leftmost y)
  else if is_always y then
    map x ~f:(fun x -> f x y.left_of_leftmost)
  else {
    left_of_leftmost = f x.left_of_leftmost y.left_of_leftmost;
    value_right_of = map2 ~f
                       x.left_of_leftmost
                       y.left_of_leftmost
                       x.value_right_of
                       y.value_right_of
  }

let join =
  let fold_step ~key ~data:inner acc =
    (* acc is correct for [< key], has any old garbage [>= key] *)
    let acc =
      match Map.split acc key with
      | (before_key, _at_key, _after_key) -> before_key
    in
    (* acc now just has correct content, all [< key] *)
    if is_always inner then
      (* inner value is unchanging, so change to it at [key] and let it remain *)
      Map.set acc ~key ~data:inner.left_of_leftmost
    else
      let acc = Map.set acc ~key ~data:(find inner key) in
      (* acc is now filled in for [<= key],
         finally add values from inner [> key] to acc.
         any beyond the next key in the outer sequence will be replaced.
      *)
      Map.to_sequence inner.value_right_of
        ~keys_greater_or_equal_to:key
      |> Sequence.fold ~init:acc
           ~f:(fun acc (key, data) -> Map.set acc ~key ~data)
  in
  fun x ->
    if is_always x then
      x.left_of_leftmost
    else {
      left_of_leftmost = x.left_of_leftmost.left_of_leftmost;
      value_right_of =
        Map.fold x.value_right_of
          ~init:x.left_of_leftmost.value_right_of ~f:fold_step;
    }

let iterate_changes t interval =
  let low_key, high_key =
    match interval with
    | `Always                      -> None,         None
    | `Until high_key              -> None,         Some high_key
    | `From low_key                -> Some low_key, None
    | `Between (low_key, high_key) -> Some low_key, Some high_key
  in
  let bounded_above =
    match high_key with
    | None -> t.value_right_of
    | Some high_key -> fst3 (Map.split t.value_right_of high_key)
  in
  Map.to_sequence bounded_above ?keys_greater_or_equal_to:low_key

let remove_changes_within t interval =
  if Interval.is_empty interval ~cmp:(comparator t).Comparator.compare
  then t
  else begin
    match interval with
    | `Always ->
      always t.left_of_leftmost ~comparator:(comparator t)
    | (`Until _ | `From _ | `Between _) as interval ->
      { t with
        value_right_of =
          Sequence.fold (iterate_changes t interval) ~init:t.value_right_of
            ~f:(fun map (key, _) -> Map.remove map key); }
  end

let set_within t interval v =
  if Interval.is_empty interval ~cmp:(comparator t).Comparator.compare
  then t
  else begin
    match interval with
    | `Always -> always v ~comparator:(comparator t)
    | `Until high_key ->
      { (change
           (remove_changes_within t interval)
           ~at:high_key (find t high_key))
        with left_of_leftmost = v; }
    | `From low_key ->
      change
        (remove_changes_within t interval)
        ~at:low_key v
    | `Between (low_key, high_key) ->
      change (change (
        remove_changes_within t interval)
        ~at:low_key v)
        ~at:high_key (find t high_key)
  end

let map_within t interval ~f =
  if Interval.is_empty interval ~cmp:(comparator t).Comparator.compare
  then t
  else begin
    match interval with
    | `Always -> map t ~f
    | (`Until _ | `From _ | `Between _) as interval ->
      let base_changes =
        iterate_changes t interval
        |> Sequence.fold ~init:t.value_right_of
             ~f:(fun map (key, data) ->
               Map.set map ~key ~data:(f data))
      in
      match interval with
      | `Until high_key -> {
          left_of_leftmost = f t.left_of_leftmost;
          value_right_of =
            Map.set base_changes
              ~key:high_key ~data:(find t high_key);
        }

      | `From low_key -> {
          left_of_leftmost = t.left_of_leftmost;
          value_right_of =
            Map.set base_changes
              ~key:low_key ~data:(f (find t low_key));
        }
      | `Between (low_key, high_key) -> {
          left_of_leftmost = t.left_of_leftmost;
          value_right_of =
            Map.set (Map.set base_changes
                       ~key:low_key ~data:(f (find t low_key)))
              ~key:high_key ~data:(find t high_key);
        }
  end

module Preimage_impl = struct
  type ('k, 'v, 'cmp) state =
    | Initially of 'v * ('k * 'v) Sequence.t
    | From of 'k * 'v * ('k * 'v) Sequence.t
    | Fin

  let step = function
    | Fin -> Sequence.Step.Done
    | (Initially (_, seq) | From (_, _, seq)) as state ->
      match Sequence.next seq with
      | None -> Sequence.Step.Yield (begin
        match state with
        | Fin ->
          Error.failwithp [%here] "Reached impossible case"
            () [%sexp_of: unit]
        | Initially (x, _) ->
          x, `Always
        | From (date, x, _) ->
          x, `From date
      end, Fin)
      | Some ((until, y), seq) -> Sequence.Step.Yield (begin
        match state with
        | Fin ->
          Error.failwithp [%here] "Reached impossible case"
            () [%sexp_of: unit]
        | Initially (x, _) ->
          x, `Until until
        | From (from, x, _) ->
          x, `Between (from, until)
      end, From (until, y, seq))

  let init { left_of_leftmost; value_right_of; } =
    Initially (left_of_leftmost, Map.to_sequence value_right_of)
end
let construct_preimage t =
  Sequence.unfold_step ~init:(Preimage_impl.init t) ~f:Preimage_impl.step

module Make(T : Type_with_map_module) = struct
  module Key = T.Map.Key
  module Interval = struct
    type t = Key.t Interval.t
    let is_empty = Interval.is_empty ~cmp:T.compare
    let contains = Interval.contains ~cmp:T.compare
  end

  let compare = compare

  let create = create
  let always x = always ~comparator:Key.comparator x
  let unit = always ()

  let find = find
  let change = change
  let map2 = map2
  let remove_changes_within = remove_changes_within
  let set_within = set_within
  let map_within = map_within
  let construct_preimage = construct_preimage

  let custom_join = join
  let custom_map2 = map2

  module T = struct
    type nonrec 'a t = (Key.t, 'a, Key.comparator_witness) t

    let t_of_sexp a_of_sexp sexp =
      match sexp with
      | Sexp.Atom _ | Sexp.List [] ->
        raise (Sexplib.Conv.Of_sexp_error (
          Failure "t_of_sexp: non-empty list needed", sexp))
      | Sexp.List (left_of_leftmost :: value_right_of) -> {
          left_of_leftmost = a_of_sexp left_of_leftmost;
          value_right_of = [%of_sexp: a T.Map.t] (Sexp.List value_right_of);
        }
    ;;

    let sexp_of_t sexp_of_a t =
      let f ~key ~data acc =
        Sexp.List [T.sexp_of_t key; sexp_of_a data] :: acc
      in
      Sexp.List (
        sexp_of_a t.left_of_leftmost :: Map.fold_right t.value_right_of ~f ~init:[])
    ;;

    let return = always

    let apply f x = map2 ~f:(fun f x -> f x) f x
    let bind x ~f = join (map ~f x)

    let map = `Custom map
  end
  include T
  include Applicative.Make(T)
  include Monad.Make(T)

  (* Shadow some of the generated bindings for efficiency because
     we are in an unusal position of defining bind via join and apply
     via map2.
  *)
  let join = custom_join
  let map2 = custom_map2

  (* all values of type unit t = unit, so... *)
  let ignore_m (_ : _ t) = unit
  let all_unit (_ : unit t list) = unit
end

module Make_with_boundary (Key : Key) = struct
  let find' t key =
    find t (Left_boundary.Inc key)

  module Left_boundary = struct
    module T = struct
      type t = Key.t Left_boundary.t [@@deriving sexp, compare]
    end
    include T
    include Comparable.Make(T)
  end

  include Make(Left_boundary)
end

let gen k_gen a_gen ~comparator =
  let module G = Quickcheck.Generator in
  let open G.Monad_infix in
  G.tuple2 a_gen (List.gen k_gen)
  >>= fun (initial_value, changes_keys) ->
  let changes_keys = List.dedup_and_sort changes_keys ~compare:comparator.Comparator.compare in
  List.gen_with_length (List.length changes_keys) a_gen
  >>| fun changes_vals ->
  create ~left_of_leftmost:initial_value
    ~value_right_of:(
      List.zip_exn changes_keys changes_vals
      |> Map.Using_comparator.of_alist_exn ~comparator)

let%test_module "Check construction from standard map." = (
  module struct
    (* This test is actually only checking that certain functor
       applications that we want to allow really do type-check.
    *)

    module Test_existing_w_String = Make(String)
    module Test_existing_w_Int = Make(Int)

    module Make_via_map_key (Key : Key) = struct
      include Make(struct
          include Key
          module Map = Map.Make(Key)
        end)
    end

    module Make_via_comparable (Key : Key) = struct
      include Make(struct
          include Key
          include Comparable.Make(Key)
        end)
    end
  end)

let%test_module "Quickcheck tests." = (
  module struct
    open Quickcheck

    module Int_key = Make(Int)
    module Interval = Interval

    type 'a poly_t = 'a Int_key.t [@@deriving sexp, compare]
    let poly_t_gen a_gen =
      gen ~comparator:Int.comparator Int.gen a_gen

    type t = int poly_t [@@deriving sexp, compare]
    let t_gen = poly_t_gen Int.gen

    type point = int * int [@@deriving sexp]
    let point_gen = Generator.tuple2 Int.gen Int.gen

    let%test_unit "compare: reflexive" =
      Quickcheck.test t_gen ~sexp_of:sexp_of_t
        ~f:(fun t ->
          [%test_result: int] ~expect:0
            ([%compare: t] t t))
    ;;
    let%test_unit "compare: symmetric" =
      Quickcheck.test
        Generator.(tuple2 t_gen t_gen)
        ~sexp_of:[%sexp_of: t * t]
        ~f:(fun (x, y) ->
          [%test_result: int]
            ~expect:(Int.neg ([%compare: t] y x))
            ([%compare: t] x y))
    ;;
    let%test_unit "compare: transitive" =
      let transitive_rule c_a c_b =
        if c_a = 0 then Some c_b
        else if c_b = 0 then Some c_a
        else if c_a = c_b then Some c_a
        else None
      in
      Quickcheck.test
        Generator.(tuple3 t_gen t_gen t_gen)
        ~sexp_of:[%sexp_of: t * t * t]
        ~f:(fun (x, y, z) ->
          let a = [%compare: t] x y in
          let b = [%compare: t] y z in
          Option.iter (transitive_rule a b)
            ~f:([%test_result: int]
                  ~expect:([%compare: t] x z)))
    ;;

    let%test_unit "Induction principle" =
      Quickcheck.test t_gen ~sexp_of:sexp_of_t
        ~f:(fun t ->
          [%test_result: t] ~expect:t (
            let { left_of_leftmost = i; value_right_of = c; } = t in
            Map.fold c ~init:(always i ~comparator:(Map.comparator c))
              ~f:(fun ~key:at ~data i_map -> change i_map ~at data)))
    ;;

    let%test_unit "sexp round-trip" =
      Quickcheck.test t_gen ~sexp_of:sexp_of_t
        ~f:(fun t ->
          [%test_result: t] ~expect:t (t_of_sexp (sexp_of_t t)))
    ;;

    let%test_unit "change interchange" =
      let this_test_with gen =
        Quickcheck.test
          gen
          ~sexp_of:[%sexp_of: t * point * point]
          ~f:(fun (t, (k, v), (k', v')) ->
            [%test_result: t]
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
        let unequal_keys =
          filter (tuple2 Int.gen Int.gen) ~f:(fun (k, k') -> k <> k')
        in
        tuple4 t_gen unequal_keys Int.gen Int.gen
        >>| fun (t, (k, k'), v, v') ->
        t, (k, v), (k', v')
      end;
      this_test_with begin
        let open Generator in
        (* where the two keys are equal *)
        tuple4 t_gen Int.gen Int.gen Int.gen
        >>| fun (t, k, v, v') ->
        t, (k, v), (k, v')
      end
    ;;

    let%test_unit "find rules: base" =
      Quickcheck.test point_gen ~sexp_of:[%sexp_of: point]
        ~f:(fun (k, v) ->
          [%test_result: int] ~expect:v
            (find (Int_key.always v) k))
    ;;
    let%test_unit "find rules: one change" =
      Quickcheck.test
        (Generator.tuple2 point_gen point_gen)
        ~sexp_of:[%sexp_of: point * point]
        ~f:(fun ((l, c), (k, a)) ->
          [%test_result: int]
            ~expect:(if l >= k then a else c)
            (find (change (Int_key.always c) ~at:k a) l))
    ;;
    let%test_unit "find rules: n+2 changes" =
      Quickcheck.test
        (Generator.tuple4 t_gen Int.gen point_gen point_gen)
        ~sexp_of:[%sexp_of: t * int * point * point]
        ~f:(fun (t, l, (k, a), (k', b)) ->
          (* where k <= k' ... *)
          let (k, a, k', b) =
            if k > k' then
              k', b, k, a
            else
              k, a, k', b
          in
          [%test_result: int]
            ~expect:(
              if l >= k' then
                find (change t ~at:k' b) l
              else
                find (change t ~at:k a) l)
            (find (change (change t ~at:k a) ~at:k' b) l))
    ;;

    let%test_unit "functor laws: map id" =
      Quickcheck.test t_gen ~sexp_of:[%sexp_of: t]
        ~f:(fun x ->
          [%test_result: t] ~expect:x
            (map x ~f:Fn.id))
    ;;
    let%test_unit "functor laws: map composition" =
      let f x = `F x in
      let g x = `G x in

      Quickcheck.test t_gen ~sexp_of:[%sexp_of: t]
        ~f:(fun x ->
          [%test_result: [ `G of [ `F of int ]] poly_t]
            ~expect:(map x ~f:(fun x -> g (f x)))
            (map (map x ~f) ~f:g))
    ;;

    let return = Int_key.always

    let%test_unit "pointed functor law" =
      let f x = `F x in

      Quickcheck.test
        String.gen
        ~sexp_of:[%sexp_of: string]
        ~f:(fun x ->
          [%test_result: [ `F of string ] poly_t]
            ~expect:(return (f x))
            (map ~f (return x)))
    ;;

    let%test_unit "find rules: map" =
      let f x = `F x in

      Quickcheck.test
        (Generator.tuple2 t_gen Int.gen)
        ~sexp_of:[%sexp_of: (t * int)]
        ~f:(fun (x, k) ->
          [%test_result: [ `F of int ]]
            ~expect:(f (find x k))
            (find (map x ~f) k))
    ;;

    let%test_unit "monad laws (via join): associativity" =
      Quickcheck.test
        (poly_t_gen (poly_t_gen (poly_t_gen Int.gen)))
        ~sexp_of:[%sexp_of: int poly_t poly_t poly_t]
        ~f:(fun x ->
          [%test_result: int poly_t]
            ~expect:(join (join x))
            (join (map ~f:join x)));
    ;;
    let%test_unit "monad laws (via join): left/right identity" =
      Quickcheck.test t_gen ~sexp_of:[%sexp_of: t]
        ~f:(fun x ->
          [%test_result: t] ~expect:x
            (join (map ~f:Int_key.return x));
          [%test_result: t] ~expect:x
            (join (return x)););
    ;;
    let%test_unit "monad laws (via join): naturality" =
      let f x = `F x in
      Quickcheck.test
        (poly_t_gen (poly_t_gen Int.gen))
        ~sexp_of:[%sexp_of: int poly_t poly_t]
        ~f:(fun x ->
          [%test_result: [ `F of int ] poly_t]
            ~expect:(map ~f (join x))
            (join (map ~f:(map ~f) x)));
    ;;

    let%test_unit "commutative monad law (via join)" =
      let f x y = `F (x, y) in
      Quickcheck.test
        (Generator.tuple2 t_gen t_gen)
        ~sexp_of:[%sexp_of: t * t]
        ~f:(fun (x, y) ->
          [%test_result: [ `F of int * int ] poly_t]
            ~expect:(join (map x ~f:(fun x -> map y ~f:(f x))))
            (join (map y ~f:(fun y -> map x ~f:(fun x -> f x y)))));
    ;;

    let%test_unit "monad custom ops: ignore_m" =
      Quickcheck.test t_gen ~sexp_of:[%sexp_of: t]
        ~f:(fun x ->
          [%test_result: unit poly_t]
            ~expect:(map ~f:(Fn.const ()) x)
            (Int_key.ignore_m x));
    ;;
    let%test_unit "monad custom ops: all_unit" =
      Quickcheck.test
        (List.gen (poly_t_gen (Unit.gen)))
        ~sexp_of:[%sexp_of: unit poly_t list]
        ~f:(fun x ->
          [%test_result: unit poly_t]
            ~expect:(Int_key.ignore_m (Int_key.all x))
            (Int_key.all_unit x));
    ;;

    let%test_unit "find rules: join" =
      Quickcheck.test
        (Generator.tuple2 (poly_t_gen (poly_t_gen Int.gen)) Int.gen)
        ~sexp_of:[%sexp_of: int poly_t poly_t * int]
        ~f:(fun (x, k) ->
          [%test_result: int]
            ~expect:(find (find x k) k)
            (find (join x) k));
    ;;

    let%test_unit "map2 specified by join/map" =
      let f x y = `F (x, y) in
      Quickcheck.test
        (Generator.tuple2 t_gen t_gen)
        ~sexp_of:[%sexp_of: t * t]
        ~f:(fun (x, y) ->
          [%test_result: [ `F of int * int ] poly_t]
            ~expect:(join (map x ~f:(fun x -> map y ~f:(f x))))
            (map2 ~f x y));
      (* Given the properties of map and join this also implies that:
         map2 ~f (always x) y = map ~f:(f x) y
         map2 ~f (map2 ~g x y) z =
           map2 ~f:(fun x f -> f x) x (map2 ~f:(fun y z f -> f (g x y) z) y z)
         map2 ~f x y = map2 ~f:(Fn.flip f) y x
         find (map2 ~f x y) = f (find x) (find y)

         As apply is defined by map2 this also implies the laws for
         a commutative applicative functor.
      *)
    ;;

    let within interval k =
      match interval with
      | `Always -> true
      | `Until high_key -> k < high_key
      | `From low_key -> k >= low_key
      | `Between (low_key, high_key) -> (k >= low_key) && (k < high_key)
    ;;

    let interval_gen : int Interval.t Generator.t =
      Generator.(
        variant4 Unit.gen Int.gen Int.gen (tuple2 Int.gen Int.gen))
      |> Generator.map ~f:(function
        | `A () -> `Always
        | `B k -> `Until k
        | `C k -> `From k
        | `D (k, k') ->
          if k' < k then
            `Between (k', k)
          else
            `Between (k, k'))

    let%test_unit "remove_changes_within: always" =
      Quickcheck.test
        (Generator.tuple2 interval_gen Int.gen)
        ~sexp_of:[%sexp_of: int Interval.t * int]
        ~f:(fun (interval, x) ->
          [%test_result: t]
            ~expect:(Int_key.always x)
            (remove_changes_within (Int_key.always x) interval))
    ;;
    let%test_unit "remove_changes_within: change" =
      Quickcheck.test
        Generator.(tuple3 interval_gen t_gen point_gen)
        ~sexp_of:[%sexp_of: int Interval.t * t * point]
        ~f:(fun (interval, t, (k, x)) ->
          [%test_result: t]
            ~expect:(
              let t' = remove_changes_within t interval in
              if within interval k then t' else change t' ~at:k x)
            (remove_changes_within (change t ~at:k x) interval))
    ;;

    let%test_unit "set_within" =
      Quickcheck.test
        (Generator.tuple4 t_gen interval_gen Int.gen Int.gen)
        ~sexp_of:[%sexp_of: t * int Interval.t * int * int]
        ~f:(fun (t, interval, v, k) ->
          [%test_result: int]
            ~expect:(if within interval k then v else find t k)
            (find (set_within t interval v) k))
    ;;

    let%test_unit "map_within: constant function gives set_within" =
      Quickcheck.test
        Generator.(tuple3 t_gen interval_gen Int.gen)
        ~sexp_of:[%sexp_of: t * int Interval.t * int]
        ~f:(fun (t, interval, v) ->
          [%test_result: t]
            ~expect:(set_within t interval v)
            (map_within t interval ~f:(Fn.const v)))
    ;;
    let%test_module "map_within" = (
      module struct
        type 'a r =
          | Base of 'a
          | F of 'a r
        [@@deriving compare, sexp]

        let base x = Base x
        let f x = F x

        let%test_unit _ =
          Quickcheck.test
            (Generator.tuple3 t_gen interval_gen Int.gen)
            ~sexp_of:[%sexp_of: t * int Interval.t * int]
            ~f:(fun (t, interval, k) ->
              let t = map t ~f:base in
              [%test_result: int r]
                ~expect:(if within interval k then f (find t k) else find t k)
                (find (map_within t interval ~f) k))
        ;;
      end)

    let%test_unit "construct_preimage: reconstruction piecewise is identity" =
      Quickcheck.test t_gen ~sexp_of:[%sexp_of: t]
        ~f:(fun t ->
          construct_preimage t
          |> Sequence.iter ~f:(fun (v, interval) ->
            [%test_result: t] ~expect:t
              (set_within t interval v)))
    ;;
    let%test_unit "construct_preimage: full reconstruction" =
      Quickcheck.test
        (Generator.tuple2 Int.gen t_gen)
        ~sexp_of:[%sexp_of: int * t]
        ~f:(fun (x, t) ->
          [%test_result: t] ~expect:t
            (construct_preimage t
             |> Sequence.fold ~init:(Int_key.always x)
                  ~f:(fun seq (v, interval) ->
                    set_within seq interval v)))
    ;;
    let%test_unit "construct_preimage: multiple reconstruction" =
      (* A more complex reconstruction which also ensures that the set
         of intervals given do not overlap. *)
      let cons x tail = x :: tail in
      Quickcheck.test t_gen ~sexp_of:[%sexp_of: t]
        ~f:(fun t ->
          let seq1 =
            construct_preimage t
            |> Sequence.fold ~init:(Int_key.always [])
                 ~f:(fun seq (v, interval) ->
                   map_within seq interval ~f:(cons v))
          in
          [%test_result: t] ~expect:t (
            map seq1 ~f:(function
              | [v] -> v
              | [] | _ :: _ :: _ ->
                Error.failwithp [%here]
                  "Missing or multiple values at points"
                  seq1 [%sexp_of: int list poly_t])))
    ;;
  end)

let%test_module "Int test" = (
  module struct
    module For_int = Make_with_boundary(Int)

    let create (left_of_leftmost, values) =
      For_int.create
        ~left_of_leftmost
        ~value_right_of:(For_int.Left_boundary.Map.of_alist_exn values)

    let%test_unit "check sexp example" =
      [%test_result: Sexp.t]
        ~expect:(Sexp.of_string "(Pre ((Exc 1) A) ((Inc 2) B))")
        ([%sexp_of: [ `A | `B | `Pre ] For_int.t] (
           create (`Pre, [Exc 1, `A; Inc 2, `B])))
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
      (For_int.find' map key) = expected

    let%test "1/-10" = test t1 (-10) None
    let%test "1/-4" = test t1 (-4) None
    let%test "1/0" = test t1 0 None
    let%test "1/1" = test t1 1 (Some "a")
    let%test "1/2" = test t1 2 None
    let%test "1/3" = test t1 3 None
    let%test "1/4" = test t1 4 (Some "b")
    let%test "1/5" = test t1 5 (Some "c")
    let%test "1/6" = test t1 6 (Some "c")
    let%test "1/9" = test t1 9 (Some "c")
    let%test "1/10" = test t1 10 (Some "c")
    let%test "1/11" = test t1 11 None
    let%test "1/12" = test t1 12 (Some "d")
    let%test "1/13" = test t1 13 (Some "d")
    let%test "1/14" = test t1 14 (Some "e")
    let%test "1/15" = test t1 15 (Some "e")
    let%test "1/19" = test t1 19 (Some "e")
    let%test "1/20" = test t1 20 None
    let%test "1/47" = test t1 47 None

    let l2 = l1 @ [
      Exc (-1), Some "x";
      Inc (-8), Some "y";
      Exc (-4), None;
    ]

    let t2 = create (Some "left", l2)
    let%test "2/-10" = test t2 (-10) (Some "left")
    let%test "2/-4" = test t2 (-4) (Some "y")
    let%test "2/-3" = test t2 (-3) None
    let%test "2/0" = test t2 0 (Some "x")
    let%test "2/1" = test t2 1 (Some "a")
    let%test "2/2" = test t2 2 None
    let%test "2/3" = test t2 3 None
    let%test "2/4" = test t2 4 (Some "b")
    let%test "2/5" = test t2 5 (Some "c")
    let%test "2/6" = test t2 6 (Some "c")
    let%test "2/9" = test t2 9 (Some "c")
    let%test "2/10" = test t2 10 (Some "c")
    let%test "2/11" = test t2 11 None
    let%test "2/12" = test t2 12 (Some "d")
    let%test "2/13" = test t2 13 (Some "d")
    let%test "2/14" = test t2 14 (Some "e")
    let%test "2/15" = test t2 15 (Some "e")
    let%test "2/19" = test t2 19 (Some "e")
    let%test "2/20" = test t2 20 None
    let%test "2/47" = test t2 47 None


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
      (For_int.find' map key) = expected

    let%test "cont -100" = test_cont cont1 (-100) "x"
    let%test "cont -40" = test_cont cont1 (-40) "x"
    let%test "cont -21" = test_cont cont1 (-21) "x"
    let%test "cont -20" = test_cont cont1 (-20) "x"
    let%test "cont -19" = test_cont cont1 (-19) "b"
    let%test "cont -4" = test_cont cont1 (-4) "b"
    let%test "cont -3" = test_cont cont1 (-3) "b"
    let%test "cont -2" = test_cont cont1 (-2) "b"
    let%test "cont -1" = test_cont cont1 (-1) "c"
    let%test "cont 0" = test_cont cont1 (0) "c"
    let%test "cont 1" = test_cont cont1 (1) "c"
    let%test "cont 2" = test_cont cont1 (2) "c"
    let%test "cont 3" = test_cont cont1 (3) "d"
    let%test "cont 4" = test_cont cont1 (4) "d"
    let%test "cont 9" = test_cont cont1 (9) "d"
    let%test "cont 10" = test_cont cont1 (10) "e"
    let%test "cont 11" = test_cont cont1 (11) "e"
    let%test "cont 99" = test_cont cont1 (99) "a"

    let%test "cont always" = test_cont (For_int.always "a") (-100) "a"
  end)
