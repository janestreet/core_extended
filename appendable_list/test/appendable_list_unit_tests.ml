let%test_module "individual test cases" =
  (module (
   struct
     open Core
     open Appendable_list

     type nonrec 'a t = 'a t [@@deriving sexp]

     let to_sequence = to_sequence

     let test_with_list ~expect expr =
       [%test_result: int list] ~expect (expr |> to_list);
       [%test_result: int list]
         ~expect
         (expr |> to_sequence |> Sequence.to_list)
     ;;

     let empty = empty

     let%test_unit "empty" =
       List.iter
         ~f:(test_with_list ~expect:[])
         [ empty; append empty empty; concat (List.init 10 ~f:(const empty)) ]
     ;;

     let of_list = of_list
     let fold_result = fold_result
     let fold_until = fold_until

     let%test_unit "of_list" =
       let n = 10 in
       assert (n mod 2 = 0);
       let expect = List.init n ~f:Fn.id in
       List.iter
         ~f:(test_with_list ~expect)
         [ of_list expect
         ; concat (List.init n ~f:(fun i -> of_list [ i ]))
         ; List.fold ~init:empty expect ~f:(fun acc i ->
             append acc (of_list [ i ]))
         ; (assert (n mod 2 = 0);
            concat
              (List.init (n / 2) ~f:(fun i -> of_list [ 2 * i; (2 * i) + 1 ])))
         ]
     ;;

     let singleton = singleton
     let return = return

     let%test_unit "singleton" =
       test_with_list ~expect:[ 1 ] (singleton 1);
       test_with_list ~expect:[ 1 ] (return 1)
     ;;

     let append = append
     let concat = concat
     let bind = bind
     let ( >>= ) = ( >>= )
     let concat = concat
     let join = join
     let map = map
     let ( >>| ) = ( >>| )
     let add_front = add_front
     let add_back = add_back

     module Monad_infix = Monad_infix
     module Let_syntax = Let_syntax

     let%test_unit _ =
       let test xss =
         let xs = List.map xss ~f:of_list in
         let ts_concat () = concat xs in
         let ts_add_front () =
           List.fold
             (List.rev (List.concat xss))
             ~init:empty
             ~f:(fun acc x -> add_front x acc)
         in
         let ts_add_back () =
           List.fold (List.concat xss) ~init:empty ~f:(fun acc x ->
             add_back acc x)
         in
         let ts_left () =
           List.fold xs ~init:empty ~f:(fun acc t -> append acc t)
         in
         let ts_right () =
           List.fold_right xs ~init:empty ~f:(fun t acc -> append t acc)
         in
         let ts_join () = join (of_list xs) in
         let ts_bind () = bind (of_list xss) ~f:of_list in
         let expect = List.concat xss in
         let f_map i = float (i * 100) in
         let expect_f_map = List.map expect ~f:f_map in
         List.iteri
           ~f:(fun i l ->
             let l =
               try l () with
               | exn -> raise_s [%sexp "exn", (i : int), (exn : Exn.t)]
             in
             let iter_to_list l =
               let r = ref [] in
               iter l ~f:(fun x -> r := x :: !r);
               List.rev !r
             in
             try
               test_with_list l ~expect;
               [%test_result: int list] (iter_to_list l) ~expect;
               [%test_result: float list]
                 (to_list (map l ~f:f_map))
                 ~expect:expect_f_map
             with
             | exn ->
               raise_s [%sexp "fail", (i : int), (l : int t), (exn : Exn.t)])
           [ ts_concat
           ; ts_add_front
           ; ts_add_back
           ; ts_left
           ; ts_right
           ; ts_bind
           ; ts_join
           ]
       in
       List.iter
         ~f:test
         [ []
         ; [ [] ]
         ; [ []; [] ]
         ; [ [ 0 ] ]
         ; [ [ 0; 1 ]; [ 2; 3 ]; []; [ 1; 6 ]; List.init 10 ~f:Fn.id ]
         ; (let v = 10 in
            List.init v ~f:(fun i -> List.init i ~f:Fn.id))
         ; (let v = 10 in
            List.init v ~f:(fun i -> List.init (v - i) ~f:Fn.id))
         ]
     ;;


     let max_elt = max_elt
     let min_elt = min_elt
     let to_array = to_array
     let to_list = to_list
     let find_map = find_map
     let find = find
     let sum = sum
     let count = count
     let for_all = for_all
     let exists = exists
     let fold = fold
     let iter = iter
     let is_empty = is_empty
     let length = length
     let mem = mem
     let all_unit = all_unit
     let all = all
     let ignore_m = ignore_m

     module For_testing = For_testing
   end
   (* This signature is here to remind us to update the unit tests whenever we change
      [Appendable_list]. *) :
     module type of Appendable_list))
;;

(* These tests consider [to_list] to define the semantics of [Appendable_list], which
   means that a correct implementation matches the behavior of [List] for all operations
   as observed through [to_list].

   Test coverage is good because we generate all the branches of the type randomly. It
   suffices for ['a] values only to track their locations, because that's all a
   polymorphic implementation could know about values of type ['a]. *)

let%test_module "semantics" =
  (module (
   struct
     open Core
     open Appendable_list

     (* [to_list] is effectively tested throughout all the tests. *)
     let to_list = to_list

     type nonrec 'a t = 'a t

     let sexp_of_t = sexp_of_t

     let%test_unit "sexp_of_t" =
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t ->
           [%test_eq: Sexp.t]
             [%sexp (t : For_testing.Element.t t)]
             [%sexp (to_list t : For_testing.Element.t list)])
     ;;

     let t_of_sexp = t_of_sexp

     let%test_unit "t_of_sexp" =
       let check sexp =
         [%test_eq: Sexp.t list Or_error.t]
           (Or_error.map
              (Or_error.try_with (fun () -> [%of_sexp: Sexp.t t] sexp))
              ~f:to_list)
           (Or_error.try_with (fun () -> [%of_sexp: Sexp.t list] sexp))
       in
       Quickcheck.test [%quickcheck.generator: Sexp.t] ~f:check
     ;;

     let empty = empty

     let%test_unit "empty" = [%test_eq: Nothing.t list] (to_list empty) []

     let of_list = of_list

     let%test_unit "of_list" =
       Quickcheck.test
         Quickcheck.Generator.small_non_negative_int
         ~f:(fun len ->
           let list = List.init len ~f:Fn.id in
           [%test_eq: int list] (to_list (of_list list)) list)
     ;;

     let singleton = singleton

     let%test_unit "singleton" =
       [%test_eq: unit list] (to_list (singleton ())) [ () ]
     ;;

     let append = append

     let%test_unit "append" =
       Quickcheck.test
         [%quickcheck.generator: For_testing.t * For_testing.t]
         ~sexp_of:[%sexp_of: For_testing.Element.t t * For_testing.Element.t t]
         ~f:(fun (l, r) ->
           let l = For_testing.map_simple l ~f:(fun e -> `L, e) in
           let r = For_testing.map_simple r ~f:(fun e -> `R, e) in
           [%test_eq: ([ `L | `R ] * For_testing.Element.t) list]
             (to_list (append l r))
             (List.append (to_list l) (to_list r)))
     ;;

     let concat = concat

     let%test_unit "concat" =
       Quickcheck.test
         [%quickcheck.generator: For_testing.t list]
         ~sexp_of:[%sexp_of: For_testing.Element.t t list]
         ~f:(fun ts ->
           [%test_eq: For_testing.Element.t list]
             (to_list (concat ts))
             (List.concat (List.map ts ~f:to_list)))
     ;;

     let add_front = add_front

     let%test_unit "add_front" =
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t ->
           let t = For_testing.map_simple t ~f:(fun e -> `tl e) in
           [%test_eq: [ `hd | `tl of For_testing.Element.t ] list]
             (to_list (add_front `hd t))
             (`hd :: to_list t))
     ;;

     let add_back = add_back

     let%test_unit "add_back" =
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t ->
           let t = For_testing.map_simple t ~f:(fun e -> `init e) in
           [%test_eq: [ `init of For_testing.Element.t | `last ] list]
             (to_list (add_back t `last))
             (to_list t @ [ `last ]))
     ;;

     let to_sequence = to_sequence

     let%expect_test "to_sequence" =
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t ->
           [%test_eq: For_testing.Element.t Sequence.t]
             (to_sequence t)
             (Sequence.of_list (to_list t)))
     ;;

     let return = return

     let%test_unit "return" =
       [%test_eq: unit list] (to_list (return ())) (List.return ())
     ;;

     let map = map

     let%test_unit "map" =
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t ->
           [%test_eq: [ `f of For_testing.Element.t ] list]
             (to_list (map t ~f:(fun e -> `f e)))
             (List.map (to_list t) ~f:(fun e -> `f e)))
     ;;

     let bind = bind

     let%test_unit "bind" =
       Quickcheck.test
         [%quickcheck.generator:
           For_testing.t * (For_testing.Element.t -> For_testing.t)]
         ~sexp_of:
           [%sexp_of:
             For_testing.Element.t t * (For_testing.Element.t -> For_testing.t)]
         ~f:(fun (t, f) ->
           [%test_eq: For_testing.Element.t list]
             (to_list (bind t ~f))
             (List.bind (to_list t) ~f:(fun e -> to_list (f e))))
     ;;

     (* The monad implementation is defined by [return], [map], and [bind] *)
     include struct
       let ignore_m = ignore_m
       and join = join
       and ( >>= ) t f = bind t ~f
       and ( >>| ) = ( >>| )
       and return = return
       and all = all
       and all_unit = all_unit

       module Monad_infix = Monad_infix
       module Let_syntax = Let_syntax
     end

     let iter = iter

     let%test_unit "iter" =
       let module Accum = struct
         type t =
           | Init
           | F of t * For_testing.Element.t
         [@@deriving compare, sexp_of]
       end
       in
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t ->
           let iter =
             let acc = ref Accum.Init in
             iter t ~f:(fun e -> acc := Accum.F (!acc, e));
             !acc
           in
           let list_iter =
             let acc = ref Accum.Init in
             List.iter (to_list t) ~f:(fun e -> acc := Accum.F (!acc, e));
             !acc
           in
           [%test_eq: Accum.t] iter list_iter)
     ;;

     let fold = fold

     let%test_unit "fold" =
       let module Accum = struct
         type t =
           | Init
           | F of t * For_testing.Element.t
         [@@deriving compare, sexp_of]
       end
       in
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t ->
           [%test_eq: Accum.t]
             (fold t ~init:Accum.Init ~f:(fun acc e -> Accum.F (acc, e)))
             (List.fold (to_list t) ~init:Accum.Init ~f:(fun acc e ->
                Accum.F (acc, e))))
     ;;

     let fold_until = fold_until

     let%expect_test "fold_until" =
       let module Accum = struct
         type t =
           [ `Init
           | `Continue of t * For_testing.Element.t
           ]
         [@@deriving compare, sexp_of]

         let quickcheck_observer =
           Quickcheck.Observer.fixed_point (fun quickcheck_observer ->
             [%quickcheck.observer:
               [ `Init | `Continue of t * For_testing.Element.t ]])
         ;;
       end
       in
       let module Final = struct
         type t =
           | Stop of Accum.t * For_testing.Element.t
           | Finish of Accum.t
         [@@deriving compare, sexp_of]
       end
       in
       Quickcheck.test
         [%quickcheck.generator:
           For_testing.t
           * (Accum.t -> For_testing.Element.t -> [ `Continue | `Stop ])]
         ~f:(fun (t, f) ->
           let f accum e =
             match f accum e with
             | `Continue -> Continue_or_stop.Continue (`Continue (accum, e))
             | `Stop -> Stop (Final.Stop (accum, e))
           in
           let finish accum = Final.Finish accum in
           [%test_eq: Final.t]
             (fold_until t ~init:`Init ~f ~finish)
             (List.fold_until (to_list t) ~init:`Init ~f ~finish))
     ;;

     let fold_result = fold_result

     let%test_unit "fold_result" =
       let module Accum = struct
         type t =
           [ `Init
           | `Ok of t * For_testing.Element.t
           ]
         [@@deriving compare, sexp_of]

         let quickcheck_observer =
           Quickcheck.Observer.fixed_point (fun quickcheck_observer ->
             [%quickcheck.observer:
               [ `Init | `Ok of t * For_testing.Element.t ]])
         ;;
       end
       in
       Quickcheck.test
         [%quickcheck.generator:
           For_testing.t
           * (Accum.t -> For_testing.Element.t -> [ `Ok | `Error ])]
         ~f:(fun (t, f) ->
           let f accum e =
             match f accum e with
             | `Ok -> Ok (`Ok (accum, e))
             | `Error -> Error (`Error (accum, e))
           in
           [%test_eq:
             (Accum.t, [ `Error of Accum.t * For_testing.Element.t ]) Result.t]
             (fold_result t ~init:`Init ~f)
             (List.fold_result (to_list t) ~init:`Init ~f))
     ;;

     let is_empty = is_empty

     let%test_unit "is_empty" =
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t ->
           [%test_eq: bool] (is_empty t) (List.is_empty (to_list t)))
     ;;

     let length = length

     let%test_unit "length" =
       Quickcheck.test
         For_testing.quickcheck_generator
         ~sexp_of:[%sexp_of: For_testing.Element.t t]
         ~f:(fun t -> [%test_eq: int] (length t) (List.length (to_list t)))
     ;;

     (* The rest of the container implementation is defined by [fold], [iter], and
        [length]. *)
     let count = count
     and exists = exists
     and find = find
     and find_map = find_map
     and for_all = for_all
     and min_elt = min_elt
     and max_elt = max_elt
     and mem = mem
     and to_array = to_array
     and sum = sum

     let%expect_test "For_testing.quickcheck_generator at least generates \
                      each constructor"
       =
       let generates f =
         Quickcheck.test_can_generate
           For_testing.quickcheck_generator
           ~f:(fun t -> f [%sexp (t : For_testing.Element.t t)])
       in
       generates (function
         | List [] -> true
         | _ -> false);
       generates (function
         | List [ Atom "S" ] -> true
         | _ -> false);
       generates (function
         | List (List (Atom "L" :: _) :: _) -> true
         | _ -> false);
       generates (function
         | List (List (Atom "N" :: _) :: _) -> true
         | _ -> false)
     ;;

     module For_testing = For_testing
   end :
     module type of Appendable_list))
;;
