let%test_module _ = (module (struct
  open Core
  open Appendable_list

  type nonrec 'a t = 'a t [@@deriving sexp]

  let to_sequence = to_sequence

  let test_with_list ~expect expr =
    [%test_result: int list] ~expect (expr |> to_list);
    [%test_result: int list] ~expect (expr |> to_sequence |> Sequence.to_list);
  ;;

  let empty = empty

  let%test_unit "empty" =
    List.iter ~f:(test_with_list ~expect:[])
      [ empty
      ; append empty empty
      ; concat (List.init 10 ~f:(const empty))
      ]
  ;;

  let of_list = of_list
  let fold_result = fold_result
  let fold_until = fold_until

  let%test_unit "of_list" =
    let n = 10 in
    assert (n mod 2 = 0);
    let expect = List.init n ~f:Fn.id in
    List.iter ~f:(test_with_list ~expect)
      [ of_list expect
      ; concat (List.init n ~f:(fun i -> of_list [ i ]))
      ; List.fold ~init:empty expect ~f:(fun acc i -> append acc (of_list [ i ]))
      ; begin
        assert (n mod 2 = 0);
        concat (List.init (n/2) ~f:(fun i -> of_list [ 2*i ; 2*i+1 ]))
      end
      ]
  ;;

  let singleton = singleton
  let return = return

  let%test_unit "singleton" =
    test_with_list ~expect:[1] (singleton 1);
    test_with_list ~expect:[1] (return 1);
  ;;

  let append = append
  let concat = concat

  let bind = bind
  let (>>=) = (>>=)

  let concat = concat
  let join = join

  let map = map
  let (>>|) = (>>|)

  let add_front = add_front
  let add_back  = add_back

  module Monad_infix = Monad_infix
  module Let_syntax = Let_syntax

  let%test_unit _ =
    let test xss =
      let xs = List.map xss ~f:of_list in
      let ts_concat () = concat xs in
      let ts_add_front () =
        List.fold (List.rev (List.concat xss)) ~init:empty ~f:(fun acc x ->
          add_front x acc)
      in
      let ts_add_back () =
        List.fold (List.concat xss) ~init:empty ~f:(fun acc x -> add_back acc x)
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
      List.iteri ~f:(fun i l ->
        let l = try l () with exn -> failwiths "exn" (i, exn) [%sexp_of: int * Exn.t] in
        let iter_to_list l =
          let r = ref [] in
          iter l ~f:(fun x -> r := x :: !r);
          List.rev !r
        in
        try
          test_with_list l ~expect;
          [%test_result: int list] (iter_to_list l) ~expect;
          [%test_result: float list] (to_list (map l ~f:f_map)) ~expect:expect_f_map;
        with exn ->
          failwiths "fail" (i, l, exn) [%sexp_of: int * int t * Exn.t]
      )
        [ ts_concat
        ; ts_add_front
        ; ts_add_back
        ; ts_left
        ; ts_right
        ; ts_bind
        ; ts_join
        ]
    in
    List.iter ~f:test
      [ []
      ; [ [] ]
      ; [ [] ; [] ]
      ; [ [ 0 ] ]
      ; [ [ 0 ; 1 ] ; [ 2 ; 3 ] ; [] ; [ 1 ; 6 ] ; List.init 10 ~f:Fn.id ]
      ; begin let v = 10 in List.init v ~f:(fun i -> List.init i ~f:Fn.id) end
      ; begin let v = 10 in List.init v ~f:(fun i -> List.init (v - i) ~f:Fn.id) end
      ]
  ;;


  let max_elt    = max_elt
  let min_elt    = min_elt
  let to_array   = to_array
  let to_list    = to_list
  let find_map   = find_map
  let find       = find
  let sum        = sum
  let count      = count
  let for_all    = for_all
  let exists     = exists
  let fold       = fold
  let iter       = iter
  let is_empty   = is_empty
  let length     = length
  let mem        = mem
  let all_unit   = all_unit
  let all_ignore = all_unit
  let all        = all
  let ignore_m   = ignore_m

end
(* This signature is here to remind us to update the unit tests whenever we change
   [Appendable_list]. *)
: module type of Appendable_list))
