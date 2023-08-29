open! Core

let%expect_test "create, get, set" =
  let array = Bitarray.create 100 in
  Bitarray.set array 42 true;
  print_endline (Bool.to_string (Bitarray.get array 42));
  [%expect {| true |}];
  print_endline (Bool.to_string (Bitarray.get array 70));
  [%expect {| false |}];
  Bitarray.set array 42 false;
  print_endline (Bool.to_string (Bitarray.get array 42));
  [%expect {| false |}];
  print_endline (Bool.to_string (Bitarray.get array 70));
  [%expect {| false |}]
;;

let matching_array_and_bitarray operations =
  let max_index =
    List.map operations ~f:fst
    |> List.max_elt ~compare:[%compare: int]
    |> Option.value_exn
  in
  let len = 1 + max_index in
  let array = Array.create ~len false in
  let bitarray = Bitarray.create len in
  List.iter operations ~f:(fun (index, value) ->
    Array.set array index value;
    Bitarray.set bitarray index value);
  array, bitarray
;;

let%expect_test "create, get, and set" =
  let generator =
    Quickcheck.Generator.list_non_empty
      (Quickcheck.Generator.tuple2
         Quickcheck.Generator.small_positive_int
         Bool.quickcheck_generator)
  in
  Quickcheck.test generator ~f:(fun operations ->
    let array, bitarray = matching_array_and_bitarray operations in
    let len = Array.length array in
    for i = 0 to len - 1 do
      let array_value = Array.get array i in
      let bitarray_value = Bitarray.get bitarray i in
      if Bool.(array_value <> bitarray_value)
      then
        raise_s
          [%message
            "Array and Bitarray implementations disagree"
              (array_value : bool)
              (bitarray_value : bool)]
    done)
;;

let%expect_test "count" =
  (* verify 1 bit, through edge cases *)
  List.iter [ 0; 1; 61; 62; 63; 123; 124; 125 ] ~f:(fun len ->
    let array = Bitarray.create len in
    if len > 0
    then (
      Bitarray.set array (len - 1) true;
      assert (Bitarray.count array ~len = 1));
    assert (Bitarray.count array ~len:0 = 0));
  let generator =
    Quickcheck.Generator.list_non_empty
      (Quickcheck.Generator.tuple2
         (Int.gen_uniform_incl 0 ((62 * 3) + 31))
         Bool.quickcheck_generator)
  in
  Quickcheck.test generator ~f:(fun operations ->
    let array, bitarray = matching_array_and_bitarray operations in
    let len = Array.length array in
    List.iter operations ~f:(fun (index, value) ->
      Array.set array index value;
      Bitarray.set bitarray index value);
    let array_count ~len =
      let c = ref 0 in
      for i = 0 to len - 1 do
        if array.(i) then incr c
      done;
      !c
    in
    for len = 0 to len do
      if array_count ~len <> Bitarray.count bitarray ~len
      then
        raise_s
          [%message
            "Array and Bitarray implementations disagree"
              (len : int)
              ~array_count:(array_count ~len : int)
              ~bitarray_count:(Bitarray.count bitarray ~len : int)]
    done)
;;
