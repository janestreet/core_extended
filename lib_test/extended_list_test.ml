open Core.Std
open OUnit

module L = Core_extended.Std.List

let is_even x = x mod 2 = 0

let test = "Extended_list" >::: [
  "number" >::
    (fun () ->
       "base" @? (L.number [1;2;3;1;4] = [1,0;2,0;3,0;1,1;4,0]));
  "multimerge" >::
    (fun () ->
       "base" @? (L.multimerge [[0;2];[2;3];[0;1];[1;2]] = [0;1;2;3]);
       "dup" @? (L.multimerge [[0;1;2;0];[0;1]] = [0;1;2;0]);
       (* There is no solution here: we just want to make sure that the
          result has all the fields. *)
       "circle" @? (
         let header = L.multimerge [[0;1;2];[0;2;1;4]] in
         List.sort ~cmp:Int.compare header = [0;1;2;4]));
  ("take_while" >:: fun () ->
    "take evens" @? (
      (L.take_while [2;4;6;7;8;9] is_even) = [2;4;6]));
  ("equal" >:::
    let equal xs ys = L.equal ~equal:Int.equal xs ys in
    let assert_equal xs ys = assert (equal xs ys) in
    let assert_not_equal xs ys = assert (not (equal xs ys)) in
  [
    ("1" >:: fun () -> assert_equal     []     []);
    ("2" >:: fun () -> assert_not_equal [2]    []);
    ("3" >:: fun () -> assert_not_equal []     [3]);
    ("4" >:: fun () -> assert_equal     [4]    [4]);
    ("5" >:: fun () -> assert_not_equal [0; 5] [0]);
    ("6" >:: fun () -> assert_not_equal [0]    [0; 6]);
    ("7" >:: fun () -> assert_equal     [0; 7] [0; 7]);
  ]);
  ("compare" >:::
    let compare xs ys = L.compare ~cmp:Int.compare xs ys in
    let assert_eq xs ys = assert (compare xs ys = 0) in
    let assert_lt xs ys = assert (compare xs ys < 0) in
    let assert_gt xs ys = assert (compare xs ys > 0) in
  [
    ("1" >:: fun () -> assert_eq  []     []);
    ("2" >:: fun () -> assert_gt  [2]    []);
    ("3" >:: fun () -> assert_lt  []     [3]);
    ("4" >:: fun () -> assert_eq  [4]    [4]);
    ("4" >:: fun () -> assert_lt  [3]    [4]);
    ("4" >:: fun () -> assert_gt  [3]    [2]);
    ("5" >:: fun () -> assert_gt  [0; 5] [0]);
    ("6" >:: fun () -> assert_lt  [0]    [0; 6]);
    ("5" >:: fun () -> assert_lt  [0; 5] [1]);
    ("6" >:: fun () -> assert_gt  [1]    [0; 6]);
    ("7" >:: fun () -> assert_eq  [0; 7] [0; 7]);
  ]);
]
