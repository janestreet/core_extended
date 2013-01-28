open Core.Std

type t = char List_zipper.t

open List_zipper

let drop_before = drop_before
let drop_after = drop_after
let drop_all_before = drop_all_before
let drop_all_after = drop_all_after
let insert_before = insert_before
let insert_after = insert_after
let previous = previous
let next = next

let contents zip =
  let ll = List.length zip.l
  and lr = List.length zip.r in
  let res = String.create (ll+lr) in
  List.iteri zip.l
    ~f:(fun i c -> res.[ll-1-i] <- c);
  List.iteri zip.r
    ~f:(fun i c -> res.[ll+i] <- c);
  res

let left_contents zip =
  let len = List.length zip.l in
  let res = String.create len in
  List.iteri zip.l
    ~f:(fun i c -> res.[len-1-i] <- c);
  res

let right_contents zip =
  let len = List.length zip.r in
  let res = String.create len in
  List.iteri zip.r
    ~f:(fun i c -> res.[i] <- c);
  res

let first zip =
  {
    l = [];
    r = List.rev zip.l @ zip.r;
  }

let last zip =
  {
    l = List.rev zip.r @ zip.l;
    r = [];
  }

let create left right =
  {
    l = String.to_list_rev left;
    r = String.to_list right
  }

let replace_left z l = replace_left z (String.to_list_rev l)
let replace_right z r = replace_right z (String.to_list r)
