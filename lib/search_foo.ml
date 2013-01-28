(* rdouglass: this file used to be called search.ml.  However, search is one of those
magic values that omake cares about, so using it as a file name causes dependency
deadlock.  Feel free to come up with a better name than search_foo *)
open Core.Std

type comparison = int
  
(** cf Jane.Std.Common.ascending *)
let cmp_fasc f = fun x y -> compare (f x) (f y)

(** cf Jane.Std.Common.descending *)
let cmp_fdesc f = fun x y -> compare (f y) (f x)

(** Given a function [f], returns a maximizer of [f]. *)
let maxf f =
  (fun x y -> if (f x) > (f y) then x else y)

(** Given a function [f], returns a minimizer of [f]. *)
let minf f =
  (fun x y -> if (f x) < (f y) then x else y)

let rec in_order lst ~cmp =
  match lst with
  | [] | _::[] -> true
  | first::second::tl ->
      if (cmp first second) = -1
      then (in_order (second::tl) ~cmp)
      else false

(* return the max length of strings in the array *)
let max_len ~key strings =
  Array.fold ~init:0 strings ~f:(fun acc s ->
    max acc (String.length (key s)))

(** Binary search.

    (f i) returns an integer and should be monotonic.
    f should have values for all i in [low,high], inclusive.

    if \E i \in [low,high] such that (f i) = 0,
    then such an i is returned.
    Otherwise, i is returned such that (f i > 0) and (f (i-1) < 0).
    Unless it's all > 0 or all < 0.
    If it's all > 0, then the first i is returned.
    If it's all < 0 , then raise Not_found
*)
let bsearch_internal ~f ~low ~high =
  let rec loop ~f ~low ~high =
    if low = high then
      match f low with
      | 0 -> Some low
      | x -> if x > 0 then Some low else None
    else let mid = (low + high)/2 in
    match f mid with
    | 0 -> Some mid
    | x -> if x > 0
      then loop ~f ~low ~high:mid
      else loop ~f ~low:(mid+1) ~high
  in
  if high < low
  then None
  else loop ~f ~low ~high

let bsearch_opt = bsearch_internal
let bsearch_exn ~f ~low ~high =
  match bsearch_internal ~f ~low ~high with
  | Some x -> x
  | None -> raise Not_found
let bsearch = bsearch_opt

type poly_comparison = [`Low | `Good | `High]
let bsearch2_internal ~f ~low ~high =
  let rec loop ~f ~low ~high =
    if low = high then
      match f low with `Good -> Some low | _ -> None
    else
      let mid = (low + high)/2 in
      match f mid with
      | `Good -> Some mid
      | `Low -> loop ~f ~low:(mid+1) ~high
      | `High -> loop ~f ~low ~high:mid
  in
  if high < low
  then None
  else loop ~f ~low ~high

let bsearch2_opt = bsearch2_internal
let bsearch2_exn ~f ~low ~high =
  match bsearch2_internal ~f ~low ~high with
  | Some x -> x
  | None -> raise Not_found
let bsearch2 = bsearch2_opt


(** similar to bsearch, but returns (index,value) pair.
    f is expected to return a (test,value) pair,
    where test is like the output of f above, and value is some
    related value.  *)
let bsearch_val_internal ~f ~low ~high =
  let rec loop ~f ~low ~high =
    if low = high then
      let (test,value) = f low in
      match test with
      | 0 -> Some (low,value)
      | x -> if x > 0 then Some (low,value) else None
    else
      let mid = (low + high)/2 in
      let (test,value) = f mid in
      match test with
      | 0 -> Some (mid,value)
      | 1 -> loop ~f ~low ~high:mid
      | (-1) -> loop ~f ~low:(mid+1) ~high
      | _ -> raise (Failure ("bsearch_val: " ^
                             "Search returned value other than -1,0,1"))
  in
  if high < low
  then None
  else loop ~f ~low ~high

let bsearch_val_opt = bsearch_val_internal
let bsearch_val_exn ~f ~low ~high =
  match bsearch_val_internal ~f ~low ~high with
  | Some x -> x
  | None -> raise Not_found
let bsearch_val = bsearch_val_opt
