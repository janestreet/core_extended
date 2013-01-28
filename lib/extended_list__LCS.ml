open Core.Std

(* LCS.... *)
let lcsTBL x y m n =
  let c = Array.make_matrix ~dimx:(m+1) ~dimy:(n+1) 0 in
  for i = 0 to m-1 do
    for j = 0 to n -1 do
      if x.(i) = y.(j) then
        c.(i+1).(j+1) <- c.(i).(j) + 1
      else
        c.(i+1).(j+1) <- max c.(i+1).(j) c.(i).(j+1)
    done
  done;
  c

let rec lcsBacktrace c x y i j acc =
  if i=0 || j=0 then
    acc
  else if x.(i-1) = y.(j-1) then
    lcsBacktrace c x y (i-1) (j-1) (x.(i-1)::acc)
  else if c.(i).(j-1) > c.(i-1).(j) then
    lcsBacktrace c x y i (j-1) acc
  else
    lcsBacktrace c x y (i-1) j acc

(** Naive dynamic programming LCS *)
let lcs_kernel x y =
  let m = Array.length x
  and n = Array.length y in
  let c = lcsTBL x y m n in
  lcsBacktrace c x y m n []

(** Find common front part for an LCS *)
let rec common_start x y acc =
  match x,y with
  | h::t,h'::t' when h = h' -> common_start t t' (h::acc)
  | _ -> acc,x,y

(** LCS with common front and back part detection optimization.*)
let lcs x y =
  let rev_start,x,y = common_start x y [] in
  let stop,rev_x,rev_y = common_start (List.rev x) (List.rev y) [] in
  let lcs_middle = lcs_kernel (Array.of_list rev_x) (Array.of_list rev_y) in
  List.rev_append rev_start (List.rev_append lcs_middle stop)
