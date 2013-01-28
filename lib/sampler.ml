(*
    This algorithm was originally described in the paper "A Linear
    Algorithm For Generating Random Numbers With a Given Distribution"
    by Michael Vose.

    original paper: http://web.eecs.utk.edu/~vose/Publications/random.pdf
    decent exposition: http://www.keithschwarz.com/darts-dice-coins/
*)
open Core.Std

type 'a cell = Single of 'a | Branch of float * 'a * 'a

type 'a t = 'a cell array

let sample ?state t =
  let module R = Random.State in
  let s = Option.value state ~default:R.default in
  let i = R.int s (Array.length t) in
  match t.(i) with
  | Single a -> a
  | Branch (p, a, b) -> if R.float s 1.0 < p then a else b

let create dist =
  let dist =
    (*
        the following two steps are fused together into a single
        normalization phase that is both more efficient and more
        numerically stable than the two passes done in sequence.

        1. we support histograms as inputs by scaling all "probabilities"
           so that they add up to 1.  This is done by dividing each
           "probability" by the sum of all the "probabilities".

        2. the remainder of the algorithm works in terms of scaled
           probabilities: multiplied by the size of the distribution.
    *)
    let w = List.fold ~f:(fun sum (_, p) -> sum +. p) ~init:0. dist in
    let n = float (List.length dist) in
    List.map dist ~f:(fun (a, p) -> (a, (p *. n) /. w))
  in
  (* loop invariants:
      1. forall [ p >= 1 | (_, p) <- above ]
      2. forall [ p <  1 | (_, p) <- below ]
      3. length acc + length above + length below = length dist
  *)
  let add (below, above) (x, p) =
    if p < 1. then ((x, p) :: below, above) else (below, (x, p) :: above)
  in
  let rec loop acc = function
    | ((b, pb) :: below, (a, pa) :: above) ->
      let pa = (pa +. pb) -. 1.0 in
      loop (Branch (pb, b, a) :: acc) (add (below, above) (a, pa))
    | (below, (x, _) :: above) | ((x, _) :: below, above) ->
      loop (Single x :: acc) (below, above)
    | ([], []) ->
      Array.of_list acc
  in
  loop [] (List.fold ~f:add ~init:([], []) dist)

TEST_MODULE = struct

  let probs = [
      ("A", 0.083);
      ("B", 0.084);
      ("C", 0.333);
      ("D", 0.500);
    ]

  let t : string t = create probs

  let histogram = String.Table.create ~size:5 ()

  let num_samples = 1_000_000

  TEST_UNIT =
    for _i = 1 to num_samples do
      let key = sample t in
      incr (Hashtbl.find_or_add histogram key ~default:(fun () -> ref 0))
    done

  let test_outcome key =
    let prob = List.Assoc.find_exn probs key in
    let count = !(Hashtbl.find_exn histogram key) in
    let percentage = float count /. float num_samples in
    if Float.abs (percentage -. prob) > 0.001 then
      failwithf "prob = %G; percentage = %G" prob percentage ()

  TEST_UNIT = test_outcome "A"
  TEST_UNIT = test_outcome "B"
  TEST_UNIT = test_outcome "C"
  TEST_UNIT = test_outcome "D"

end

