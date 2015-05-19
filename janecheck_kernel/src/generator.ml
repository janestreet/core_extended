
let module_name = "Janecheck_kernel.Std.Kernel_generator"

module List  = ListLabels
module Array = ArrayLabels

module Approx = struct

  let compare x y =
    let diff = x -. y in
    if diff < -.epsilon_float then -1 else
    if diff >   epsilon_float then  1 else
      0

  let ( =. ) x y = compare x y = 0

end

let float_is_finite float =
  match classify_float float with
  | FP_normal   | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan                 -> false

(* [residual] represents a portion of a generator's probability distribution that remains
   after some of it has been "used up" in a sense.  A [residual] may be:

   [Keep], meaning the whole generator remains and none has been used up.

   [Drop], meaning none of the generator remains and all of it has been used up.

   [Part (p, w, r1, r2)], meaning part of a weighted-choice generator has been used up.
   [p] is the portion (out of 1.0) of the probability distribution that remains, [w] is
   the weight (out of 1.0) of the left branch of the original weighted choice, and [r1]
   and [r2] are the residuals of the left and right branches of the weighted choice,
   respectively.

   Invariant: [Part (_, _, Keep, Keep)] and [Part (_, _, Drop, Drop)] should never be
   created; they should always be just [Keep] and [Drop] respectively.
*)
type residual =
  | Keep
  | Drop
  | Part of float * float * residual * residual

(* ['a t] represents a probability distribution of values of type ['a].

   [Empty] means an empty distribution with no values (guaranteed to fail to produce a
   value).

   [Singleton x] means a distribution with a 100% chance to produce [x].

   [Weighted (w, t1, t2)] means a distribution that recursively produces values from [t1]
   with probability [w] and from [t2] with probability [1. -. w].

   [Of_fun (r, f)] means a distribution that recursively produces values from the
   remainder of the generator [f ()] according to the residual [r].  Keeping residuals in
   the [Of_fun] constructor allows residuals to be applied to generators without caching
   the result of applying [f].
*)
type 'a t =
  | Empty
  | Singleton of 'a
  | Weighted  of float * 'a t * 'a t
  | Of_fun    of residual * (unit -> 'a t)

let empty = Empty
let singleton x = Singleton x
let of_fun f = Of_fun (Keep, f)

let remaining_fraction_of_residual r =
  match r with
  | Keep -> 1.
  | Drop -> 0.
  | Part (p, _, _, _) -> p

let partial ~original_weight_of_left r1 r2 =
  match r1, r2 with
  | Keep, Keep -> Keep
  | Drop, Drop -> Drop
  | _ ->
    let p1 = remaining_fraction_of_residual r1 in
    let p2 = remaining_fraction_of_residual r2 in
    let w1 = p1 *.        original_weight_of_left  in
    let w2 = p2 *. (1. -. original_weight_of_left) in
    let p = w1 +. w2 in
    Part (p, original_weight_of_left, r1, r2)

let mismatch_error ~from_str ~to_str () =
  failwith
    (Printf.sprintf
       "%s: %s changed to %s, possibly due to nondeterministic [of_fun]"
       module_name
       from_str
       to_str)

(* [compose_residuals ~outer ~inner] computes the residual that is equivalent to [outer]
   composed with [inner].  This is used to accumulate residuals in [Of_fun] generators.
   The intended invariant is:

   {[
     apply_residual (compose_residuals ~outer ~inner) t
     =
     apply_residual outer (apply_residual inner t)
   ]}

   See immediately below for [apply_residual].
   See test module at the bottom of the file for tests of the invariant.
*)
let rec compose_residuals ~outer ~inner =
  match outer, inner with

  (* Simple base cases. *)
  | Drop, _ | _, Drop -> Drop
  | Keep, r | r, Keep -> r

  (* When the inner residual drops half of a weighted choice, it will simply promote the
     other branch rather than re-create the choice.  To account for that, we push the
     outer residual into the branch that will be kept. *)
  | outer, Part (_, original_weight_of_left, inner, Drop) ->
    partial ~original_weight_of_left
      (compose_residuals ~outer ~inner)
      Drop
  | outer, Part (_, original_weight_of_left, Drop, inner) ->
    partial ~original_weight_of_left
      Drop
      (compose_residuals ~outer ~inner)

  (* Otherwise, recursively combine both branches. *)
  | Part (_, _, r1_o, r2_o), Part (_, original_weight_of_left, r1_i, r2_i) ->
    partial ~original_weight_of_left
      (compose_residuals ~outer:r1_o ~inner:r1_i)
      (compose_residuals ~outer:r2_o ~inner:r2_i)
;;

(* [apply_residual r t] produces the remainder of [t] according to [r]. *)
let rec apply_residual r t =
  match r, t with

  (* Simple base cases. *)
  | Drop, _ -> Empty
  | Keep, _ -> t

  (* Accumulate residuals at [Of_fun]. *)
  | _, Of_fun (r', f) -> Of_fun (compose_residuals ~outer:r ~inner:r', f)

  (* Error on mismatched residuals. *)
  | Part _, Empty       -> mismatch_error ~from_str:"Empty"     ~to_str:"Part" ()
  | Part _, Singleton _ -> mismatch_error ~from_str:"Singleton" ~to_str:"Part" ()

  (* Dropping half of a weighted choice no longer requires a choice. *)
  | Part (_, _, Drop, r), Weighted (_, _, t) -> apply_residual r t
  | Part (_, _, r, Drop), Weighted (_, t, _) -> apply_residual r t

  (* Otherwise, recur on choices and recompute the weight. *)
  | Part (_, original_weight, r1, r2), Weighted (w, t1, t2) ->
    if not (Approx.( =. ) original_weight w) then
      mismatch_error ()
        ~from_str:(string_of_float w)
        ~to_str:(string_of_float original_weight);
    let w1 = (remaining_fraction_of_residual r1) *. w in
    let w2 = (remaining_fraction_of_residual r2) *. (1. -. w) in
    Weighted (w1 /. (w1 +. w2), apply_residual r1 t1, apply_residual r2 t2)
;;

let rec apply_fun r f =
  match f () with
  | Of_fun (r', f') -> apply_fun (compose_residuals ~outer:r ~inner:r') f'
  | t               -> apply_residual r t

let rec bind t t_of_a =
  of_fun (fun () ->
    match t with
    | Empty                -> Empty
    | Singleton a          -> t_of_a a
    | Weighted (w, t1, t2) -> Weighted (w, bind t1 t_of_a, bind t2 t_of_a)
    | Of_fun (r, f)        -> bind (apply_fun r f) t_of_a)

let rec rev_app_weighted_union_of_neighbors alist acc =
  match alist with
  | [] -> acc
  | [ pair ] -> pair :: acc
  | (w1, t1) :: (w2, t2) :: alist ->
    let w = w1 +. w2 in
    let t = Weighted (w1 /. w, t1, t2) in
    rev_app_weighted_union_of_neighbors alist ((w, t) :: acc)

let weighted_union_of_neighbors alist =
  List.rev (rev_app_weighted_union_of_neighbors alist [])

let rec unchecked_weighted_union alist =
  match alist with
  | []         -> Empty
  | [ (_, t) ] -> t
  | _          -> unchecked_weighted_union (weighted_union_of_neighbors alist)

let check_weight w =
  if w < 0.
  then failwith "Kernel_generator.weighted_union: weight is negative";
  if not (float_is_finite w)
  then failwith "Kernel_generator.weighted_union: weight is infinite or NaN"

let weighted_union alist =
  unchecked_weighted_union (List.filter alist ~f:(fun (w, _) ->
    check_weight w;
    w <> 0.))

TEST = Pervasives.(=) (weighted_union [ 0., Singleton 1 ; 0., Singleton 2 ]) Empty
TEST = Pervasives.(=) (weighted_union [ 0., Singleton 1 ; 1., Singleton 2 ]) (Singleton 2)

let rec inspect_weighted w t acc =
  match t with
  | Weighted (w', t1, t2) ->
    inspect_weighted (w *. w') t1
      (inspect_weighted (w *. (1. -. w')) t2 acc)
  | _ -> (w, t) :: acc

let rec inspect t =
  match t with
  | Empty         -> `Empty
  | Singleton x   -> `Singleton x
  | Of_fun (r, f) -> inspect (apply_fun r f)
  | _             -> `Weighted_union (inspect_weighted 1. t [])

module Choice = struct

  type 'a gen = 'a t

  (* [step] represents a single weighted choice made to reach a random value *)
  type step = { dir : [ `Left | `Right ] ; weight_of_choice : float }

  (* ['a t] contains a chosen [value], then [gen] it was chosen from, and the [path] to
     [value] in [gen] as a list of [step]s (individual weighted choices). *)
  type 'a t =
    { value : 'a
    ; gen   : 'a gen
    ; path  : step list
    } with fields

  (* ['a context] represents partial progress toward choosing a random value from a
     generator: the original generator and a reversed accumulator of weighted choices made
     so far. *)
  type 'a context =
    { original : 'a gen
    ; rev_path : step list
    }

  let apply_context context value =
    { value ; gen = context.original ; path = List.rev context.rev_path }

  let left_context ~weight_of_choice context =
    { context with
      rev_path = { dir = `Left ; weight_of_choice } :: context.rev_path
    }

  let right_context ~weight_of_choice context =
    { context with
      rev_path = { dir = `Right ; weight_of_choice } :: context.rev_path
    }

  let empty_context original = { original ; rev_path = [] }

  let rec residual_of_path path where =
    match path with
    | [] ->
      (match where with
       | `le | `ge       -> Keep
       | `lt | `gt | `ne -> Drop)
    | { dir = `Left ; weight_of_choice } :: path ->
      let r1 = residual_of_path path where in
      let r2 =
        match where with
        | `lt | `le       -> Drop
        | `gt | `ge | `ne -> Keep
      in
      partial r1 r2 ~original_weight_of_left:weight_of_choice
    | { dir = `Right ; weight_of_choice } :: path ->
      let r1 =
        match where with
        | `gt | `ge       -> Drop
        | `lt | `le | `ne -> Keep
      in
      let r2 = residual_of_path path where in
      partial r1 r2 ~original_weight_of_left:(1. -. weight_of_choice)

  let residual_of_context context =
    residual_of_path (List.rev context.rev_path) `ne

  let lt t = apply_residual (residual_of_path t.path `lt) t.gen
  let le t = apply_residual (residual_of_path t.path `le) t.gen
  let ne t = apply_residual (residual_of_path t.path `ne) t.gen
  let ge t = apply_residual (residual_of_path t.path `ge) t.gen
  let gt t = apply_residual (residual_of_path t.path `gt) t.gen

end

let rec build_choices t context =
  match t with
  | Empty           -> Empty
  | Singleton value -> Singleton (Choice.apply_context context value)
  | Of_fun (r, f)   -> Of_fun (Keep, (fun () -> build_choices (apply_fun r f) context))
  | Weighted (w, t1, t2) ->
    let t1' =
      build_choices t1 (Choice.left_context  context ~weight_of_choice:w)
    in
    let t2' =
      build_choices t2 (Choice.right_context context ~weight_of_choice:(1. -. w))
    in
    Weighted (w, t1', t2')

let choices t = build_choices t (Choice.empty_context t)
let bind_choice t t_of_choice = bind (choices t) t_of_choice

let rec choose_residual t context ~prob ~random_float =
  (* Rejecting very low cumulative probabilities helps avoid branches that never bottom
     out.  Because the branch has low probability, however, there is little to be gained
     by dropping it from the residual.  We fail, leave the branch in place, and hope it
     does not come up again. *)
  if prob < epsilon_float
  then `Failure Keep
  else
    match t with
    | Empty           -> `Failure (Choice.residual_of_context context)
    | Singleton value -> `Success (Choice.apply_context context value)
    | Of_fun (r', t') -> choose_residual (apply_fun r' t') context ~prob ~random_float
    | Weighted (w, t1, t2) ->
      if (random_float ()) < w
      then
        let weight_of_choice = w in
        let prob = prob *. weight_of_choice in
        let context = Choice.left_context ~weight_of_choice context in
        choose_residual t1 context ~prob ~random_float
      else
        let weight_of_choice = (1. -. w) in
        let prob = prob *. weight_of_choice in
        let context = Choice.right_context ~weight_of_choice context in
        choose_residual t2 context ~prob ~random_float

(* [choose] is implemented in terms of [choose_residual] and [Choice] so that, as
   subtrees are eliminated from the generator, only residuals are accumulated.
   Specifically, the results of [apply_fun] are never cached. *)
let choose t ~random_float_between_zero_and_one:random_float =
  match choose_residual t (Choice.empty_context t) ~prob:1. ~random_float with
  | `Failure Drop -> `Empty
  | `Failure r    -> `Failure (apply_residual r t)
  | `Success c    -> `Success c

TEST_MODULE "raw generator" = struct

  open Sexplib.Std
  module Sexp = Sexplib.Sexp

  (* This registers a top-level exception printer that uses sexp conversions. *)
  let () =
    Printexc.register_printer (fun exc ->
      match Sexplib.Conv.sexp_of_exn_opt exc with
      | None -> None
      | Some sexp ->
        Some (Sexp.to_string_hum ~indent:2 sexp))

  type approx_float = float with sexp

  let compare_approx_float = Approx.compare

  type approx_residual =
    [ `Keep
    | `Drop
    | `Part of approx_float * approx_float * approx_residual * approx_residual
    ] with sexp, compare

  type 'a approx_t =
    [ `Empty
    | `Singleton of 'a
    | `Of_fun of approx_residual * 'a approx_t
    | `Weighted of approx_float * 'a approx_t * 'a approx_t
    ] with sexp, compare

  let rec approximate_residual r =
    match r with
    | Keep -> `Keep
    | Drop -> `Drop
    | Part (p, w, r1, r2) ->
      `Part (p, w, approximate_residual r1, approximate_residual r2)

  let rec approximate_t t =
    match t with
    | Empty                 -> `Empty
    | Singleton a           -> `Singleton a
    | Of_fun    (r, f)      -> `Of_fun    (approximate_residual r, approximate_t (f ()))
    | Weighted  (w, t1, t2) -> `Weighted  (w, approximate_t t1, approximate_t t2)

  let sexp_of_residual r = <:sexp_of< approx_residual >> (approximate_residual r)
  let sexp_of_t sexp_of_a t = <:sexp_of< a approx_t >> (approximate_t t)

  let rec residuals_of_t t =
    match t with
    | Empty       -> [ Keep ; Drop ]
    | Singleton _ -> [ Keep ; Drop ]
    | Of_fun (r, t) -> residuals_of_t (apply_fun r t)
    | Weighted (original_weight_of_left, t1, t2) ->
      let residuals1 = residuals_of_t t1 in
      let residuals2 = residuals_of_t t2 in
      List.fold_left ~init:[] ~f:List.rev_append
        (List.rev_map residuals1 ~f:(fun r1 ->
           List.rev_map residuals2 ~f:(fun r2 ->
             partial ~original_weight_of_left r1 r2)))

  let rec make_sub_int_t ~lower_inclusive ~upper_exclusive =
    if lower_inclusive >= upper_exclusive
    then Empty
    else if lower_inclusive = upper_exclusive - 1
    then Singleton lower_inclusive
    else
      let middle = (lower_inclusive + upper_exclusive) / 2 in
      let w =
        float (middle          - lower_inclusive) /.
        float (upper_exclusive - lower_inclusive)
      in
      let t1 =
        Of_fun (Keep, fun () -> make_sub_int_t ~lower_inclusive ~upper_exclusive:middle)
      in
      let t2 =
        Of_fun (Keep, fun () -> make_sub_int_t ~lower_inclusive:middle ~upper_exclusive)
      in
      Weighted (w, t1, t2)

  let make_int_t n = make_sub_int_t ~lower_inclusive:0 ~upper_exclusive:n

  let init n ~f =
    let rec loop i =
      if i = n
      then []
      else f i :: loop (i + 1)
    in
    loop 0

  let int_t_list = init 10 ~f:make_int_t

  let seed_arrays = init 10 ~f:(fun i -> Array.init i ~f:(fun x -> x))

  let rec to_list t =
    match t with
    | Empty                -> []
    | Singleton x          -> [x]
    | Of_fun (r, f)        -> to_list (apply_fun r f)
    | Weighted (_, t1, t2) -> to_list t1 @ to_list t2

  module Int_set = struct
    include Set.Make(struct
      type t = int
      let compare (x:int) y = Pervasives.compare x y
    end)
    let sexp_of_t t = <:sexp_of< int list >> (elements t)
  end

  let to_set t = to_list t |> Int_set.of_list

  exception Janecheck of string * Sexp.t * exn with sexp

  let with_note msg sexp_of_a a f =
    match f () with
    | x -> x
    | exception exn -> raise (Janecheck (msg, sexp_of_a a, exn))

  let iter list msg sexp_of f =
    List.iter list ~f:(fun x ->
      with_note msg sexp_of x (fun () -> f x))

  let choose_by_state t random_state =
    let random_float_between_zero_and_one () =
      Random.State.float random_state 1.
    in
    choose t ~random_float_between_zero_and_one

  let rec choose_set_and_test t random_state =
    match choose_by_state t random_state with
    | `Empty ->
      <:test_result< Int_set.t >> (to_set t) ~expect:Int_set.empty;
      Int_set.empty
    | `Failure t' ->
      <:test_result< Int_set.t >> (to_set t') ~expect:(to_set t);
      choose_set_and_test t' random_state
    | `Success c ->
      let t' = Choice.ne c in
      let value = Choice.value c in
      <:test_result< Int_set.t >> (to_set t') ~expect:(Int_set.remove value (to_set t));
      Int_set.add value (choose_set_and_test t' random_state)

  TEST_UNIT "apply_residual" =
    iter int_t_list "generator" <:sexp_of< int t >> (fun t ->
      iter (residuals_of_t t) "residual" <:sexp_of< residual >> (fun r ->
        let t' = apply_residual r t in
        assert (Int_set.subset (to_set t') (to_set t))))

  TEST_UNIT "compose_residuals" =
    iter int_t_list "original generator" <:sexp_of< int t >> (fun t ->
      iter (residuals_of_t t) "inner residual" <:sexp_of< residual >> (fun inner ->
        let t' = apply_residual inner t in
        with_note "intermediate generator" <:sexp_of< int t >> t' (fun () ->
          iter (residuals_of_t t') "outer residual" <:sexp_of< residual >> (fun outer ->
            <:test_eq< int approx_t >>
              (approximate_t (apply_residual outer (apply_residual inner t)))
              (approximate_t (apply_residual (compose_residuals ~outer ~inner) t))))))

  TEST_UNIT "Choice.ne" =
    iter seed_arrays "seed" <:sexp_of< int array >> (fun seed ->
      iter int_t_list "generator" <:sexp_of< int t >> (fun t ->
        let random_state = Random.State.make seed in
        match choose_by_state t random_state with
        | `Empty | `Failure _ -> ()
        | `Success c ->
          <:test_result< Int_set.t >>
            (to_set (Choice.ne c))
            ~expect:(Int_set.remove (Choice.value c) (to_set t))))

  TEST_UNIT "choose" =
    iter seed_arrays "seed" <:sexp_of< int array >> (fun seed ->
      iter int_t_list "generator" <:sexp_of< int t >> (fun t ->
        let random_state = Random.State.make seed in
        <:test_result< Int_set.t >>
          (choose_set_and_test t random_state)
          ~expect:(to_set t)))

end
