open Sexplib.Std

module Sexp  = Sexplib.Sexp
module List  = ListLabels
module Array = ArrayLabels

open Janecheck_intf

module Configure (Config : Janecheck_kernel_config) = struct

  include Config

  let random_state_of_seed seed =
    match seed with
    | `Nondeterministic  -> Random_state.make_self_init ()
    | `Deterministic str ->
      let array = Array.init (String.length str) ~f:(fun i -> Char.code str.[i]) in
      Random_state.make array

  let iter
        ?(seed     = default_seed)
        ?(trials   = default_trial_count)
        ?(attempts = default_attempt_factor * trials)
        gen ~f =
    let random_state = random_state_of_seed seed in
    let random_float_between_zero_and_one () =
      Random_state.float random_state 1.
    in
    let rec loop gen ~remaining_trials ~remaining_attempts =
      if remaining_trials < 1
      then ()
      else if remaining_attempts < 1
      then
        failwith
          (Printf.sprintf "Janecheck.iter: failed to generate %d inputs in %d attempts"
             trials
             attempts)
      else
        match Generator.choose gen ~random_float_between_zero_and_one with
        | `Empty          -> ()
        | `Failure gen    ->
          loop gen
            ~remaining_trials
            ~remaining_attempts:(remaining_attempts-1)
        | `Success choice ->
          f (Generator.Choice.value choice);
          loop (Generator.Choice.ne choice)
            ~remaining_trials:(remaining_trials-1)
            ~remaining_attempts:(remaining_attempts-1)
    in
    loop gen ~remaining_trials:trials ~remaining_attempts:attempts

  let random_value ?seed gen =
    let r = ref None in
    iter ?seed ~trials:1 gen ~f:(fun x -> r := Some x);
    match !r with
    | None   -> raise Not_found
    | Some x -> x

  exception Random_input of Sexp.t * exn with sexp

  let test ?seed ?trials ?attempts ?sexp_of gen ~f =
    let f_with_sexp =
      match sexp_of with
      | None -> f
      | Some sexp_of_arg ->
        (fun x ->
           try f x with exn ->
             raise (Random_input (sexp_of_arg x, exn)))
    in
    iter ?seed ?trials ?attempts gen ~f:f_with_sexp

  let test_no_duplicates (type a)
        ?seed
        ?(trials  = default_trial_count * 10)
        ?attempts
        ?sexp_of
        gen ~by =
    let f =
      match by with
      | `Equal equal ->
        let r = ref [] in
        (fun x ->
           if List.exists !r ~f:(fun y -> equal x y)
           then failwith "duplicate value"
           else r := x :: !r)
      | `Compare compare ->
        let module T = struct
          type t = a
          let compare = compare
        end in
        let module S = Set.Make (T) in
        let set = ref S.empty in
        (fun x ->
           if S.mem x !set
           then failwith "duplicate value"
           else set := S.add x !set)
    in
    test ?seed ~trials ?attempts ?sexp_of gen ~f

  exception Can_generate
  exception Cannot_generate with sexp
  exception Cannot_generate_from_any_of of Sexp.t with sexp

  let test_can_generate ?seed ?trials ?attempts ?sexp_of gen ~f =
    let r = ref [] in
    let f_and_enqueue x = r := x :: !r; if f x then raise Can_generate in
    match iter ?seed ?trials ?attempts gen ~f:f_and_enqueue with
    | exception Can_generate -> ()
    | () ->
      match sexp_of with
      | None ->
        raise Cannot_generate
      | Some sexp_of_value ->
        raise (Cannot_generate_from_any_of (<:sexp_of< value list >> !r))

end

include Configure (struct
    module Random_state = Random.State
    let default_seed           = `Deterministic "an arbitrary but deterministic string"
    let default_trial_count    = 100
    let default_attempt_factor = 10
  end)
