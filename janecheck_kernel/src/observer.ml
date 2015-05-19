
module Sexp = Sexplib.Sexp
module Gen = Generator

type 'a raw =
  | Singleton : _ raw
  | Variant   : 'a raw * 'b raw              -> [ `A of 'a | `B of 'b ] raw
  | Tuple     : 'a raw * 'b raw              -> ('a * 'b) raw
  | Unmap     : 'a raw * ('b -> 'a) * Sexp.t -> 'b raw

type 'a t = 'a raw Gen.t

let singleton () = Gen.singleton Singleton

let variant a b =
  Gen.bind a (fun a_raw ->
    Gen.bind b (fun b_raw ->
      Gen.singleton (Variant (a_raw, b_raw))))

let tuple a b =
  Gen.bind a (fun a_raw ->
    Gen.bind b (fun b_raw ->
      Gen.singleton (Tuple (a_raw, b_raw))))

let of_fun f = Gen.of_fun f

let unmap t ~f ~f_sexp =
  Gen.bind t (fun raw ->
    Gen.singleton (Unmap (raw, f, f_sexp)))

let const_with_sexp b sexp_of_b =
  (fun _ -> b),
  <:sexp_of< [`const of b] >> (`const b)

let cases_with_sexp f_a sexp_a f_b sexp_b =
  (function `A a -> f_a a | `B b -> f_b b),
  <:sexp_of< [`cases of [`A of Sexp.t] * [`B of Sexp.t]] >>
    (`cases (`A sexp_a, `B sexp_b))

let uncurry_with_sexp f sexp =
  (fun (a, b) -> (fst (f a)) b),
  <:sexp_of< [`uncurry of Sexp.t] >> (`uncurry sexp)

let compose_with_sexp f sexp_f g sexp_g =
  (fun x -> f (g x)),
  <:sexp_of< [`compose_left_to_right of Sexp.t * Sexp.t] >>
    (`compose_left_to_right (sexp_g, sexp_f))

let rec observe_raw
  : type a b . a raw -> b Gen.t -> sexp_of_rng:(b -> Sexp.t) -> ((a -> b) * Sexp.t) Gen.t
  = fun raw gen ~sexp_of_rng ->
    match raw with
    | Singleton ->
      Gen.bind gen (fun y ->
        Gen.singleton (const_with_sexp y sexp_of_rng))
    | Variant (raw_a, raw_b) ->
      Gen.of_fun (fun () ->
        Gen.bind (observe_raw raw_a gen ~sexp_of_rng) (fun (f_a, sexp_a) ->
          Gen.bind (observe_raw raw_b gen ~sexp_of_rng) (fun (f_b, sexp_b) ->
            Gen.singleton (cases_with_sexp f_a sexp_a f_b sexp_b))))
    | Tuple (raw_a, raw_b) ->
      Gen.bind (observe_raw raw_a (observe_raw raw_b gen ~sexp_of_rng) ~sexp_of_rng:snd)
        (fun (f, sexp) ->
           Gen.singleton (uncurry_with_sexp f sexp))
    | Unmap (raw, f, sexp_f) ->
      Gen.bind (observe_raw raw gen ~sexp_of_rng) (fun (g, sexp_g) ->
        Gen.singleton (compose_with_sexp g sexp_g f sexp_f))

let observe t gen ~sexp_of_rng =
  Gen.bind t (fun raw ->
    observe_raw raw gen ~sexp_of_rng)
