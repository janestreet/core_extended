open Core

type 'a t = [
  | `GT of 'a * 'a
  | `LT of 'a * 'a
  | `GE of 'a * 'a
  | `LE of 'a * 'a
  | `EQ of 'a * 'a
  | `NE of 'a * 'a
  | `One_of of 'a * 'a list
] [@@deriving bin_io, sexp, compare]

let t_of_sexp a_of_sexp sexp =
  let open Sexplib.Type in
  match sexp with
  | List [x; Atom ">"; y]  -> `GT (a_of_sexp x, a_of_sexp y)
  | List [x; Atom "<"; y]  -> `LT (a_of_sexp x, a_of_sexp y)
  | List [x; Atom ">="; y] -> `GE (a_of_sexp x, a_of_sexp y)
  | List [x; Atom "<="; y] -> `LE (a_of_sexp x, a_of_sexp y)
  | List [x; Atom "="; y]  -> `EQ (a_of_sexp x, a_of_sexp y)
  | List [x; Atom "<>"; y] -> `NE (a_of_sexp x, a_of_sexp y)
  | List [x; Atom "one-of"; List xs]
    -> `One_of (a_of_sexp x, List.map xs ~f:a_of_sexp)
  | _ -> failwithf "bad predicate sexp: %s" (Sexp.to_string_hum sexp) ()

let sexp_of_t sexp_of_a t =
  let open Sexplib.Type in
  match t with
  | `GT (x, y) -> List [sexp_of_a x; Atom ">"; sexp_of_a y]
  | `LT (x, y) -> List [sexp_of_a x; Atom "<"; sexp_of_a y]
  | `GE (x, y) -> List [sexp_of_a x; Atom ">="; sexp_of_a y]
  | `LE (x, y) -> List [sexp_of_a x; Atom "<="; sexp_of_a y]
  | `EQ (x, y) -> List [sexp_of_a x; Atom "="; sexp_of_a y]
  | `NE (x, y) -> List [sexp_of_a x; Atom "<>"; sexp_of_a y]
  | `One_of (x, ys) ->
    List [sexp_of_a x; Atom "one-of"; List (List.map ~f:sexp_of_a ys)]

include struct
  open Int.Replace_polymorphic_compare
  let eval ~compare = function
    | `GT (x, y) -> compare x y > 0
    | `LT (x, y) -> compare x y < 0
    | `GE (x, y) -> compare x y >= 0
    | `LE (x, y) -> compare x y <= 0
    | `EQ (x, y) -> compare x y = 0
    | `NE (x, y) -> not (compare x y = 0)
    | `One_of (x, ys) -> List.mem ys x ~equal:(fun x y -> compare x y = 0)
end

let%test_module _ = (module struct

  module Term0 = struct
    type t = [ `C of float | `V of string ] [@@deriving sexp, bin_io, compare]

    let t_of_sexp = function
      | Sexp.Atom x -> (try `C (Float.of_string x) with _ -> `V x)
      | sexp -> t_of_sexp sexp

    let sexp_of_t = function
      | `C x -> Float.sexp_of_t x
      | `V v -> String.sexp_of_t v
  end

  module Term = struct
    include (Flang : (module type of Flang) with type 'a t := 'a Flang.t)
    include Eval (Float)
    type t = Term0.t Flang.t [@@deriving sexp, compare]

    let x = base (`V "x")
    let const value = base (`C value)
    let ( + )   = add
    let ( * )   = mult
  end

  let sexp_of_pred = sexp_of_t Term.sexp_of_t
  let pred_of_sexp = t_of_sexp Term.t_of_sexp

  let env x = function
    | "x" -> Int.to_float x
    | _ -> assert false

  let eval_term ~x term =
    let sexp_of_term = Term.sexp_of_t term in
    [%test_eq: Term.t] term (Term.t_of_sexp sexp_of_term)
      ~message:"Term sexp roundtrip";
    let env = env x in
    Term.eval term ~f:(function `C v -> v | `V x -> env x)

  let eval_pred pred ~x =
    (* Roundtrip is tested further down. *)
    let eval_term = eval_term ~x in
    eval pred ~compare:(fun t t' -> Float.compare (eval_term t) (eval_term t'))

  let sexp1 = Sexp.of_string "((x + 1) * (x + 1))"
  let term1 = Term.((x + const 1.) * (x + const 1.))

  let sexp2 = Sexp.of_string "(((x * x) + (2 * x)) + 1)"
  let term2 = Term.((x * x) + (const 2. * x) + const 1.)

  let sexp3 = Sexp.of_string "(min x (max x (abs x)))"
  let term3 = Term.(min x (max x (abs x)))

  let sexp4 = Sexp.of_string "(ln (exp x))"
  let term4 = Term.(ln (exp x))

  let sexp5 = Sexp.of_string "(min x (abs x) (max x (abs x)))"
  let term5 = Term.(min (min x (abs x)) (max x (abs x)))

  let sexp6 = Sexp.of_string "(min 1 2 3 4)"
  let term6 = Term.(min (min (min (const 1.) (const 2.)) (const 3.)) (const 4.))

  let sexp7 = Sexp.of_string "(max x)"
  let term7 = Term.(x)

  let%test_unit "sexp term1" =
    [%test_result: Term.t] ~expect:term1 (Term.t_of_sexp sexp1)

  let%test_unit "sexp term2" =
    [%test_result: Term.t] ~expect:term2 (Term.t_of_sexp sexp2)

  let%test_unit "sexp term3" =
    [%test_result: Term.t] ~expect:term3 (Term.t_of_sexp sexp3)

  let%test_unit "sexp term4" =
    [%test_result: Term.t] ~expect:term4 (Term.t_of_sexp sexp4)

  let%test_unit "sexp term5" =
    [%test_result: Term.t] ~expect:term5 (Term.t_of_sexp sexp5)

  let%test_unit "sexp term6" =
    [%test_result: Term.t] ~expect:term6 (Term.t_of_sexp sexp6)

  let%test_unit "sexp term7" =
    [%test_result: Term.t] ~expect:term7 (Term.t_of_sexp sexp7)

  let%test_unit "evaluate arithmetic" =
    let test = [%test_result: Float.t] ~equal:Float.(=.) in
    for x = 1 to 100 do
      test ~expect:(Int.to_float ((x + 1) * (x + 1))) (eval_term term1 ~x);
      test ~expect:(Int.to_float ((x + 1) * (x + 1))) (eval_term term2 ~x)
    done

  let%test_unit "evaluate min-max" =
    let test = [%test_result: Float.t] ~equal:Float.(=.) in
    for x = -100 to 100 do
      test ~expect:(Int.to_float x) (eval_term term3 ~x)
    done

  let%test_unit "evaluate predicate" =
    for x = 1 to 100 do
      [%test_pred: Term.t t * int] (fun (p, x) -> eval_pred p ~x)
        (`EQ (term1, term2), x);
      [%test_pred: Term.t t * int] (fun (p, x) -> not (eval_pred p ~x))
        (`GT (term1, term2), x)
    done

  let%test_unit "predicate sexp" =
    let x = Term.base (`V "x") in
    let const value = Term.base (`C value) in
    List.iter ~f:(fun (s, expect) ->
      let actual = [%of_sexp: Term.t t] (Sexp.of_string s) in
      [%test_result: Term.t t] actual ~expect)
      [ "((x + 1) > 4)",
        `GT (Term.add x (const 1.), const 4.);

        "((x * x) one-of ((2 / x) 4))",
        `One_of (Term.mult x x, [Term.div (const 2.) x; const 4.]);

        "((x + 1.5) <> (x + 1.6))",
        `NE (Term.add x (const 1.5), Term.add x (const 1.6))
      ]

  let eval_pred_s pred ~x =
    let sexp1 = Sexp.of_string pred in
    let pred = pred_of_sexp sexp1 in
    let sexp2 = sexp_of_pred pred in
    [%test_result: Sexp.t] ~expect:sexp1 sexp2
      ~message:"Pred sexp roundtrip";
    eval_pred pred ~x

  let%test _ = eval_pred_s "((x + 1) = 4)"               ~x:3
  let%test _ = eval_pred_s "((x * x) = 9)"               ~x:3
  let%test _ = eval_pred_s "((x * x) > (x + x))"         ~x:3
  let%test _ = eval_pred_s "((x * x) <> 8)"              ~x:3
  let%test _ = eval_pred_s "((x * x) one-of (1 2 9 12))" ~x:3
  let%test _ = eval_pred_s "((x / 2) = 1.5)"             ~x:3
  let%test _ = eval_pred_s "((x - 10) = -7)"             ~x:3
end)
