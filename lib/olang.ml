open Core.Std

type 'a t = [
  | `GT of 'a * 'a
  | `LT of 'a * 'a
  | `GE of 'a * 'a
  | `LE of 'a * 'a
  | `EQ of 'a * 'a
  | `NE of 'a * 'a
  | `One_of of 'a * 'a list
] with bin_io, sexp


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

TEST_MODULE = struct

  module Term0 = struct
    type t = [ `C of float | `V of string ] with sexp, bin_io

    let t_of_sexp = function
      | Sexp.Atom x -> (try `C (Float.of_string x) with _ -> `V x)
      | sexp -> t_of_sexp sexp
  end

  module Term = struct
    include (Flang : (module type of Flang) with type 'a t := 'a Flang.t)
    module X = Flang.Eval (Float)
    let eval = X.eval
    type t = Term0.t Flang.t with sexp
  end

  let env x = function
    | "x" -> Int.to_float x
    | _ -> assert false

  let eval_term ~x term =
    let env = env x in
    Term.eval term ~f:(function `C v -> v | `V x -> env x)

  let eval_pred pred ~x =
    let eval_term = eval_term ~x in
    eval pred ~compare:(fun t t' -> Float.compare (eval_term t) (eval_term t'))

  let sexp1 = Sexp.of_string "((x + 1) * (x + 1))"
  let sexp2 = Sexp.of_string "(((x * x) + (2 * x)) + 1)"
  let term1, term2 =
    let x = Term.base (`V "x") in
    let const value = Term.base (`C value) in
    let ( + )   = Term.add in
    let ( * )   = Term.mult in
    ((x + const 1.) * (x + const 1.),
     (x * x) + (const 2. * x) + const 1.)

  TEST_UNIT "sexp term" =
    assert (Term.t_of_sexp sexp1 = term1);
    assert (Term.t_of_sexp sexp2 = term2)

  TEST_UNIT "evaluate term" =
    for x = 1 to 100 do
      assert (eval_term term1 ~x =. Int.to_float ((x + 1) * (x + 1)));
      assert (eval_term term2 ~x =. Int.to_float ((x + 1) * (x + 1)));
    done

  TEST_UNIT "evaluate predicate" =
    for x = 1 to 100 do
      assert (eval_pred (`EQ (term1, term2)) ~x = true);
      assert (eval_pred (`GT (term1, term2)) ~x = false);
    done

  TEST_UNIT "predicate sexp" =
    let x = Term.base (`V "x") in
    let const value = Term.base (`C value) in
    List.iter ~f:(fun (s, expected) ->
      let actual = t_of_sexp Term.t_of_sexp (Sexp.of_string s) in
      assert (Polymorphic_compare.(=) actual expected))
      [ "((x + 1) > 4)",
        `GT (Term.add x (const 1.), const 4.);

        "((x * x) one-of ((2 / x) 4))",
        `One_of (Term.mult x x, [Term.div (const 2.) x; const 4.]);

        "((x + 1.5) <> (x + 1.6))",
        `NE (Term.add x (const 1.5), Term.add x (const 1.6))
      ]

  let eval_pred_s pred ~x =
    eval_pred (t_of_sexp Term.t_of_sexp (Sexp.of_string pred)) ~x

  TEST = eval_pred_s "((x + 1) = 4)"               ~x:3
  TEST = eval_pred_s "((x * x) = 9)"               ~x:3
  TEST = eval_pred_s "((x * x) > (x + x))"         ~x:3
  TEST = eval_pred_s "((x * x) <> 8)"              ~x:3
  TEST = eval_pred_s "((x * x) one-of (1 2 9 12))" ~x:3
  TEST = eval_pred_s "((x / 2) = 1.5)"             ~x:3
  TEST = eval_pred_s "((x - 10) = -7)"             ~x:3
end
