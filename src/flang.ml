open Core

module type Ordered_field_with_exponential = sig
  type t [@@deriving compare]

  val zero : t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val exp : t -> t
  val log : t -> t
end

type 'a t = [
  | `Base of 'a
  | `Add  of 'a t * 'a t
  | `Sub  of 'a t * 'a t
  | `Mult of 'a t * 'a t
  | `Div  of 'a t * 'a t
  | `Abs of 'a t
  | `Min of 'a t * 'a t
  | `Max of 'a t * 'a t
  | `Exp of 'a t
  | `Ln  of 'a t
] [@@deriving sexp, bin_io, compare]

module Eval (F : Ordered_field_with_exponential) = struct
  open F

  let eval t ~f =
    let rec eval = function
      | `Base x      -> f x
      | `Add  (x, y) -> (eval x) + (eval y)
      | `Sub  (x, y) -> (eval x) - (eval y)
      | `Mult (x, y) -> (eval x) * (eval y)
      | `Div  (x, y) -> (eval x) / (eval y)
      | `Min  (x, y) -> min (eval x) (eval y)
      | `Max  (x, y) -> max (eval x) (eval y)
      | `Abs x ->
        let x' = eval x in
        max x' (zero - x')
      | `Exp x -> exp (eval x)
      | `Ln  x -> log (eval x)
    in
    eval t
end

let base a   = `Base a
let add  x y = `Add (x, y)
let sub  x y = `Sub  (x, y)
let mult x y = `Mult (x, y)
let div  x y = `Div  (x, y)
let abs x    = `Abs x
let min x y  = `Min (x, y)
let max x y  = `Max (x, y)
let exp x    = `Exp x
let ln  x    = `Ln  x

let add_list xs =
  match xs with
  | [] ->
    failwith "Flang.add_list: empty list is not allowed"
  | x :: xs -> List.fold xs ~f:add ~init:x
let mult_list xs =
  match xs with
  | [] ->
    failwith "Flang.mult_list: empty list is not allowed"
  | x :: xs -> List.fold xs ~f:mult ~init:x

let t_of_sexp a_of_sexp sexp =
  let is_constructor lc s = String.equal lc (String.lowercase s) in
  let open Sexplib.Type in
  let rec t_of_sexp = function
    | List [Atom c; x; y] when is_constructor "div"  c ->
      `Div  (t_of_sexp x, t_of_sexp y)
    | List [Atom c; x; y] when is_constructor "sub"  c ->
      `Sub  (t_of_sexp x, t_of_sexp y)
    | List (Atom c :: xs) when is_constructor "add"  c ->
      add_list (List.map ~f:t_of_sexp xs)
    | List (Atom c :: xs) when is_constructor "mult" c ->
      mult_list (List.map ~f:t_of_sexp xs)
    | List [Atom c; x] when is_constructor "abs" c ->
      `Abs (t_of_sexp x)
    | List (Atom c :: x :: xs) when is_constructor "min"  c ->
      List.fold (List.map xs ~f:t_of_sexp) ~init:(t_of_sexp x) ~f:min
    | List (Atom c :: x :: xs) when is_constructor "max"  c ->
      List.fold (List.map xs ~f:t_of_sexp) ~init:(t_of_sexp x) ~f:max
    | List [x; Atom "/"; y] -> div  (t_of_sexp x) (t_of_sexp y)
    | List [x; Atom "-"; y] -> sub  (t_of_sexp x) (t_of_sexp y)
    | List [x; Atom "+"; y] -> add  (t_of_sexp x) (t_of_sexp y)
    | List [x; Atom "*"; y] -> mult (t_of_sexp x) (t_of_sexp y)
    | List [Atom "exp"; x] -> exp (t_of_sexp x)
    | List [Atom "ln" ; x] -> ln (t_of_sexp x)
    | sexp -> `Base (a_of_sexp sexp)
  in
  t_of_sexp sexp

let sexp_of_t sexp_of_a t =
  let open Sexplib.Type in
  let rec sexp_of_t = function
    | `Base a -> sexp_of_a a
    | `Add (x, y) -> List [sexp_of_t x; Atom "+"; sexp_of_t y]
    | `Sub (x, y) -> List [sexp_of_t x; Atom "-"; sexp_of_t y]
    | `Mult (x, y) -> List [sexp_of_t x; Atom "*"; sexp_of_t y]
    | `Div (x, y) -> List [sexp_of_t x; Atom "/"; sexp_of_t y]
    | `Min (x, y) -> List [Atom "min"; sexp_of_t x; sexp_of_t y]
    | `Max (x, y) -> List [Atom "max"; sexp_of_t x; sexp_of_t y]
    | `Abs x -> List [Atom "abs"; sexp_of_t x]
    | `Exp x -> List [Atom "exp"; sexp_of_t x]
    | `Ln  x -> List [Atom "ln" ; sexp_of_t x]
  in
  sexp_of_t t

let rec atoms_aux t acc =
  match t with
  | `Base x -> x :: acc
  | `Abs x | `Exp x | `Ln x -> atoms_aux x acc
  | `Add (x, y) | `Sub (x, y) | `Mult (x, y) | `Div (x, y) | `Min (x, y) | `Max (x, y) ->
    atoms_aux x (atoms_aux y acc)

let atoms t = atoms_aux t []

(* The Olang module tests both Flang and Olang. *)
