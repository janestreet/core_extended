open Core.Std

module type Ordered_field = sig
  type t [@@deriving compare, sexp]
  val zero : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
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
] [@@deriving sexp, bin_io, compare]

let base a   = `Base a
let add  x y = `Add (x, y)
let sub  x y = `Sub  (x, y)
let mult x y = `Mult (x, y)
let div  x y = `Div  (x, y)
let abs x    = `Abs x
let min x y  = `Min (x, y)
let max x y  = `Max (x, y)

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
    | List [Atom c; x; y] when is_constructor "min"  c ->
      `Min  (t_of_sexp x, t_of_sexp y)
    | List [Atom c; x; y] when is_constructor "max"  c ->
      `Max  (t_of_sexp x, t_of_sexp y)
    | List [x; Atom "/"; y] -> div  (t_of_sexp x) (t_of_sexp y)
    | List [x; Atom "-"; y] -> sub  (t_of_sexp x) (t_of_sexp y)
    | List [x; Atom "+"; y] -> add  (t_of_sexp x) (t_of_sexp y)
    | List [x; Atom "*"; y] -> mult (t_of_sexp x) (t_of_sexp y)
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
  in
  sexp_of_t t

let rec atoms_aux t acc =
  match t with
  | `Base x -> x :: acc
  | `Abs x -> atoms_aux x acc
  | `Add (x, y) | `Sub (x, y) | `Mult (x, y) | `Div (x, y) | `Min (x, y) | `Max (x, y) ->
    atoms_aux x (atoms_aux y acc)

let atoms t = atoms_aux t []

module Eval (F : Ordered_field) = struct
  module F = struct
    include F
    include Comparable.Make (F)
  end

  let eval t ~f =
    let rec eval = function
      | `Base x      -> f x
      | `Add  (x, y) -> F.( + ) (eval x) (eval y)
      | `Sub  (x, y) -> F.( - ) (eval x) (eval y)
      | `Mult (x, y) -> F.( * ) (eval x) (eval y)
      | `Div  (x, y) -> F.( / ) (eval x) (eval y)
      | `Min  (x, y) -> F.min   (eval x) (eval y)
      | `Max  (x, y) -> F.max   (eval x) (eval y)
      | `Abs x ->
        let x = eval x in
        if F.(x >= zero) then x else F.(zero - x)
    in
    eval t
end

(* The Olang module tests both Flang and Olang. *)
