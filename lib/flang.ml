open Core.Std

module type Field = sig
  type t
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
] with sexp, bin_io

let base a   = `Base a
let add  x y = `Add  (x, y)
let sub  x y = `Sub  (x, y)
let mult x y = `Mult (x, y)
let div  x y = `Div  (x, y)

let t_of_sexp a_of_sexp sexp =
  let is_constructor lc s = String.equal lc (String.lowercase s) in
  let open Sexplib.Type in
  let rec t_of_sexp = function
    | List [Atom c; x; y] when is_constructor "div"  c -> `Div  (t_of_sexp x, t_of_sexp y)
    | List [Atom c; x; y] when is_constructor "add"  c -> `Add  (t_of_sexp x, t_of_sexp y)
    | List [Atom c; x; y] when is_constructor "sub"  c -> `Sub  (t_of_sexp x, t_of_sexp y)
    | List [Atom c; x; y] when is_constructor "mult" c -> `Mult (t_of_sexp x, t_of_sexp y)
    | List [x; Atom "/"; y] -> `Div  (t_of_sexp x, t_of_sexp y)
    | List [x; Atom "+"; y] -> `Add  (t_of_sexp x, t_of_sexp y)
    | List [x; Atom "-"; y] -> `Sub  (t_of_sexp x, t_of_sexp y)
    | List [x; Atom "*"; y] -> `Mult (t_of_sexp x, t_of_sexp y)
    | sexp -> `Base (a_of_sexp sexp)
  in
  t_of_sexp sexp

module Eval (F : Field) = struct
  let eval t ~f =
    let rec eval = function
      | `Base x      -> f x
      | `Add  (x, y) -> F.( + ) (eval x) (eval y)
      | `Sub  (x, y) -> F.( - ) (eval x) (eval y)
      | `Mult (x, y) -> F.( * ) (eval x) (eval y)
      | `Div  (x, y) -> F.( / ) (eval x) (eval y)
    in
    eval t
end

(* The Olang module tests both Flang and Olang. *)
