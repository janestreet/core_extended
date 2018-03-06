(** The language of terms over floats. *)

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
  (** sexp [(x + y)] or [(add x y z ...)]. An empty list ([(add)]) is not allowed. *)
  | `Sub  of 'a t * 'a t
  (** sexp [(x - y)] or [(sub x y)] *)
  | `Mult of 'a t * 'a t
  (** sexp [(x * y)] or [(mult x y z ...)]. An empty list ([(mult)]) is not allowed. *)
  | `Div  of 'a t * 'a t
  (** sexp [(x / y)] or [(div x y)] *)
  | `Abs of 'a t
  | `Min of 'a t * 'a t
  (** sexp [(min x y z ...)]. An empty list ([(min)]) is not allowed. *)
  | `Max of 'a t * 'a t
  (** sexp [(max x y z ...)]. An empty list ([(max)]) is not allowed. *)
  | `Exp of 'a t
  | `Ln  of 'a t
] [@@deriving sexp, bin_io, compare]

val base : 'a -> 'a t
val add  : 'a t -> 'a t -> 'a t
val sub  : 'a t -> 'a t -> 'a t
val mult : 'a t -> 'a t -> 'a t
val div  : 'a t -> 'a t -> 'a t
val add_list  : 'a t list -> 'a t
val mult_list : 'a t list -> 'a t
val abs : 'a t -> 'a t
val min : 'a t -> 'a t -> 'a t
val max : 'a t -> 'a t -> 'a t
val exp : 'a t -> 'a t
val ln  : 'a t -> 'a t

val atoms : 'a t -> 'a list

module Eval (F : Ordered_field_with_exponential) : sig
  val eval : 'a t -> f:('a -> F.t) -> F.t
end
