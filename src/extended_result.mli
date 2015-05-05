open Core.Std
  (** Extension to the {Core.Result} *)

module Ok: Monad.S2 with type ('a,'err) t = ('a,'err) Result.t
module Error: Monad.S2 with type ('err,'a) t = ('a,'err) Result.t

module Exn : sig
  type 'a t = ('a, exn) Result.t

  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  include Monad.S with type 'a t := 'a t

  (** [ok t] returns [x] if [t = Ok x], or raises [e] if [t = Error e]. *)
  val ok : 'a t -> 'a
end
