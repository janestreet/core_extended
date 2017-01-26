open Core

module type S_eval = sig

  (*
     [S_eval] defines a generic interface for instantiations of
     [subst], [map], [specialize], and [eval] to different set types and monads.
     See [Set_lang] for documentation of [subst], [map], [specialize], and [eval].
  *)

  type 'base t
  type set
  type 'a result

  val subst      : 'b1 t -> f:('b1 -> 'b2 t result)     -> 'b2 t result
  val map        : 'b1 t -> f:('b1 -> 'b2 result)       -> 'b2 t result
  val specialize : 'b t  -> f:('b -> set option result) -> 'b t result
  val eval       : 'b t  -> f:('b -> set result)        -> set result

end

(* The ['a value] type serves to instantiate [S_eval] for non-monadic use. *)
type 'a value = 'a

module type S_lang = sig

  (*
     [S_lang] defines a generic interface for instantiation of [Set_lang] operations
     to a known set type.

     These operations differ from [Set_lang] in that [union] accepts empty lists,
     and there is no need for separate [union_list] or [union_list_exn] operations.

     Otherwise, see [Set_lang] for documentation of individual operations.
  *)

  type set
  type 'base t

  val base   : 'base -> 'base t
  val set    : set -> _ t
  val inter2 : 'b t -> 'b t -> 'b t
  val union2 : 'b t -> 'b t -> 'b t
  val diff   : 'b t -> 'b t -> 'b t

  val union : 'b t list -> 'b t

  val inter : 'b t * 'b t list -> 'b t
  val inter_list : 'b t list -> 'b t Or_error.t
  val inter_list_exn : 'b t list -> 'b t

  val values : 'base t -> 'base list

  val constant_value : _ t -> set option

  (* [S] includes non-monadic [subst], [map], [specialize], and [eval]. *)
  include S_eval
    with type 'base t := 'base t
    with type set := set
    with type 'a result := 'a value

  (* [S] can also construct monadic [subst], [map], [specialize], and [eval]. *)
  module Make_monadic_eval (M : Monad.S) : S_eval
    with type 'base t := 'base t
    with type set := set
    with type 'a result := 'a M.t

end

module type S = sig

  (*
     [S] specializes [S_lang] to set types implementing Set.S.
     See [S_lang] for its contents.
     See [Set_lang] for documentation of individual operations.
  *)

  module Raw : sig type ('base, 'set) t end
  module Set : Set.S

  type 'base t = ('base, Set.t) Raw.t [@@deriving compare, sexp]

  include S_lang
    with type 'base t := 'base t
    with type set := Set.t

end

module type S_binable = sig

  (*
     [S_binable] specializes [S_lang] to set types implementing Set.S_binable.
     See [S_lang] for its contents.
     See [Set_lang] for documentation of individual operations.
  *)

  module Raw : sig type ('base, 'set) t end
  module Set : Set.S_binable

  type 'base t = ('base, Set.t) Raw.t [@@deriving compare, sexp, bin_io]

  include S_lang
    with type 'base t := 'base t
    with type set := Set.t

end
