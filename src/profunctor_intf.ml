open Core.Std

(** A basic profunctor, which has an input end which can be
    mapped contravariantly, and an output end which is mapped
    covariantly.
*)
module type Basic = sig
  type ('a, 'b) t

  val map : ('u, 'a) t -> f:('a -> 'b) -> ('u, 'b) t
  val contra_map : ('b, 'u) t -> f:('a -> 'b) -> ('a, 'u) t
end

(** Strong profunctors can be joined together, keeping the same
    input and giving both outputs.
*)
module type Strong = sig
  include Basic

  val both : ('i, 'a) t -> ('i, 'b) t -> ('i, 'a * 'b) t
end

(** Modules of this type are used to traverse a record using a specific
    profunctor. They can be made using the functor
    [Profunctor.Record_builder].

    This can be used to build all sorts of things, e.g. building
    a catalog form for a record type, or building a creator function
    which uses an applicative or monad like deferred to create the
    content for each field, perhaps useful when combined with
    [Quickcheck.Generator].

    e.g. {[
      let form : (t, t) Form.t =
        let module B = Profunctor.Make_record_builder(Form) in
        let edit_string field = B.field (Form.edit_string ~label:(Field.name field)) in
        B.build_for_record (
          Fields.make_creator
            ~forename:edit_string
            ~surname:edit_string
            ~birthday:(B.field (Form.edit_date ~label:"birthday")))
    ]}
*)
module type Record_builder = sig
  type ('a, 'b) profunctor

  (** These types are exposed so the typechecker works, but
      you shouldn't ever have to think about them. If you want
      to know how they work then see the implementation.
  *)
  module Internal : sig
    type unfilled = [ `Unfilled ]
    type filled = [ `Filled ]
    type input
    type ('record, 'witness) accum
    type ('record, 'witness, 'step) fold_step =
      ('record, 'witness) accum -> (input -> 'step) * ('record, filled) accum
  end

  (** Perform the single step of the traversal.
      The type of this is designed to match up with [Fields.make_creator]
      (see the example).
  *)
  val field : ('a, 'a) profunctor
    -> ('record, 'a) Field.t
    -> ('record, _, 'a) Internal.fold_step

  (** Perform the overarching traversal.
      The type of this is designed to match up with [Fields.make_creator]
      (see the example).
  *)
  val build_for_record
    : ('record, Internal.unfilled, 'record) Internal.fold_step
    -> ('record, 'record) profunctor
end

(** A profunctor constructed from an applicative.

    In this case [contra_map] has no effect, and [map] and [both]
    behave exactly as they would with the underlying applicative.

    e.g. {[
      type t = {
        name : string;
        age : int;
      } [@@deriving fields]

      let gen : t Quickcheck.Generator.t =
        let module G = Quickcheck.Generator in
        let module G_pro = Profunctor.Of_applicative(G) in
        G_pro.Of_record.(build_for_record (
          Fields.make_creator
            ~name:(field String.gen)
            ~age:(field Int.gen)))
    ]}
*)
module type Of_applicative = sig
  type 'a applicative
  type ('a, 'b) t = 'b applicative

  include Strong with type ('a, 'b) t := ('a, 'b) t

  module Of_record : Record_builder
    with type ('a, 'b) profunctor := ('a, 'b) t
end
