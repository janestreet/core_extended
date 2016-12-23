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

    Is equivalent to: {[
      let form : (t, t) Form.t =
        let for_field field x = Form.contra_map x ~f:(Fields.get field) in
        let edit_string field = for_field field (Form.edit_string ~label:(Field.name field)) in
        Form.map
          (Form.both (edit_string Field.forename) (Form.both (edit_string Field.surname)
                                                     (for_field Field.birthday (Form.edit_date ~label:"birthday"))))
          ~f:(fun (forename, (surname, birthday)) -> { forename; surname; birthday; })
    ]}
*)
module type Record_builder = sig
  type ('a, 'b) profunctor

  (** These types have to be exposed so that the typechecker
      can tell that the use of [Fields.make_creator] is well-typed.

      However, you shouldn't really have to think about them.
  *)
  module Internal : sig
    (** An internal state which is folded through the fields. *)
    type ('record, 'out, 'all_fields) accum

    (** Each part of the fold has a type of this form. *)
    type ('record, 'field, 'head, 'tail, 'all_fields) fold_step =
      ('record, 'head, 'all_fields) accum
      -> ('all_fields -> 'field) * ('record, 'tail, 'all_fields) accum

    (** A step of the fold over a single field has this type.

        Each argument to [Fields.make_creator] should take that field
        as an argument and return something of this type (see [field] below).
    *)
    type ('record, 'field, 'tail, 'all_fields) make_creator_one_field =
      ('record, 'field, ('field, 'tail) Hlist.cons, 'tail, 'all_fields) fold_step

    (** The overall fold of multiple steps created by applying
        [Fields.make_creator] without an initial value should have a type
        of this form. You then supply it as an argument to [build_for_record] below.

        ['x] and ['xs] are types of the parts of the [('x, 'xs) Hlist.cons Hlist.t] which
        contains the values of all the fields of the record type.
    *)
    type ('record, 'x, 'xs) make_creator_all_fields =
      ('record, 'record, ('x, 'xs) Hlist.cons, Hlist.nil, ('x, 'xs) Hlist.cons) fold_step
  end

  (** Supply the term for one field.

      The input end of this profunctor will be mapped to consume that field from
      an input record, and the output end will be mapped and used with the output
      ends of the other fields' profunctors to produce the record type.

      The type of this is designed to match up with [Fields.make_creator]
      (see the example).
  *)
  val field : ('field, 'field) profunctor
    -> ('record, 'field) Field.t
    -> ('record, 'field, _, _) Internal.make_creator_one_field

  (** Build the overarching profunctor for the whole record.

      This takes a partial application of [Fields.make_creator] as its argument,
      which supplies the profunctors to use for each field of the record. It
      performs the needed mapping of the input and output ends.

      The type of this is designed to match up with [Fields.make_creator]
      (see the example).
  *)
  val build_for_record
    : ('record, _, _) Internal.make_creator_all_fields
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
