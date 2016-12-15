open! Core.Std

open Profunctor_intf

open! Int.Replace_polymorphic_compare

module Packing_description = struct
  type 'values t =
    | One_value : 'field Type_equal.Id.t -> 'field t
    | Tuple : 'others t * 'field Type_equal.Id.t -> ('others * 'field) t

  let rec get_value_exn : type packed a. packed t -> packed -> key:a Type_equal.Id.t -> a =
    fun descr packed ~key:sought_key -> match descr with
      | One_value key ->
        begin match Type_equal.Id.same_witness key sought_key with
        | Some Type_equal.T -> packed
        | None -> failwith "There is a bug in Core_extended.Profunctor.Make_builder"
        end
      | Tuple (inner_descr, key) ->
        begin match Type_equal.Id.same_witness key sought_key with
        | Some Type_equal.T -> snd packed
        | None -> get_value_exn inner_descr (fst packed) ~key:sought_key
        end
end

module Packed_values = struct
  type t = | Packed_values : 'packed Packing_description.t * 'packed -> t

  let get_value_exn (Packed_values (descr, packed)) ~key =
    Packing_description.get_value_exn descr packed ~key
end

module Record_builder(F : Strong) = struct
  module Internal = struct
    type unfilled = [ `Unfilled ]
    type filled = [ `Filled ]

    type input = Packed_values.t

    type ('record, 'witness) accum =
      | Unfilled : (_, unfilled) accum
      | Filled : 'fields Packing_description.t * ('record, 'fields) F.t -> ('record, filled) accum

    type ('record, 'witness, 'step) fold_step =
      ('record, 'witness) accum -> (input -> 'step) * ('record, filled) accum
  end
  open Internal

  let field (type fill_state) for_field field (acc : (_, fill_state) accum) =
    let key = Type_equal.Id.create ~name:(Field.name field) sexp_of_opaque
    and for_field = F.contra_map for_field ~f:(Field.get field)
    in
    let new_acc =
      match acc with
      | Unfilled -> Filled (One_value key, for_field)
      | Filled (packing, for_others) ->
        Filled (Tuple (packing, key), F.both for_others for_field)
    in
    Packed_values.get_value_exn ~key, new_acc
  ;;

  let build_for_record (builder : ('record, unfilled, 'record) fold_step) =
    match builder Unfilled with
    | from_packed, Filled (packing, for_record) ->
      F.map for_record ~f:(fun packed ->
        from_packed (Packed_values (packing, packed)))
  ;;
end

module Of_applicative (F : Applicative) = struct
  module T = struct
    type ('a, 'b) t = 'b F.t

    let contra_map x ~f:_ = x

    let map = F.map
    let both = F.both
  end
  include T
  module Of_record = Record_builder(T)
end
