open! Core.Std

open Profunctor_intf

open! Int.Replace_polymorphic_compare

module Record_builder(F : Strong) = struct
  module Internal = struct
    module Hlist_F = struct
      (** A profunctor returning an hlist. *)
      type ('a, 'b) t =
        | Nil : ('a, Hlist.nil) t
        | Cons : ('a, ('x, 'xs) Hlist.cons Hlist.t) F.t -> ('a, ('x, 'xs) Hlist.cons) t

      let cons (type a b)
            (left : (_, a) F.t) (right : (_, b) t)
        : (_, (a, b) Hlist.cons) t
        = match right with
        | Nil -> Cons (F.map left ~f:(fun x -> Hlist.cons x Hlist.empty))
        | Cons right -> Cons (F.both left right)
      ;;

      let unpack (built : (_, (_, _) Hlist.cons) t) =
        match built with | Cons x -> x
      ;;
    end

    type ('record, 'out, 'all_fields) accum =
      (('record, 'out) Hlist_F.t -> ('record, 'all_fields) Hlist_F.t)
      * ('all_fields, 'out) Hlist.Suffix_index.t

    type ('record, 'field, 'head, 'tail, 'all_fields) fold_step =
      ('record, 'head, 'all_fields) accum
      -> ('all_fields -> 'field) * ('record, 'tail, 'all_fields) accum

    type ('record, 'field, 'tail, 'all_fields) make_creator_one_field =
      ('record, 'field, ('field, 'tail) Hlist.cons, 'tail, 'all_fields) fold_step

    type ('record, 'x, 'xs) make_creator_all_fields =
      ('record, 'record, ('x, 'xs) Hlist.cons, Hlist.nil, ('x, 'xs) Hlist.cons) fold_step
  end

  let field profunctor field (build_hlist, suffix) =
    let build_hlist =
      let applicative = F.contra_map profunctor ~f:(Field.get field) in
      fun tail -> build_hlist (Internal.Hlist_F.cons applicative tail)
    and get_field =
      let index = Hlist.Element_index.(within ~suffix first_element) in
      fun hlist -> Hlist.nth hlist index
    and suffix = Hlist.Suffix_index.tail_of suffix
    in
    get_field, (build_hlist, suffix)
  ;;

  let build_for_record folding =
    let from_values, (build_up, _) =
      folding (Fn.id, Hlist.Suffix_index.whole_list)
    in
    let built = Internal.Hlist_F.(unpack (build_up Nil)) in
    F.map built ~f:from_values
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
