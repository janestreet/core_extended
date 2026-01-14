open! Core

module type Option = sig
  type t : immediate
  [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

  include Immediate_option.S with type t := t
  include Identifiable.S with type t := t
end

module type Option_zero_alloc = sig
  include Option
  include Immediate_option.S_zero_alloc with type t := t
end

module type Option_int63 = sig
  type t : immediate64
  [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

  include Immediate_option.S_int63 with type t := t
  include Identifiable.S with type t := t
end

module type S_no_option = sig
  type t : immediate
  [@@deriving
    bin_io ~localize, compare ~localize, equal ~localize, globalize, hash, typerep]

  include Identifiable.S with type t := t
end

(** Obviously, [Char], [Bool], and [Int] are already immediate, but this module is a place
    to put the [Option] sub-module in the same style as other immediate data structures,
    and make the option sub-type identifiable, etc.

    Exposing [Option.t] as an immediate type is key to avoiding caml_modify calls. *)
module type Immediate_kernel = sig @@ portable
  module type Option = Option
  module type Option_zero_alloc = Option_zero_alloc
  module type Option_int63 = Option_int63
  module type S_no_option = S_no_option

  module Char : sig
    include S_no_option with type t = char

    module Option : sig
      include Option_zero_alloc with type value := t

      module Stable : sig
        module V1 : sig
          type nonrec t = t [@@deriving equal ~localize, globalize]

          include%template
            Stable_without_comparator_with_witness [@mode local] with type t := t
        end
      end
    end
  end

  module Bool : sig
    include S_no_option with type t = bool

    module Option : sig
      type outer := t
      type t : immediate [@@deriving quickcheck]

      include Option_zero_alloc with type value := outer and type t := t

      module Stable : sig
        module V1 : sig
          type nonrec t = t [@@deriving equal ~localize, globalize]

          include%template
            Stable_without_comparator_with_witness [@mode local] with type t := t

          val to_wire : t -> int
          val of_wire : int -> t
        end
      end
    end
  end

  module Int : sig
    include S_no_option with type t = int

    val type_immediacy : t Type_immediacy.Always.t

    module Option : sig
      type outer := t
      type t : immediate [@@deriving globalize, quickcheck, sexp ~stackify]

      include Option_zero_alloc with type value := outer and type t := t

      val of_or_null : outer or_null -> t [@@zero_alloc]
      val to_or_null : t -> outer or_null [@@zero_alloc]
      val unchecked_some : int -> t [@@zero_alloc]
      val type_immediacy : t Type_immediacy.Always.t

      module Stable : sig
        module V1 : sig
          type nonrec t = t [@@deriving equal ~localize, globalize, sexp_of ~stackify]

          include%template
            Stable_without_comparator_with_witness [@mode local] with type t := t

          include Typerep_lib.Typerepable.S with type t := t

          val of_int : int -> t [@@zero_alloc]
          val to_int : t -> int [@@zero_alloc]
        end
      end
    end
  end

  (** A functor to build an immediate [Option] submodule based on [Intable] conversions. *)
  module Of_intable : sig
    module type S = sig
      (** [to_int_exn] and [of_int_exn] are used to convert between [t] and immediate
          option values. [Int.min_value] is used to represent [none]; any [t]
          corresponding to [Int.min_value] will be unrepresentable as a [some] value. *)
      include Core.Intable

      (** The immediate option type respects the value type's sexp format. *)
      include Core.Sexpable with type t := t

      (** [bin_shape_uuid] is used to construct the [bin_shape_digest] for the resulting
          bin-io format. *)
      val bin_shape_uuid : Bin_shape.Uuid.t
    end

    module Option : sig
      module%template.portable Make (I : S) : sig
        include Option with type value := I.t

        module Stable : sig
          module V1 : sig
            include Stable_without_comparator_with_witness with type t = t

            val to_int : t -> int
            val of_int : int -> t
          end
        end
      end
    end
  end

  module Immediate_kernel_stable : sig
    module Char : sig
      module Option : sig
        module%template V1 :
          Stable_without_comparator [@mode local] with type t = Char.Option.t
      end
    end

    module Bool : sig
      module Option : sig
        module%template V1 :
          Stable_without_comparator_with_witness [@mode local] with type t = Bool.Option.t
      end
    end

    module Int : sig
      module Option : sig
        module%template V1 :
          Stable_without_comparator_with_witness [@mode local] with type t = Int.Option.t
      end
    end
  end
end
