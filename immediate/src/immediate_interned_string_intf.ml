(** [Immediate_interned_string] represents strings as indices into a global intern table.

    Invariant: all strings are represented as non-negative integers. *)

open Core

module type S = sig
  include Immediate_intf.String_no_option
  include Immediate_intf.Intern_table

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving globalize, hash, stable_witness, typerep]

      include%template Stable_without_comparator [@mode local] with type t := t

      include Stringable.S with type t := t
    end
  end

  module Option : sig
    include Immediate_intf.String_option with type value := t

    module Stable : sig
      module V1 : sig
        type nonrec t = t [@@deriving hash]

        include%template Stable_without_comparator [@mode local] with type t := t

        module For_testing_only : sig
          val of_option : Stable.V1.t option -> t
          val to_option : t -> Stable.V1.t option
        end
      end
    end
  end

  val of_string_no_intern : local_ string -> Option.t
  val of_local_string : local_ string -> t
end

module type Immediate_interned_string = sig
  module type S = S

  (** The default universe is shared among many clients. The specific integer values of
      interned strings should not be considered stable or predictable. *)

  include S

  (** Creating a new universe mints a new interned string type with a new intern table.

      Within a given version (which determines things like preloaded strings), and
      assuming the same set of strings are interned in the same order, the integer values
      of interned strings are reliable, even across different executables.

      Any application relying on this should thoroughly test their universe construction,
      as this module gives no help in making sure universes are populated consistently. *)
  module Universe : sig
    module V1 : sig
      module Make () : S
    end
  end
end
