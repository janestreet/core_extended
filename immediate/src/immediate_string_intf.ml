(** [Immediate_string] represents strings using [Immediate_short_string] if they are short
    enough, or using [Immediate_interned_string] otherwise.  This guarantees that short
    strings will not be stored in the interned table and longer strings will.  However,
    there is no guarantee about the particular mapping between strings and integers used
    in this module. *)

open! Core

(** Use fast, non-lexicographic [compare] by default.  This is not stable (as {!Stable});
    for a stable one, use {!Stable.V1.compare} or {!Lexicographic.compare}. *)
module type S = sig
  include Immediate_intf.String_no_option
  include Immediate_intf.Intern_table
  module Interned : Immediate_interned_string.S

  val is_interned : t -> bool
  val of_short_string : Immediate_short_string.t -> t
  val of_interned_string : Interned.t -> t
  val to_short_string_exn : t -> Immediate_short_string.t
  val to_interned_string_exn : t -> Interned.t

  module Stable : sig
    module V1 : sig
      type t [@@deriving typerep]

      include Stable_without_comparator with type t := t

      val compare : [ `removed_because_not_antisymmetric ]

      module For_testing_only : Stringable with type t := t
    end

    module V2 : sig
      (* We do /not/ declare the typical equivalance between [Immediate_string.Map] and
         [Immediate_string.Stable.V2.Map] because they're actually different: this order
         is stable (lexographic) and the non-stable one is fast but arbitrary.

         As a result, [Immediate_string.Stable.V2.Map] is slower than
         [Immediate_string.Map]; convert via [Immediate_string.Map.to_alist
         |> Map.of_alist_exn (module Immediate_string.Stable.V2)] or similar. *)

      type nonrec t = t [@@deriving hash, typerep]

      include
        Stable_comparable.With_stable_witness.V1
        with type t := t
         and type comparator_witness = Lexicographic.comparator_witness

      include Sexpable.S_with_grammar with type t := t

      val of_v1 : V1.t -> t

      module For_testing_only : Stringable with type t := t
    end
  end

  module Option : sig
    include Immediate_intf.String_option with type value := t

    module Stable : sig
      (* The original V1.compare function could differ between heap images, in particular
         depending on the order in which long strings are interned. *)
      module V1 : sig
        include Stable_without_comparator

        val compare : [ `removed_because_unstable ]

        module For_testing_only : sig
          val of_option : Stable.V1.t option -> t
          val to_option : t -> Stable.V1.t option
        end
      end

      module V2 : sig
        type nonrec t = t [@@deriving hash]

        include Stable_without_comparator_with_witness with type t := t

        val of_v1 : V1.t -> t

        module For_testing_only : sig
          val of_option : Stable.V2.t option -> t
          val to_option : t -> Stable.V2.t option
        end
      end
    end

    module For_testing_only : sig
      val representation : t -> int
    end

    (* For [Immediate_identifiable.S] *)
    val to_immediate_string_option : t -> t
    val of_immediate_string_option : t -> t
  end

  val of_local_string : string -> t
  val to_local_string : t -> string
  val of_string_no_intern : string -> Option.t

  (* This is [Immediate_stringable.S], but written out to avoid a dependency loop. *)
  val of_immediate_string : t -> t
  val to_immediate_string : t -> t
end

module type Immediate_string = sig
  module type S = S

  (** The default immediate string universe shares the default [Immediate_interned_string]
      universe. *)

  include S with module Interned = Immediate_interned_string

  (** Creating a new universe mints a new table of interned strings. See documentation on
      [Immediate_interned_string.Universe]. *)
  module Universe : sig
    module V1 : sig
      module Make () : S
    end
  end
end
