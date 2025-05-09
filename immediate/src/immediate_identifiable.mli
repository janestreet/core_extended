open Core

(** Much as [Core.Identifiable.S] models string-like things without exposing unwanted
    [String.t] type equalities, this models the same for [Immediate.String]. *)
module type S_not_binable = sig
  type t [@@deriving hash, sexp] [@@immediate]

  include Identifiable.S_not_binable with type t := t
  include Immediate_stringable.S with type t := t

  module Option : sig
    include Immediate_intf.String_option with type value := t

    val of_immediate_string_option : Immediate_string.Option.t -> t
    val to_immediate_string_option : t -> Immediate_string.Option.t
  end
end
