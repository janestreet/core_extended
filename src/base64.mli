(** Simple library for encoding and decoding base64 strings *)
open! Core.Std

module type T = sig
  val encode : string -> string
  val decode : string -> string
end

include T

(** Web/file safe encoding, replacing '+' and '/' with '-' and '_' respectively.
    See Table 2 in RFC4648 (http://www.ietf.org/rfc/rfc4648.txt) for the full character
    set *)
module Websafe : T

(** Create other Base64 derivatives *)
module Make(D : sig
    (** Usually [ '+' ] *)
    val char62 : char
    (** Usually [ '/' ] *)
    val char63 : char
    (** Usually [ `Suggested '=' ]. *)
    val pad_char :
      (** Padding is not included when encoding. Padding errors are suppress when
          decoding. *)
      [ `None
      (** Padding is included when encoding. Padding errors are suppressed when
          decoding. *)
      | `Suggested of char
      (** Padding is included when encoding. Padding errors are raised when decoding. *)
      | `Required of char ]
    (** Usually [ Char.is_whitespace ] *)
    val ignore_char : char -> bool
  end) : T
