open! Core

val load :
  ?pos : Int64.t -> ?len : Int64.t -> string -> 'a Bin_prot.Read.reader -> 'a

val save :
  ?header : bool -> ?perm : Unix.file_perm -> string ->
  'a Bin_prot.Type_class.writer -> 'a -> unit

(* converts the value to a string with a newline at the end and no other newlines *)
val to_line : 'a Bin_prot.Type_class.t -> 'a -> Bigstring.t
(* reads a string with no newlines (which must be the output of [to_line] without the
   trailing newline) and converts it to a value *)
val of_line : string -> 'a Bin_prot.Type_class.t -> 'a

module Serialized : sig
  (** A container for storing an ['a] as a bin-io buffer. The advantages of storing a
      value as an ['a t] rather than an ['a] are:
       - it will take up less space
       - serializing ['a t] is faster (it's just a blit) and it has the same bin-io
         representation as ['a].

      However, every time you need to access ['a] itself you need to call [value], which
      requires deserializing.

      ['a t] is safe in the sense that you cannot construct an ['a t] that doesn't store a
      valid ['a]. When deserializing an ['a], this requires actually constructing ['a]. If
      you'd like access to the ['a] that's constructed during deserialization, see the
      [bin_reader_t_with_value] below. *)
  type 'a t [@@deriving bin_io]

  val create : 'a Bin_prot.Type_class.writer -> 'a -> 'a t

  val value : 'a t -> 'a Bin_prot.Type_class.reader -> 'a

  (** Deserializing requires actually constructing an ['a], so this reader gives you
      access to that ['a], rather than just throwing it away. This is useful because you
      often might want to do something like indexing ['a t] by inspecting ['a] at the time
      that you read it. *)
  val bin_reader_t_with_value
    :  'a Bin_prot.Type_class.reader
    -> ('a t * 'a) Bin_prot.Type_class.reader

  val bin_t_with_value
    :  'a Bin_prot.Type_class.t
    -> ('a t * 'a) Bin_prot.Type_class.t


  module Make (B : Binable) : sig
    type t [@@deriving bin_io]

    val create : B.t -> t
    val value : t -> B.t

    val bin_reader_t_with_value : (t * B.t) Bin_prot.Type_class.reader
    val bin_t_with_value : (t * B.t) Bin_prot.Type_class.t
  end
end
