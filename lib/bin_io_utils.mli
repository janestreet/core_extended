open Core.Std

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
  type 'a t with bin_io

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
    type t with bin_io

    val create : B.t -> t
    val value : t -> B.t

    val bin_reader_t_with_value : (t * B.t) Bin_prot.Type_class.reader
    val bin_t_with_value : (t * B.t) Bin_prot.Type_class.t
  end
end

module Wrapped : sig
  (* A ['a Wrapped.t] is a type that is type-equivalent to ['a], but has different
     bin-prot serializers that prefix the representation with the length.

     The length is encoded as a 64 bit int. We could also use bin_io's variable length int
     encoding, but using 64 bit ints makes us compatible with the length-prefixed
     encodings preferred elsewhere (in e.g., Bin_prot.Utils' bin_dump and bin_read_stream,
     Core.Std.Unpack_buffer's unpack_bin_prot, and Reader/Writer's bin_prot functions).

     To understand where this is useful, imagine we have an event type where many
     applications look at with some parts of an event, but not all applications need to
     deal with all parts of an event. We might define:

       type 'a event =
         { time : Time.t
         ; source : string
         ; details : 'a
         }

       Applications that needs to understand all the details of an event could use:

         type concrete_event = Details.t Wrapped.t event

       An application that filters events to downsteam consumers based on just [source] or
       [time] (but doesn't need to parse [details]) may use:

         type opaque_event = Wrapped.Opaque.t event

       This has two advantages:
        - (de)serializing messages is faster because potentially costly (de)serialization
          of [details] is avoided
        - the application can be compiled without any knowledge of any conrete [Details.t]
          type, so it's robust to changes in [Details.t]

       An application that's happy to throw away [details] may use:

         type ignored_event = Wrapped.Ignored.t event

       Whereas [opaque_event]s roundtrip, [ignored_event]s actually drop the bytes
       representing [details] when deserializing, and therefore do not roundtrip. *)
  type 'a t = 'a with bin_io

  (* An [Opaque.t] is an arbitrary piece of bin-prot. The bin-prot (de-)serializers simply
     read/write the data, prefixed with its length.

     When reading bin-prot data, sometimes you won't care about deserializing a particular
     piece: perhaps you want to operate on a bin-prot stream, transforming some bits of
     the stream and passing the others through untouched. In these cases you can
     deserialize using the bin-prot converters for a type involving [Opaque.t]. This is
     anologuous to reading a sexp file / operating on a sexp stream and using
     (de-)serialization functions for a type involving [Sexp.t].
 *)
  module Opaque : sig
    type t with bin_io
  end

  (* An [Ignored.t] is a unit with special bin-prot converters. The reader reads the
     length and drops that much data from the buffer. Writing is not supported, however
     the size of [t] is kept, so [bin_size_t] does work.

     This can be used in similar situations to [Opaque.t]. If instead of transforming a
     bin-prot stream, you are simply consuming it (and not passing it on anywhere), there
     is no need to remember the bin-prot representation for the bits you're ignoring. E.g.
     if you wish to extract a subset of information from a bin-prot file, which contains
     the serialized representation of some type T (or a bunch of Ts in a row, or something
     similar), you can define a type which is similar to T but has various components
     replaced with [Ignored.t].
  *)
  module Ignored : sig
    type t with bin_io
  end

  val to_opaque : 'a t -> 'a Bin_prot.Type_class.writer -> Opaque.t

  val of_opaque_exn : Opaque.t -> 'a Bin_prot.Type_class.reader -> 'a
end
