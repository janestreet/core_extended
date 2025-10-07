open! Core

module type Iobuf_accessors = sig @@ portable
  module For_cinaps : sig
    (** These functions read/write padding bytes for padded strings in {!Iobuf}s. Unlike
        the [tail_padded_fixed_string] accessors from [Iobuf], these do not use strings,
        and thus do not use heap allocation, except for optional arguments. *)

    val read_padding_and_get_unpadded_length
      :  padding:char
      -> pos:int
      -> padded_length:int
      -> local_ ([> read ], 'seek) Iobuf.t
      -> int

    val bigstring_read_padding_and_get_unpadded_length
      :  padding:char
      -> pos:int
      -> padded_length:int
      -> local_ Bigstring.t
      -> int

    val write_padding
      :  padding:char
      -> pos:int
      -> unpadded_length:int
      -> padded_length:int
      -> local_ (read_write, 'seek) Iobuf.t
      -> unit

    val bigstring_write_padding
      :  padding:char
      -> pos:int
      -> unpadded_length:int
      -> padded_length:int
      -> local_ Bigstring.t
      -> unit

    (** These functions are bounds-checked wrappers for (potentially) unsafe [Iobuf]
        functions. They do a number of things that just using safe [Iobuf] accessors
        can't:

        - compute [pos:int] and [len:int] from [pos:int option] and [len:int option]
        - check length of values being written other than just strings
        - check bounds once, even when the given function makes multiple (unsafe)
          reads/writes

        Doing this consistently, when wrapped around unsafe reads/writes, is really
        touchy. It seemed better to write it once and reuse it, rather than rewrite it in
        each function. *)

    val checked_read_with_pos_and_len
      :  ?pos:local_ int
      -> ?len:local_ int
      -> local_ ('rw, 'seek) Iobuf.t
      -> local_ (pos:int -> len:int -> local_ ('rw, 'seek) Iobuf.t -> 'a)
      -> string
      -> 'a

    val checked_read_with_len
      :  ?len:local_ int
      -> local_ ('rw, 'seek) Iobuf.t
      -> local_ (len:int -> local_ ('rw, 'seek) Iobuf.t -> 'a)
      -> string
      -> 'a

    val checked_write_with_pos_and_len
      :  'a
      -> length:('a -> int)
      -> ?pos:local_ int
      -> ?len:local_ int
      -> local_ ('rw, 'seek) Iobuf.t
      -> local_ ('a -> pos:int -> len:int -> local_ ('rw, 'seek) Iobuf.t -> unit)
      -> string
      -> unit

    val checked_write_with_len
      :  'a
      -> length:('a -> int)
      -> ?len:local_ int
      -> local_ ('rw, 'seek) Iobuf.t
      -> local_ ('a -> len:int -> local_ ('rw, 'seek) Iobuf.t -> unit)
      -> string
      -> unit

    val checked_write_with_pos
      :  'a
      -> length:('a -> int)
      -> ?pos:local_ int
      -> local_ ('rw, 'seek) Iobuf.t
      -> local_ ('a -> pos:int -> local_ ('rw, 'seek) Iobuf.t -> unit)
      -> string
      -> unit

    val checked_write
      :  'a
      -> length:('a -> int)
      -> local_ ('rw, 'seek) Iobuf.t
      -> local_ ('a -> local_ ('rw, 'seek) Iobuf.t -> unit)
      -> string
      -> unit
  end
end
