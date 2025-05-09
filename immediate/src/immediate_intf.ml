open Core

module type Option = Immediate_kernel.Option_zero_alloc
module type S_no_option = Immediate_kernel.S_no_option

module type String_no_option_without_zero_alloc_functions = sig
  include S_no_option
  include Quickcheckable with type t := t
  include Sexpable.S_with_grammar with type t := t

  (** [Lexicographic.compare x y = String.compare (to_string x) (to_string y)] *)
  module Lexicographic : Identifiable.S_not_binable with type t = t

  include Intable with type t := t

  val empty : t
  val unsafe_of_bigstring : pos:int -> len:int -> local_ Bigstring.t -> t

  val of_iobuf_peek
    :  ?pos:local_ int
    -> ?len:local_ int
    -> local_ ([> read ], _) Iobuf.t
    -> t

  val of_iobuf_consume : ?len:local_ int -> local_ ([> read ], Iobuf.seek) Iobuf.t -> t
  val to_iobuf_poke : t -> ?pos:local_ int -> local_ (read_write, _) Iobuf.t -> unit
  val to_iobuf_fill : t -> local_ (read_write, Iobuf.seek) Iobuf.t -> unit

  module Padded : sig
    (** WARNING: using the padded functions to convert to a bigstring / iobuf will ignore
        [~len] arguments that are less than the length of the source [t]! *)

    val of_iobuf_peek
      :  padding:char
      -> ?pos:local_ int
      -> ?len:local_ int
      -> local_ ([> read ], _) Iobuf.t
      -> t

    val of_iobuf_consume
      :  padding:char
      -> ?len:int
      -> local_ ([> read ], Iobuf.seek) Iobuf.t
      -> t

    val unsafe_of_bigstring
      :  padding:char
      -> pos:int
      -> len:int
      -> local_ Bigstring.t
      -> t

    val unsafe_to_bigstring
      :  t
      -> padding:char
      -> pos:int
      -> len:int
      -> local_ Bigstring.t
      -> unit

    val to_iobuf_poke
      :  t
      -> padding:char
      -> ?pos:int
      -> ?len:int
      -> local_ (read_write, _) Iobuf.t
      -> unit

    val to_iobuf_fill
      :  t
      -> padding:char
      -> ?len:int
      -> local_ (read_write, Iobuf.seek) Iobuf.t
      -> unit

    val unsafe_of_iobuf_peek
      :  padding:char
      -> pos:int
      -> len:int
      -> local_ ([> read ], _) Iobuf.t
      -> t

    val unsafe_of_iobuf_consume
      :  padding:char
      -> len:int
      -> local_ ([> read ], Iobuf.seek) Iobuf.t
      -> t

    val unsafe_to_iobuf_poke
      :  t
      -> padding:char
      -> pos:int
      -> len:int
      -> local_ (read_write, _) Iobuf.t
      -> unit

    val unsafe_to_iobuf_fill
      :  t
      -> padding:char
      -> len:int
      -> local_ (read_write, Iobuf.seek) Iobuf.t
      -> unit
  end

  val unsafe_get : t -> int -> char
  val unsafe_of_iobuf_peek : pos:int -> len:int -> local_ ([> read ], _) Iobuf.t -> t
  val unsafe_of_iobuf_consume : len:int -> local_ ([> read ], Iobuf.seek) Iobuf.t -> t
  val unsafe_to_iobuf_poke : t -> pos:int -> local_ (read_write, _) Iobuf.t -> unit
  val unsafe_to_iobuf_fill : t -> local_ (read_write, Iobuf.seek) Iobuf.t -> unit
  val unsafe_to_bigstring : t -> pos:int -> local_ Bigstring.t -> unit
end

module type String_no_option = sig
  include String_no_option_without_zero_alloc_functions

  val length : t -> int [@@zero_alloc]
  val get : t -> int -> char [@@zero_alloc]
  val of_char : char -> t [@@zero_alloc]
  val mem : t -> char -> bool [@@zero_alloc]
  val unsafe_to_int : t -> int [@@zero_alloc]
  val unsafe_of_int : int -> t [@@zero_alloc]
  val is_empty : t -> bool [@@zero_alloc]
end

module type String_no_option_not_zero_alloc = sig
  include String_no_option_without_zero_alloc_functions

  val length : t -> int
  val get : t -> int -> char
  val of_char : char -> t
  val mem : t -> char -> bool
  val unsafe_to_int : t -> int
  val unsafe_of_int : int -> t
  val is_empty : t -> bool
end

module type String_option = sig
  include Option

  val of_string_option : string option -> t
  val to_string_option : t -> string option
end

module type Intern_table = sig
  module Stats : sig
    (** Number of strings in the intern table. *)
    val interned_count : unit -> int

    (** Total length of all strings in the intern table. *)
    val interned_total_length : unit -> int
  end

  (** [grow_by n] ensures that the intern table can hold at least [n] additional entries
      without resizing (again). This is useful for applications which must not pause
      online for the (substantial) time required to rehash a large intern table. *)
  val grow_by : int -> unit

  (** [after_grow f] registers a growth logger. [f] will be called immediately after every
      growth of the intern table, with the time and size before and the size after.

      [f] would typically call {!Time_ns.now} to learn the time after. *)
  val after_grow : (before:Time_ns.t -> len_before:int -> len:int -> unit) -> unit
end
