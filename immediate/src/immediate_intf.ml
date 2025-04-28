open Core

module type Option = Immediate_kernel.Option_zero_alloc
module type S_no_option = Immediate_kernel.S_no_option

module type String_no_option = sig
  include S_no_option
  include Quickcheckable with type t := t
  include Sexpable.S_with_grammar with type t := t

  (** [Lexicographic.compare x y = String.compare (to_string x) (to_string y)] *)
  module Lexicographic : Identifiable.S_not_binable with type t = t

  val length : t -> int
  val get : t -> int -> char
  val of_char : char -> t
  val mem : t -> char -> bool

  include Intable with type t := t

  val unsafe_to_int : t -> int
  val unsafe_of_int : int -> t
  val empty : t
  val is_empty : t -> bool
  val unsafe_of_bigstring : pos:int -> len:int -> Bigstring.t -> t
  val of_iobuf_peek : ?pos:int -> ?len:int -> ([> read ], _) Iobuf.t -> t
  val of_iobuf_consume : ?len:int -> ([> read ], Iobuf.seek) Iobuf.t -> t
  val to_iobuf_poke : t -> ?pos:int -> (read_write, _) Iobuf.t -> unit
  val to_iobuf_fill : t -> (read_write, Iobuf.seek) Iobuf.t -> unit

  module Padded : sig
    (** WARNING: using the padded functions to convert to a bigstring / iobuf will ignore
        [~len] arguments that are less than the length of the source [t]! *)

    val of_iobuf_peek
      :  padding:char
      -> ?pos:int
      -> ?len:int
      -> ([> read ], _) Iobuf.t
      -> t

    val of_iobuf_consume
      :  padding:char
      -> ?len:int
      -> ([> read ], Iobuf.seek) Iobuf.t
      -> t

    val unsafe_of_bigstring : padding:char -> pos:int -> len:int -> Bigstring.t -> t

    val unsafe_to_bigstring
      :  t
      -> padding:char
      -> pos:int
      -> len:int
      -> Bigstring.t
      -> unit

    val to_iobuf_poke
      :  t
      -> padding:char
      -> ?pos:int
      -> ?len:int
      -> (read_write, _) Iobuf.t
      -> unit

    val to_iobuf_fill
      :  t
      -> padding:char
      -> ?len:int
      -> (read_write, Iobuf.seek) Iobuf.t
      -> unit

    val unsafe_of_iobuf_peek
      :  padding:char
      -> pos:int
      -> len:int
      -> ([> read ], _) Iobuf.t
      -> t

    val unsafe_of_iobuf_consume
      :  padding:char
      -> len:int
      -> ([> read ], Iobuf.seek) Iobuf.t
      -> t

    val unsafe_to_iobuf_poke
      :  t
      -> padding:char
      -> pos:int
      -> len:int
      -> (read_write, _) Iobuf.t
      -> unit

    val unsafe_to_iobuf_fill
      :  t
      -> padding:char
      -> len:int
      -> (read_write, Iobuf.seek) Iobuf.t
      -> unit
  end

  val unsafe_get : t -> int -> char
  val unsafe_of_iobuf_peek : pos:int -> len:int -> ([> read ], _) Iobuf.t -> t
  val unsafe_of_iobuf_consume : len:int -> ([> read ], Iobuf.seek) Iobuf.t -> t
  val unsafe_to_iobuf_poke : t -> pos:int -> (read_write, _) Iobuf.t -> unit
  val unsafe_to_iobuf_fill : t -> (read_write, Iobuf.seek) Iobuf.t -> unit
  val unsafe_to_bigstring : t -> pos:int -> Bigstring.t -> unit
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
