open Core

module type To_string = sig
  type t

  val to_string : t -> string
end

module type M = sig
  (** Used to describe a way to create a single row of a CSV from a given type. *)
  type -'a t

  module type S = sig
    type -'a delimited_writer := 'a t
    type t

    val delimited_writer : t delimited_writer
  end

  val empty : 'a t
  val column : ('a -> string) -> header:string -> 'a t
  val column_m : (module To_string with type t = 'a) -> header:string -> 'a t

  (** [default] is printed in place of [None], and if not supplied is the empty string. *)
  val column_opt : ?default:string -> ('a -> string) -> header:string -> 'a option t

  val column_m_opt
    :  ?default:string
    -> (module To_string with type t = 'a)
    -> header:string
    -> 'a option t

  (** [default] is used for every column in the case of [None]. *)
  val optional : ?default:string -> 'a t -> 'a option t

  val of_list : 'a t list -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val contra_map : 'b t -> f:('a -> 'b) -> 'a t
  val map_headers : 'a t -> f:(string -> string) -> 'a t
  val headers : 'a t -> string list
  val to_columns : 'a t -> 'a -> string list
  val header_line : ?quote:char -> ?sep:char -> 'a t -> string
  val line : ?quote:char -> ?sep:char -> 'a t -> 'a -> string

  (** Convert a list of ['a] to a CSV document in a string. *)
  val to_string
    :  ?quote:char
    -> ?sep:char
    -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
    -> write_header:bool
    -> 'a t
    -> 'a list
    -> string

  (** Open for prefix operators useful for using with Fields.to_list.

      e.g.
      {[
        let csv =
          Delimited.Write.Fields_O.(
            Fields.to_list ~a_string:!!Fn.id ~a_date:!!Date.to_string ~mv:!>Long_short.csv)
          |> Delimited.Write.of_list
        ;;
      ]} *)
  module Fields_O : sig
    (** Create a single column from a field of a record. *)
    val ( !! ) : ('a -> string) -> ('b, 'a) Field.t -> 'b t

    (** Nest a builder in a field of a record.

        Column headers will be prefixed with the name of the field. *)
    val ( !> ) : 'a t -> ('b, 'a) Field.t -> 'b t
  end

  module O : sig
    val ( <<| ) : 'b t -> ('a -> 'b) -> 'a t
    val ( <> ) : 'a t -> 'a t -> 'a t
  end

  module By_row : sig
    type row = string list

    (** Prints a valid csv file to a given channel. The [line_breaks] arg can be used to
        override the default line ending of "\r\n" (DOS/Windows line endings). Example
        ~line_breaks:`Unix to get *nix line endings *)
    val output_lines
      :  ?quote:char
      -> ?sep:char
      -> ?line_breaks:[ `Windows | `Unix ]
      -> Out_channel.t
      -> row list
      -> unit

    (** Convert one CSV line to a string. *)
    val line_to_string : ?quote:char -> ?sep:char -> row -> string

    module Out_channel : sig
      type t

      val create
        :  ?quote:char
        -> ?sep:char
        -> ?line_breaks:[ `Windows | `Unix ]
        -> Out_channel.t
        -> t

      val output_line : t -> string list -> unit
    end
  end

  module Expert : sig
    (** Escape the a CSV field if need be. *)
    val maybe_escape_field : ?quote:char -> ?sep:char -> string -> string

    (** Escape a CSV (even if doesn't have any characters that require escaping). *)
    val escape_field : ?quote:char -> string -> string

    (** Get the escaped length of one quoted field (without the quotes). Returns None if
        the field doesn't need to be escaped. *)
    val quote_len : quote:char -> sep:char -> pos:int -> len:int -> string -> int option

    (** Copy and escapes the content of a field over from one string to another. This does
        not put the quotes in. *)
    val quote_blit
      :  quote:char
      -> src:string
      -> dst:Bytes.t
      -> src_pos:int
      -> dst_pos:int
      -> len:int
      -> int
  end

  (** Wraps [Stdio.Out_channel] for writing CSVs one line at a time. *)
  module Out_channel : sig
    type 'a write = 'a t
    type 'a t

    val create
      :  ?quote:char
      -> ?sep:char
      -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
      -> write_header:bool
      -> 'a write
      -> Out_channel.t
      -> 'a t

    val channel : 'a t -> Out_channel.t
    val output_row : 'a t -> 'a -> unit
  end

  val save
    :  ?quote:char
    -> ?sep:char
    -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
    -> write_header:bool
    -> 'a t
    -> Filename.t
    -> 'a list
    -> unit
end
