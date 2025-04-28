open Core

(** If we can read a row correctly but the ['a t] provided can't convert it, your
    ['a On_invalid_row.t] will define what happens.

    Here, 'read a row correctly' means that it's valid CSV (subject to your delimiter etc)
    & it has the correct number of columns. If that's not the case the parsers will just
    raise. *)
module type On_invalid_row = sig
  type 'a t

  (** The default. Raise with additional context. *)
  val raise : _ t

  (** Same as [raise] but includes the provided filename *)
  val raise_with_filename : filename:string -> _ t

  (** Skip the bad row *)
  val skip : _ t

  (** Do something else!

      - [`Skip]: skip the line. Same as [skip] above.
      - [`Yield]: return the given value for this row.
      - [`Raise]: raise the given exception
      - [`Fallback]: invoke the given handler instead *)
  val create
    :  (line_number:int (** The line number of the bad row. *)
        -> int String.Map.t (** Map from header to position. *)
        -> string Append_only_buffer.t (** Value at each position. *)
        -> exn (** Exception raised when trying to convert this row. *)
        -> [ `Skip | `Yield of 'a | `Raise of exn | `Fallback of 'a t ])
    -> 'a t
end

module type Open_on_rhs = sig
  type 'a t

  val at_index : int -> f:(string -> 'a) -> 'a t
  val at_header : string -> f:(string -> 'a) -> 'a t
  val at_header_opt : string -> f:(string option -> 'a) -> 'a t
  val label : 'a t -> Info.t -> 'a t
end

module type Root = sig
  (** Row up to the error, and the field with the error up to the point of failure. Same
      as [Expert.Parse_state.Bad_csv_formatting]. *)
  exception Bad_csv_formatting of string list * string

  (** This provides an applicative interface for constructing values from a csv file.

      An ['a t] describes how to build an OCaml model ['a] for each row.

      Simple example:
      {[
        type t =
          { foo : int
          ; bar : string
          }

        (* Describes how to generate a [t] from a row of a csv file *)
        let parse : t Delimited_kernel.Read.t =
          let open Delimited_kernel.Read.Let_syntax in
          let%map_open foo = at_header "foo" ~f:Int.of_string
          and bar = at_header "bar" ~f:String.of_string in
          { foo; bar }
        ;;

        let _ =
          Delimited_kernel.Read.list_of_string
            ~header:`Yes
            parse
            "foo,bar\n2,\"hello, world\"\n"
        ;;
      ]} *)
  type 'a t

  include Applicative.S with type 'a t := 'a t

  module Open_on_rhs_intf : sig
    module type S = Open_on_rhs with type 'a t := 'a t
  end

  include
    Applicative.Let_syntax
    with type 'a t := 'a t
    with module Open_on_rhs_intf := Open_on_rhs_intf

  (** Read a field at the given index. Use [f] to convert the field from string. *)
  val at_index : int -> f:(string -> 'a) -> 'a t

  (** Read a field at the given header. Use [f] to convert the field from string.

      Note that if the given header is not provided through either the file or the
      [~header] argument to the parsers, this will fail at runtime. *)
  val at_header : string -> f:(string -> 'a) -> 'a t

  (** Read a field at the given header, if it exists. Use [f] to convert the field from
      string. *)
  val at_header_opt : string -> f:(string option -> 'a) -> 'a t

  (** Provide a label to be used in exceptions raised by this builder *)
  val label : 'a t -> Info.t -> 'a t

  module Record_builder : Record_builder.S with type 'a applicative = 'a t

  module Fields_O : sig
    (** The following are convenience functions that build on [Record_builder.field] to
        make it easy to define a [t Delimited.Read.t] for some record type [t].

        Example usage:

        {[
          type t =
            { foo : int
            ; bar : bool
            }
          [@@deriving fields]

          let read : t Delimited.Read.t =
            Delimited.Read.Fields_O.(
              Fields.make_creator
                ~foo:!!Int.of_string
                ~bar:!?(Option.value_map ~default:true ~f:Bool.of_string)
              |> Delimited.Read.Record_builder.build_for_record)
          ;;
        ]} *)

    (** Reads a single column from a field of a record. *)
    val ( !! )
      :  (string -> 'a)
      -> (_, 'a) Field.t
      -> ('a, _, _, _) Record_builder.Make_creator_types.handle_one_field

    (** Reads a single column from a field of a record, if the header exists.

        [( !? )] is to [at_header_opt] as [( !! )] is to [at_header]. *)
    val ( !? )
      :  (string option -> 'a)
      -> (_, 'a) Field.t
      -> ('a, _, _, _) Record_builder.Make_creator_types.handle_one_field
  end

  module On_invalid_row : On_invalid_row

  (** Header parsing control *)
  module Header = Header

  (** Whole-row parsing. *)
  module Row : sig
    type 'a builder_t = 'a t

    include module type of Row

    (** A builder for [Row.t]s.

        As this parses the whole row it's slower than using the builder interface
        directly, but simpler to use. *)
    val builder : t builder_t
  end

  (** Fold the CSV rows contained in the given string. *)
  val fold_string
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[ `No_quoting | `Using of char ]
    -> ?header:Header.t
    -> ?on_invalid_row:'a On_invalid_row.t
    -> 'a t
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> string
    -> 'b

  (** Load the CSV as a list *)
  val list_of_string
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[ `No_quoting | `Using of char ]
    -> ?header:Header.t
    -> ?on_invalid_row:'a On_invalid_row.t
    -> 'a t
    -> string
    -> 'a list

  (** Read CSV file. *)
  val read_lines
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[ `No_quoting | `Using of char ]
    -> ?header:Header.t
    -> ?on_invalid_row:'a On_invalid_row.t
    -> 'a t
    -> In_channel.t
    -> 'a list

  (** Read CSV file, processing line by line. *)
  val fold_lines
    :  ?buffer_size:int
    -> ?strip:bool
    -> ?sep:char
    -> ?quote:[ `No_quoting | `Using of char ]
    -> ?header:Header.t
    -> ?on_invalid_row:'a On_invalid_row.t
    -> 'a t
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> In_channel.t
    -> 'b

  module Streaming : sig
    type 'a builder_t = 'a t
    type 'a t

    val create
      :  ?strip:bool
      -> ?sep:char
      -> ?quote:[ `No_quoting | `Using of char ]
      -> ?start_line_number:int
      -> ?on_invalid_row:'a On_invalid_row.t
      -> ?header:Header.t
      -> 'a builder_t
      -> init:'b
      -> f:('b -> 'a -> 'b)
      -> 'b t

    (** Like [create], but additionally passes the line number of the current row to [f] *)
    val create_indexed
      :  ?strip:bool
      -> ?sep:char
      -> ?quote:[ `No_quoting | `Using of char ]
      -> ?start_line_number:int
      -> ?on_invalid_row:'a On_invalid_row.t
      -> ?header:Header.t
      -> 'a builder_t
      -> init:'b
      -> f:(int -> 'b -> 'a -> 'b)
      -> 'b t

    val input_string : 'a t -> ?pos:int -> ?len:int -> string -> 'a t
    val input : 'a t -> ?pos:int -> ?len:int -> bytes -> 'a t
    val finish : 'a t -> 'a t
    val acc : 'a t -> 'a
    val headers : 'a t -> String.Set.t option

    (** Same as [headers], but preserves the order in which the headers were present in
        the file *)
    val list_of_headers : 'a t -> string list option

    val state : 'a t -> [ `Parsing_header | `Parsing_rows ]
  end
end

module type Expert = sig
  module Append_only_buffer = Append_only_buffer
  module Parse_state = Parse_state
  module On_invalid_row : On_invalid_row

  type 'a t

  module Builder : sig
    type nonrec 'a t = 'a t

    val lambda : (int String.Map.t -> string Append_only_buffer.t -> 'a) -> 'a t
    val return : 'a -> 'a t
  end
end
