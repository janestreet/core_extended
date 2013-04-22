(** two dimensional blocks of text *)
type t

(* the empty block. a left and right unit to both [hcat] and [vcat] *)
val nil : t

(* [fill] and [space] assume width and height are non-negative *)
val fill  : char -> width:int -> height:int -> t
val space :         width:int -> height:int -> t

(* vertical and horizontal alignment specifications *)
type valign = [`Top | `Bottom | `Center]
type halign = [`Left | `Right | `Center]

(* a basic block of text, split on newlines and horizontally aligned as specified *)
val text : ?align:halign -> string -> t

(* vertical and horizontal concatenation with alignment *)
val vcat : ?align:halign -> ?sep:t -> t list -> t
val hcat : ?align:valign -> ?sep:t -> t list -> t

(* text block dimensions *)
val width  : t -> int
val height : t -> int

(* vertical and horizontal sequence alignment *)
val valign : valign -> t list -> t list
val halign : halign -> t list -> t list

(* empty blocks with either horizontal or vertical extent -- useful for specifing a
   minimum width or height in conjunction with valign or halign, respectively *)
val hstrut : int -> t
val vstrut : int -> t

(* render a block of text as a string *)
val render : t -> string

(* compress table header according to column widths.
  Input:  a list of columns of the form (title, values, column alignment).
  Output: one header block and row sequence.
  Raises: if the [values] lists are not the same length in each column.
  Example:

                                               first name
    age  first name  last name            age  |     last name
    |    |           |            ==>     |    |     |
    30   sue         smith                30   sue   smith
    18   bill        rodriguez            18   bill  rodriguez
    76   rick        jones                76   rick  jones
*)
val compress_table_header :
  [`Cols of (t * t list * halign) list] -> [`Header of t] * [`Rows of t list]

