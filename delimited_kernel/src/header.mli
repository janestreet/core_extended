open Core_kernel

(** Control how headers are parsed

    - [`No]: File has no header; columns accessed by index.
    - [`Yes]: File has a header.
    - [`Limit]: Assert that at least the given headers appear. The applicative
      parser will do this for any [at_header] specs included if [`Yes] is passed.
    - [`Replace]: ignore the header line, using the given headers instead
    - [`Add]: the file has no headers. Use the given ones for header access.
    - [`Transform]: headers will be transformed with the given function.
    - [`Filter_map]: similar to [`Transform] but [None] headers are ignored.
*)
type t =
  [ `No
  | `Yes
  | `Limit of string list
  | `Replace of string list
  | `Add of string list
  | `Transform of (string list -> string list) sexp_opaque
  | `Filter_map of (string list -> string option list) sexp_opaque ]
[@@deriving sexp_of]
