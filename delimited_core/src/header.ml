open! Core_kernel

type t =
  [ (* file has no header line, rows have no access by header *)
    `No
  | (* process header line as it appears *)
    `Yes
  | (* process header line as it appears, assert that it has at least the provided
       headers. Only the provided headers will be available by name to the processed
       rows. *)
    `Limit of string list
  |
    (* throw away header line, use provided headers *)
    `Replace of string list
  | (* file has no header line, use provided headers *)
    `Add of string list
  | (* the supplied transform function will be passed the headers as they are in the file
       and should return the headers it would like to use. *)
    `Transform of (string list -> string list) sexp_opaque
  | (* the supplied transform function will be passed the headers as they are in the file
       and should return the headers it would like to use, or [None] if it would not like to
       use a header. *)
    `Filter_map of (string list -> string option list) sexp_opaque ]
[@@deriving sexp_of]
