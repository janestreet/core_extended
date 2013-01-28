open Core.Std

(* Implements types to be used in selection languages using Blang.
   The many nested types serve partially as documentation, but mostly
   to ease the creation of custom sexp parsers to reduce the amount
   of noise in config files.  While any amount of magic may be embedded
   in the sexp parsers exposed below, the following magic will be available:
   - constructors that take lists can be written as atoms for singletons
   - specific notes as detailed below

   There is an extension to this exposed in Sqml that has an additional to_expr method for
   converting these to SQL expressions.  It also has additional selectors for Sqml
   specific types.
*)

module type Selector = sig
  type selector
  type value

  val eval : selector -> value -> bool
end

module Date_selector : sig
  (* >, <, and = are allowed in place of GT, LT, and On.  In addition >< can be used
      in place of Between and can be used infix (e.g. (date1 >< date2)).  In addition,
      the special cases of on and between can be written as simply "date" and
      "(date1 date2)"
  *)
  type t =
    | GT of Date.t
    | LT of Date.t
    | Between of Date.t * Date.t
    | On of Date.t
    with of_sexp

  include Selector with type selector = t and type value = Date.t
end

(* regular expressions must be bounded with a '/' on both ends and this is used to
    automagically produce the correct type when parsing sexps, so that you can write any
    of the following:
      /.*foo/
      foo
      (foo bar)
      (foo /bar[0-9]/)
*)
module String_selector : sig
  module Regexp : sig
    type t with sexp

    val of_regexp : string -> t
    val matches : t -> string -> bool
    val to_string : t -> string
    val to_regexp : t -> Pcre.regexp
  end

  type t =
    | Equal of string list
    | Matches of Regexp.t list
    | Mixed of [ `Regexp of Regexp.t | `Literal of string ] list
    with of_sexp

  include Selector with type selector = t and type value = String.t
end

module String_list_selector : sig
  type t = string list with sexp

  include Selector with type selector = t and type value = string
end
