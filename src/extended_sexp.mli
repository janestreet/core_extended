open! Core
(** Extensions to [Sexplib.Sexp].*)

val is_atom : Sexp.t -> bool
val is_list : Sexp.t -> bool

(** {3 Constructors } *)

val atom : string -> Sexp.t
val list : Sexp.t list -> Sexp.t

(**{3 Printing }*)
(**
   The ocaml pretty printer (used by sexplib) is a speed daemon  but is,
   sadly enough, produces wrong output (e.g it overflows in places where this
   could have avoided). This uses a printer from wadler's a prettier printer to
   output strings suited to human consumption.
*)
val to_string_hum' : Sexp.t -> string

val format : Sexp.t -> Pp.t

(**
   A more readable but less compact pretty printer than the one bundled by
   sexplib. This is going through a test period at which point it might
   make it in sexplib. It uses ocaml's pretty-printing library so it is both
   fast and broken.
*)
val pp_hum' : Format.formatter -> Sexp.t -> unit

(** Takes a string and returns the same string but commented according to
    sexp's syntax*)
val comment : string -> string

(** {3 Various} *)
module Diff : sig
  type t
  val print : ?oc:Out_channel.t -> t -> unit
  val to_buffer : t -> Buffer.t
  val to_string : t -> string
  val of_sexps : original:Sexp.t -> updated:Sexp.t -> t option
end

val print_diff : ?oc:Out_channel.t -> original:Sexp.t -> updated:Sexp.t -> unit -> unit

(** Returns a smaller sexp by replacing sections with "...".  Will try to show parts of the
   sexp "near" sub_sexp.

   Limiting size to length a string length is less efficient than a certain depth.  The
   meaning of a given depth is arbitrary except that more depth gives you a bigger sexp.  Try
   100 or so. *)
val summarize : Sexp.t -> sub_sexp:Sexp.t -> size:[ `depth of int | `string of int ] -> Sexp.t

(** {3 Transforming sexp parsers} *)

module Records_table : sig
  (* Given 2 types:
     type t1 = {
     symbol : string;
     contracts_open : int;
     }
     type t2 = {
     symbol : string;
     contracts_open : int;
     description : string;
     }

     let t1_of_sexp = t_of_sexp t1_of_sexp
     let t2_of_sexp = t_of_sexp t2_of_sexp

     Both functions will successfully parse a sexp of the form:

     ((symbol     contracts_open    description)
     (ABC           100            "...")
     (XYZ           350            "..."))
  *)
  type 'a t = 'a list

  include Sexpable.S1 with type 'a t := 'a t
end

(* This module is intended to be used when [T.t] is a record type and
   [T.explicit_sexp_option_fields] is a list of fields declared with [sexp_option].

   The sexp conversions are changed so that [None] and [Some "foo"] are written out as
   [()] and [("foo")] respectively, so that you can switch to regular options more easily.
*)
module Make_explicit_sexp_option (T: sig
  type t [@@deriving sexp]
  val explicit_sexp_option_fields : string list
end) : sig
  type t = T.t [@@deriving sexp]
end

val load_sexp_conv_exn_sample :
  ?strict:bool
  -> ?buf:Bytes.t
  -> ?on_non_existence:[`Exit | `Raise]
  -> ?name:string
  -> string
  -> sexp_of_t:('a -> Sexp.t)
  -> t_of_sexp:(Sexp.t -> 'a)
  -> sample:'a
  -> 'a

(* Sexp serializer for lists that supports wildcard expansion.
   This follows bash brace-expansion syntax as closely as possible except
   when it's really inconvenient or we can do better.

   In particular, there are two known deviations:
   In bash, complicated sets of integers are written with nested braces, e.g.
   {{1..3},4} expands to 1 2 3 4, but our in our syntax this would be {1..3, 4}.

   Bash is also picky about whitespace. We're not. So { 1 .. 2 } is the same as {1..2}.

   Supported expansions:

   {a,b,c} expands to one instance for each of a, b and c
   {a..c} expands to one instance for each character in the range a..c
   pat1pat2 expands pat1 and pat2 and concatenates all possible combinations
   {1..10} expands to an instance for each integer 1..10
   {1..10..2} expands to an instance for each integer 1..10, increasing by 2
   {1,3,5} expands to an instance for each integer 1, 3, 5

   All other strings expand to themselves.

   This is mostly meant to be used for types that have simple sexp serializers,
   like string and int, but can be used on more complicated types. The expansions
   disregard the structure of the sexp and are just a transformation on its text
   representation. This adds an unfortunate number of intermediate steps to the
   translation, but makes it easy to reason about the effects on a complicated
   sexp.

   Comprehension.t unifies with list, so you can use a Comprehension.t anywhere that you
   have a list and you want bash expansion/compression.
*)
module Comprehension : sig
  type 'a t = 'a list [@@deriving sexp]
end
