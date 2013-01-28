open Core.Std
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
  val print : ?oc:out_channel -> t -> unit
  val of_sexps : Sexp.t -> Sexp.t -> t option
end

val print_diff : ?oc:out_channel -> Sexp.t -> Sexp.t -> unit

(** Returns a smaller sexp by replacing sections with "...".  Will try to show parts of the
   sexp "near" sub_sexp.

   Limiting size to length a string length is less efficient than a certain depth.  The
   meaning of a given depth is arbitrary except that more depth gives you a bigger sexp.  Try
   100 or so. *)
val summarize : Sexp.t -> sub_sexp:Sexp.t -> size:[ `depth of int | `string of int ] -> Sexp.t

(** [of_sexp_allow_extra_fields of_sexp sexp] uses [of_sexp] to convert [sexp] to a
    value, but will not fail if there any extra fields in a record.  *)
val of_sexp_allow_extra_fields : (Sexp.t -> 'a) -> Sexp.t -> 'a

(** {3 Transforming sexp parsers} *)

(* [filter_record t_of_sexp field_names] will return a new [t_of_sexp] function
   which will take sexps representing records and ignore all fields not
   mentioned in [field_names] (which can be given by [Fields.names]).

   The motivation for this is to let config files (based on a record type) to
   gain new fields and still work with old code. The old code will ignore the
   new fields and still be able to parse the old config type from the new sexp.
*)
val filter_record : (Sexp.t -> 'a) -> string list -> (Sexp.t -> 'a)

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
  type t with sexp
  val explicit_sexp_option_fields : string list
end) : sig
  type t = T.t with sexp
end

val load_sexp_conv_exn_sample :
  ?strict:bool
  -> ?buf:string
  -> ?on_non_existence:[`Exit | `Raise]
  -> ?name:string
  -> string
  -> sexp_of_t:('a -> Sexp.t)
  -> t_of_sexp:(Sexp.t -> 'a)
  -> sample:'a
  -> 'a
