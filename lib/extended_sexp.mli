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

(* This module solves the following problem:

   With the [Sexp_maybe.t] type you can define a type like

   type t =
   | Foo of (...some type...) Sexp_maybe.t
   ...

   and if you try to parse a sexp as a type that contains [t] somewhere, if it parses all
   the way up to [Foo <sexp>] but the <sexp> doesn't parse, the function will return an
   Error instead of raising an exception.

   This is good for managing version skew, except in one case: If a type

   type t_old =
   | Foo of a * b
   ...

   is upgraded to use Sexp_maybe:

   type t_new =
   | Foo of (a * b) Sexp_maybe.t
   ...

   then sexps made from [t_new] will not parse as [t_old], since the old sexp will be
   (Foo a b) while the new one will be (Foo (a b)).

   You use this module in the following way. Instantiate the functor:

   module Sexp_maybe2 = Make_sexp_maybe2 (struct let state = Random.State.make_self_init () end)

   Then define type t_new as:

   type t_new =
   | Foo of (a, b) Sexp_maybe2.t
   ...

   Then, when you've got a sexp [sexp] which should be parsed as either [t_new] or
   [t_old], use [Sexp_maybe2.final_pass] on it to convert it to either the new or old
   format.
*)
module Make_sexp_maybe2 (Random_state:sig val state:Random.State.t end) : sig
  type ('a,'b) t = ('a * 'b) Sexp_maybe.t
  include Sexpable.S2 with type ('a,'b) t := ('a,'b) t
  include Binable.S2 with type ('a,'b) t := ('a,'b) t

  val final_pass : Sexp.t -> use_sexp_maybe:bool -> Sexp.t
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
