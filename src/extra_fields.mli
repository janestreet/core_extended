open Core.Std

exception Not_a_record_type of Sexp.t with sexp

(* The create [t_of_sexp] function will record whichever extra fields were passed in. *)
module Make (M:Sexpable) : sig
  type t = {
    value : M.t;
    extra_fields : string list
  }
  include Sexpable with type t := t
end
