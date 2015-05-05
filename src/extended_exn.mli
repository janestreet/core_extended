(** Extensions to [Core.Exn].*)

open Core.Std


(** The [to_string] function is slightly tweaked to avoid escaping the string
content of [Failure]. *)
val to_string : exn -> string

(** This is also an ever so slight variation of [to_string] target more at user
than developers ([Failure s] is just printed as [s])
*)
val to_string_hum : exn -> string

(** [unwrap e]

    Tries to unwrap an exception to find the original cause of the error
    (Finally for instance has the propency to burry exception...). This is
    useful when matching on exceptions.
*)
val unwrap : exn -> exn



(** The point of this module is to be able to include an exn in a type that has to be
    sexpable or binable.  The [Exn_string.t] type is more descriptive than just converting
    to a string and is guaranteed to have come from an exn (unless someone abuses the
    [t_of_sexp] function or something).
*)
module Exn_string : sig
  type t = private string
  include Sexpable with type t := t
  include Stringable with type t := t
  include Binable with type t := t

  val of_exn : exn -> t
end
