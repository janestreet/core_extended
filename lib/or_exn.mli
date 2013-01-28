(**
   Specialization of the Result.t, but less contraint than Or_error.
   This is mostly useful if you plan on matching the exception and do something with it.
   With Or_error, you'll have to unpack the exception, and get to a different exn (Error
   of t)
*)
open Core.Std

type 'a t = ('a, Exn.t) Result.t

include Monad   .S  with type 'a t := 'a t

(**
   Various use cases:
   {[
   Or_exn.try_with ~f x
   Or_exn.try_with () ~f:(fun () ->
   ....
   )
   let f_or_exn = Or_exn.try_with ~f
   ]}
*)
val try_with      : f:('arg -> 'res) -> 'arg -> 'res t
val try_with_bind : f:('arg -> 'res t) -> 'arg -> 'res t

val failwith : string -> _ t

(**
   Consistent with Async.Or_exn
*)
module Infix : sig
  val (>>|?) : 'a t -> ('a -> 'b) -> 'b t
  val (>>=?) : 'a t -> ('a -> 'b t) -> 'b t

  (**
     The two following operators raise in case of Error
  *)
  val (>>|!) : 'a t -> ('a -> 'b) -> 'b
  val (>>=!) : 'a t -> ('a -> 'b t) -> 'b
end

(**
   See Result for more function working on ('a, 'b) Result.t
*)
