open Core.Std

type 'a t = ('a, Exn.t) Result.t

let try_with_bind ~f x =
  try f x
  with exn -> Error exn

let try_with ~f x = try_with_bind x ~f:(fun x -> Ok (f x))

include (Result : Monad.S2 with type ('a, 'b) t := ('a, 'b) Result.t)

let failwith string = Result.fail (Failure string)

module Infix = struct
  let (>>|?) a f = map a ~f
  let (>>=?) = bind
  let (>>|!) res fct =
    match res with
    | Error exn -> raise exn
    | Ok res -> fct res
  let (>>=!) res fct =
    match res >>=? fct with
    | Error exn -> raise exn
    | Ok res -> res
end
