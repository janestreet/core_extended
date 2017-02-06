open Core

type 'a node =
  | Unevaluated of (unit -> 'a)
  | Evaluating
  | Evaluated_to_val of 'a
  | Evaluated_to_exn of exn

type 'a t = 'a node ref

type 'a lazy_m = 'a t

let of_val v = ref (Evaluated_to_val v)

let of_fun f = ref (Unevaluated f)

exception Undefined

let force t =
  match !t with
  | Evaluated_to_val v -> v
  | Evaluated_to_exn e -> raise e
  | Unevaluated f ->
      begin
        t := Evaluating;
        match f () with
        | v           -> t := Evaluated_to_val v; v
        | exception e -> t := Evaluated_to_exn e; raise e
      end
  | Evaluating -> raise Undefined

let is_val t =
  match !t with
  | Evaluated_to_val _ -> true
  | Unevaluated _
  | Evaluating
  | Evaluated_to_exn _ -> false

include Monad.Make (struct
  type 'a t = 'a lazy_m
  let return x = of_val x
  let map t ~f = of_fun (fun () -> f (force t))
  let bind m ~f = of_fun (fun () -> force (f (force m)))
  let map = `Custom map
end)


