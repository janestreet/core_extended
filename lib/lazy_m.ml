open Core.Std

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
        match (try `Val (f ()) with e -> `Exn e) with
        | `Val v -> t := Evaluated_to_val v; v
        | `Exn e -> t := Evaluated_to_exn e; raise e
      end
  | Evaluating -> raise Undefined

let map m ~f = of_fun (fun () -> f (force m))

let is_val t =
  match !t with
  | Evaluated_to_val _ -> true
  | Unevaluated _
  | Evaluating
  | Evaluated_to_exn _ -> false

include (Monad.Make(struct
  type 'a t = 'a lazy_m
  let return x = of_val x
  let bind m f = of_fun (fun () -> force (f (force m)))
  let failwith str = Pervasives.failwith str
end):Monad.S with type 'a t := 'a t)


