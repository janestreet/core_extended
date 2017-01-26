open Core

type ('a,'b) c = ('a -> 'b) -> 'b

let ( >: ) x t y = t (fun x -> x) y x
(* let ( >>: ) x t = t (fun x -> x) (fun x -> x) x *)

let __ k = k (fun f x -> f x)

let list map k = k (fun f ->
  List.map ~f:(map f)
)

let option map k = k (fun f ->
  Option.map ~f:(map f)
)

let ok map k = k (fun f ->
  function
  | Error _ as e -> e
  | Ok x -> Ok (map f x)
)

let error map k = k (fun f ->
  function
  | Error x -> Error (map f x)
  | Ok _ as ok -> ok
)

(*
let first map k = k (fun f (a,b) ->
  (map f a, b)
)

let second map k = k (fun f (a,b) ->
  (a, map f b)
)
*)

(*
let both map k = k (fun f (a,b) ->
  (map f a, map f b)
)
*)

let string k = k (fun f -> String.map ~f)

let map x ~f = x f

let iter x ~f = ignore (map x ~f:(fun x -> f x; x))

let fold x ~init ~f =
  let acc = ref init in
  iter x ~f:(fun x ->
    acc := f !acc x
  );
  !acc

let length x = fold x ~init:0 ~f:(fun acc _ -> acc + 1)
let to_list x = List.rev (fold x ~init:[] ~f:(fun l x -> x :: l))

(*
let create_type map = (); fun map' k -> k (fun f x ->
  map x ~f:(map' f)
)
*)

let (>>|) x f = map x ~f

let%test _ = map ([[1];[2;3];[3;4]] >: __ list) ~f:List.length = [1;2;2]

let%test _ = map ([[1];[2;3];[3;4]] >: __) ~f:List.length = 3

let%test _ = length ([[1];[2;3];[3;4]] >: __ list list) = 5

let%test _ = length ([[1];[2;3];[3;4]] >: __ list) = 3

let%test _ = length ( [1;2;3;4] >: __ ) = 1

let%test _ = ([ "asdf"; "a" ] >: string list >>| fun c ->
        Char.uppercase c
) = [ "ASDF"; "A" ]

let%test _ = length ([ Error "foo"; Ok (); Error "banana"] >: __ error list) = 2
let%test _ = length ([ Error "foo"; Ok (); Error "banana"] >: __ ok list) = 1

let%test _ = to_list ([ [ Error "foo"]; [Ok (); Error "banana"]]
                   >: __ error list list) = [ "foo"; "banana" ]


let%test _ = to_list ([Some 3; None; Some 4] >: __ option list) = [3;4]

(*
TEST = to_list ([Some (1,'s'); None; Some (3,'a')]
                >: __ first option list)
  = [1;3]
*)
