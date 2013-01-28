open OUnit
open Core.Std
open Core_extended.Std

let test = "iter" >::: [
  "main" >:: (fun () ->
    let l = [1;2;3;4;5] in
    let a = Array.of_list l in
    "list" @? (l = Iter.to_list (Iter.of_list l) ~f:ident);
    "array" @? (a = Iter.to_array (Iter.of_array a) ~f:ident);
    "print" @? (let b = Buffer.create 10 in
                Iter.i (Iter.of_list l)
                  ~f:(fun x -> bprintf b "%d" x);
                Buffer.contents b = "12345");
    "map1" @? (let f x = x * x in
               Iter.to_array (Iter.map (Iter.of_array a) ~f)
                 ~f:ident = Array.map ~f a);
    "map2" @? (let f x = x * x * x and g n = String.length (string_of_int n) in
               Iter.to_list (Iter.map (Iter.of_list l) ~f)
                 ~f:g = List.map l ~f:(fun n -> g (f n)));
    "reduce" @? (List.fold ~init:0 ~f:(+) l =
        Iter.reduce ~init:0 ~f:(+) (Iter.of_list l));
  );
]
