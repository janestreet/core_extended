open Core.Std
open Core_extended.Std
open OUnit

module Lru = Cache.Lru

let seteq l1 l2 =
  List.sort ~cmp:compare l1 = List.sort ~cmp:compare l2

let str l =
  "[" ^ String.concat ~sep:", " (List.map ~f:string_of_int l) ^ "]"

let dead = ref []
let clear_dead () = dead := []
let lru = Lru.create ~destruct:(Some (fun v -> dead := v::!dead)) 3

let touch v = Lru.add lru ~key:v ~data:v

let test = "lru" >::: [
  "1234-1" >:: (fun () -> clear_dead ();
    touch 1; touch 2; touch 3; touch 4;
    (str !dead) @? seteq !dead [1]);
  "235-4" >:: (fun () -> clear_dead ();
    touch 2; touch 3; touch 5;
    (str !dead) @? seteq !dead [4]);
  "36-2" >:: (fun () -> clear_dead ();
    touch 3; touch 6;
    (str !dead) @? seteq !dead [2]);
  "7-5" >:: (fun () -> clear_dead ();
    touch 7;
    (str !dead) @? seteq !dead [5]);
  "c-763" >:: (fun () ->
    clear_dead ();
    Lru.clear lru;
    (str !dead) @? seteq !dead [7;6;3]);
  "890-" >:: (fun () ->
    clear_dead ();
    touch 8; touch 9; touch 0;
    (str !dead) @? seteq !dead []);
]
