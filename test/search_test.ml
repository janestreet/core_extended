open Core.Std
open Core_extended.Std
open OUnit

let test = "search" >::: [
  "max_len" >:: (fun () ->
    "1" @? ((Search.max_len ~key:Fn.id [|"a";"bb"|]) = 2);
    "2" @? ((Search.max_len ~key:fst [|("a",2);("bb",3)|]) = 2);
  );
]
