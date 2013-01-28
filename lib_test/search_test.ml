open OUnit
open Core.Std

let test = "search" >::: [
  "max_len" >:: (fun () ->
    "1" @? ((Core_extended.Std.Search.max_len ~key:ident [|"a";"bb"|]) = 2);
    "2" @? ((Core_extended.Std.Search.max_len ~key:fst [|("a",2);("bb",3)|]) = 2);
  );
]
