open Core.Std
open Core_extended.Std
open OUnit
module Sh = Core_extended.Shell
let test =
  "shell" >:::
    [ "run" >::
        (fun () ->
          "length" @? ((Sh.sh_lines "yes yes | head -n 200000")
                        =
              List.init 200_000 ~f:(fun _num -> "yes")));

    ]
