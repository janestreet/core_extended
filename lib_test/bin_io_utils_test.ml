open Core.Std
open Core_extended.Std
open OUnit

type el = [ `Test1 | `Test2 | `Test3 ] with bin_io
type t = el list with bin_io

let test =
  "Bin_io_utils_test" >:::
    [
      "load/save" >:: (fun () ->
        let v = [ `Test1; `Test2; `Test3 ] in
        let file = "bin_io_test.bin" in
        Bin_io_utils.save file bin_writer_t v;
        let v' = Bin_io_utils.load file bin_read_t in
        Sys.remove file;
        "same" @? (v = v'));
    ]
