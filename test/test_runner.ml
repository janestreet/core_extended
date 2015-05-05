open OUnit;;

let () =
  ignore
    (run_test_tt_main
       ~arg_specs:Ldd_test.args
       Test.all:
       OUnit.test_result list)
