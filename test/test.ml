open OUnit;;
INCLUDE "../lib/config.mlh"

let all =
  let tests = [
    Extended_float_test.test;
    Extended_list_test.test;
    Extended_string_test.test;
    Extended_time_test.test;
    Iter_test.test;
    Cache_test.test;
    Shell_test.test;
    Search_test.test;
    Numbers_test.test;
    Bin_io_utils_test.test;
  ] in
  let tests =
IFDEF LINUX_EXT THEN
    Ldd_test.test :: tests
ELSE
    tests
ENDIF
  in
  TestList tests
