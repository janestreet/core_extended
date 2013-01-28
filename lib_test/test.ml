open OUnit;;

let all = TestList [
  Core_extended.Extended_string.ounit_tests ();
  Core_extended.Cbuffer.ounit_tests ();
  Core_extended.Extended_array.ounit_tests ();
  Core_extended.Extended_filename.ounit_tests ();
  Core_extended.Inline_tests.tests ();
  Extended_float_test.test;
  Extended_list_test.test;
  Extended_string_test.test;
  Extended_time_test.test;
  Iter_test.test;
  Ldd_test.test;
  Cache_test.test;
  Shell_test.test;
  Search_test.test;
  Union_find_test.test;
  Numbers_test.test;
  Bin_io_utils_test.test;
]
