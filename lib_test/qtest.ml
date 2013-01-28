(** Regression test runner. *)

open Core.Std;;

let tests = Qtest_lib.Std.Test.tests_of_ounit Test.all

let () = Qtest_lib.Std.Runner.main tests
