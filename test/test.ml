open Batteries;;
open Jhupllib;;
open OUnit2

let all_tests =
  [ Test_utils.tests
  ];;

let () =
  run_test_tt_main ("Tests" >::: all_tests)
;;
