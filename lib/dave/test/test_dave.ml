open! Core
open! Dave
open! Expect_test_helpers_core

let%expect_test "max_supported_protocol_version" =
  print_s [%sexp (max_supported_protocol_version : int)];
  [%expect {| 1 |}]
;;
