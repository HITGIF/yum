open! Core
open! Dave
open! Expect_test_helpers_core

module%test [@name "Dave"] _ = struct
  let%expect_test "a" =
    print_s [%sexp (max_supported_protocol_version : int)];
    [%expect {| 1 |}]
  ;;
end
