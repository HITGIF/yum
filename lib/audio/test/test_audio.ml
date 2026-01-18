open! Core
open! Audio
open! Expect_test_helpers_core

module%test [@name "Opus"] _ = struct
  open! Opus

  let application = Application.Voip

  let%expect_test "encoder lifecycle" =
    let encoder = Encoder.create ~application |> ok_exn in
    Encoder.destroy encoder;
    [%expect {| |}]
  ;;
end
