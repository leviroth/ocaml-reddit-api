open! Core
open! Async
open! Import

let%expect_test "block_author" =
  with_cassette "block_author" ~f:(fun connection ->
      let id = `Message (Thing.Message.Id.of_string "rdjz4y") in
      let%bind () = Api.Exn.block_author connection ~id in
      [%expect])
;;
