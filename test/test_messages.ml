open! Core
open! Async
open! Import

let message_id = Thing.Message.Id.of_string "rdjz4y"

let%expect_test "block_author" =
  with_cassette "block_author" ~f:(fun connection ->
      let id = `Message message_id in
      let%bind () = Api.Exn.block_author connection ~id in
      [%expect])
;;

let%expect_test "collapse_message" =
  with_cassette "collapse_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Api.Exn.collapse_message connection ~messages in
      [%expect])
;;

let%expect_test "uncollapse_message" =
  with_cassette "uncollapse_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Api.Exn.uncollapse_message connection ~messages in
      [%expect])
;;

let%expect_test "compose_message" =
  with_cassette "compose_message" ~f:(fun connection ->
      let%bind () =
        Api.Exn.compose_message
          connection
          ~to_:(Username.of_string "BJO_test_user")
          ~subject:"This is a message"
          ~text:"This is its body"
      in
      [%expect])
;;
