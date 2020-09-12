open! Core
open! Async
open! Import

let message_id = Thing.Message.Id.of_string "rdjz4y"

let%expect_test "block_author" =
  with_cassette "block_author" ~f:(fun connection ->
      let id = `Message message_id in
      let%bind () = Api.Exn.block_author connection ~id in
      [%expect];
      return ())
;;

let%expect_test "collapse_message" =
  with_cassette "collapse_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Api.Exn.collapse_message connection ~messages in
      [%expect];
      return ())
;;

let%expect_test "uncollapse_message" =
  with_cassette "uncollapse_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Api.Exn.uncollapse_message connection ~messages in
      [%expect];
      return ())
;;

let%expect_test "read_message" =
  with_cassette "read_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Api.Exn.read_message connection ~messages in
      [%expect];
      return ())
;;

let%expect_test "unread_message" =
  with_cassette "unread_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Api.Exn.unread_message connection ~messages in
      [%expect];
      return ())
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
      [%expect];
      return ())
;;

let%expect_test "inbox" =
  with_cassette "inbox" ~f:(fun connection ->
      let%bind listing = Api.Exn.inbox ~limit:2 connection ~mark_read:false in
      Listing.children listing
      |> List.iter ~f:(fun thing ->
             print_s [%sexp (Thing.Poly.fullname thing : Thing.Fullname.t)]);
      [%expect {|
          (Comment g3u0ce8)
          (Message rdjz4y) |}];
      return ())
;;

let%expect_test "unread" =
  with_cassette "unread" ~f:(fun connection ->
      let%bind listing = Api.Exn.unread connection ~mark_read:false in
      Listing.children listing
      |> List.iter ~f:(fun thing ->
             print_s [%sexp (Thing.Poly.fullname thing : Thing.Fullname.t)]);
      [%expect {| (Comment g3u0ce8) |}];
      return ())
;;

let%expect_test "sent" =
  with_cassette "sent" ~f:(fun connection ->
      let%bind listing = Api.Exn.sent ~limit:2 connection in
      Listing.children listing
      |> List.iter ~f:(fun message ->
             print_s [%sexp (Thing.Message.id message : Thing.Message.Id.t)]);
      [%expect {|
          rdkr3p
          rdk8sp |}];
      return ())
;;
