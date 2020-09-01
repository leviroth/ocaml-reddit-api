open! Core
open! Async
open! Import

let%expect_test "edit" =
  with_cassette "edit" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind comment = Api.Exn.edit connection ~id ~text:"edited text" in
      print_s [%sexp (Thing.Poly.fullname comment : Thing.Fullname.t)];
      [%expect {| (Comment g3krlj5) |}])
;;

let%expect_test "save" =
  with_cassette "save" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind () = Api.Exn.save connection ~id in
      [%expect {| |}])
;;

let%expect_test "unsave" =
  with_cassette "unsave" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind () = Api.Exn.unsave connection ~id in
      (* Unsave is idempotent *)
      let%bind () = Api.Exn.unsave connection ~id in
      [%expect {| |}])
;;

let%expect_test "send_replies" =
  with_cassette "send_replies" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind () = Api.Exn.send_replies connection ~id ~enabled:true in
      let%bind () = [%expect] in
      let%bind () = Api.Exn.send_replies connection ~id ~enabled:false in
      [%expect])
;;
