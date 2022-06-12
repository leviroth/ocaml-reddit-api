open! Core
open! Async
open! Import

let%expect_test "link_fields__url_contents" =
  with_cassette "link_fields__url_contents" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "lfx8ac" in
      let%bind ({ link; _ } : Comment_response.t) =
        Connection.call_exn connection (Endpoint.comments () ~link)
      in
      let contents = Thing.Link.contents link in
      print_s [%sexp (contents : Thing.Link.Contents.t)];
      [%expect {| (Url https://i.redd.it/ap95zxoqleg61.jpg) |}];
      return ())
;;

let%expect_test "link_fields__self_contents" =
  with_cassette "link_fields__self_contents" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "kvzaot" in
      let%bind ({ link; _ } : Comment_response.t) =
        Connection.call_exn connection (Endpoint.comments () ~link)
      in
      let contents : Thing.Link.Contents.t =
        match Thing.Link.contents link with
        | Self_text text -> Self_text (String.prefix text 100)
        | v -> v
      in
      print_s [%sexp (contents : Thing.Link.Contents.t)];
      [%expect
        {|
        (Self_text
         "As part of modernizing our OAuth2 infrastructure, we\226\128\153re implementing some potentially breaking cha") |}];
      return ())
;;
