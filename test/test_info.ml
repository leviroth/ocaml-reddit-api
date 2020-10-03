open! Core
open! Async
open! Import

let%expect_test "info" =
  with_cassette "info" ~f:(fun connection ->
      let%bind link =
        Api.Exn.info (Id [ `Link (Thing.Link.Id.of_string "hmjd8r") ]) connection
        >>| List.hd_exn
      in
      let link =
        match link with
        | `Link link -> link
        | _ -> raise_s [%message "Unexpected response item"]
      in
      print_s
        [%sexp
          { id : Thing.Link.Id.t = Thing.Link.id link
          ; title : string = Thing.Link.title link
          ; author : Username.t option = Thing.Link.author link
          ; creation_time : Time_ns.t = Thing.Link.creation_time link
          ; is_stickied : bool = Thing.Link.is_stickied link
          }];
      [%expect
        {|
        ((id hmjd8r) (title "This is a title") (author (BJO_test_user))
         (creation_time (2020-07-06 23:42:12.000000000Z)) (is_stickied false)) |}];
      return ())
;;
