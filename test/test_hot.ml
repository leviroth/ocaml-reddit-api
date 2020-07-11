open! Core
open! Async
open! Import
open Ocaml_reddit

let%expect_test "hot" =
  with_cassette "hot" ~f:(fun connection ->
      let%bind link =
        Api.Exn.hot ~limit:1 ~subreddit:(Subreddit_name.of_string "ThirdRealm") connection
        >>| Listing.children
        >>| List.hd_exn
      in
      print_s
        [%message
          "Link attributes"
            ~id:(Thing.Link.id link : Thing.Link.Id.t)
            ~title:(Thing.Link.title link : string)
            ~author:(Thing.Link.author link : Username.t)
            ~creation_time:(Thing.Link.creation_time link : Time_ns.t)
            ~is_stickied:(Thing.Link.is_stickied link : bool)];
      [%expect
        {|
        ("Link attributes" (id fa5dg9)
         (title "/r/thirdrealm Open Discussion Thread | February 26, 2020")
         (author BernardJOrtcutt)
         (creation_time (2020-02-26 21:55:31.000000000-05:00)) (is_stickied true)) |}])
;;
