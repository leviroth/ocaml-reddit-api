open! Core
open! Async
open! Import
open Ocaml_reddit

let%expect_test "info" =
  with_cassette "info" ~f:(fun connection ->
      let%bind link =
        Api.Exn.info (Id [ `Link (Thing.Link.Id.of_string "hmjd8r") ]) connection
        >>| Listing.children
        >>| List.hd_exn
      in
      let link =
        match link with
        | `Link link -> link
        | _ -> raise_s [%message "Unexpected response item"]
      in
      print_s
        [%message
          "Link attributes"
            ~id:(Thing.Link.id link : Thing.Link.Id.t)
            ~title:(Thing.Link.title link : string)
            ~author:(Thing.Link.author link : Username.t)
            ~creation_time:(Thing.Link.creation_time link : Time_ns.t)
            ~moderation_info:(Thing.Link.moderation_info link : Moderation_info.t option)
            ~is_stickied:(Thing.Link.is_stickied link : bool)];
      [%expect
        {|
        ("Link attributes" (id hmjd8r) (title "This is a title")
         (author BJO_test_user) (creation_time (2020-07-06 19:42:12.000000000-04:00))
         (moderation_info
          (((state Removed)
            (history
             ((moderator L72_Elite_Kraken) (time (2020-07-14 07:56:08.000000-04:00)))))))
         (is_stickied false)) |}])
;;
