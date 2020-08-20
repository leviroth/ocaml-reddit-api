open! Core
open! Async
open! Import

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
            ~is_stickied:(Thing.Link.is_stickied link : bool)];
      [%expect
        {|
        ("Link attributes" (id hmjd8r) (title "This is a title")
         (author BJO_test_user) (creation_time (2020-07-06 23:42:12.000000000Z))
         (is_stickied false)) |}])
;;
