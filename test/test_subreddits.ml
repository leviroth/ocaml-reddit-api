open! Core
open! Async
open! Import

let subreddit = Subreddit_name.of_string "ThirdRealm"

let%expect_test "banned" =
  with_cassette "banned" ~f:(fun connection ->
      let%bind bans = Api.Exn.banned connection ~subreddit >>| Listing.children in
      List.iter bans ~f:(fun ban ->
          print_s
            [%sexp
              { relationship_id : string = Ban.relationship_id ban
              ; username : Username.t = Ban.username ban
              ; user_id : Thing.User.Id.t = Ban.user_id ban
              ; note : string = Ban.note ban
              ; days_left : int option = Ban.days_left ban
              ; date : Time_ns.t = Ban.date ban
              }]);
      [%expect
        {|
          ((relationship_id rb_26zyq4m) (username ketralnis) (user_id nn0q)
           (note "blah blah blah") (days_left (2))
           (date (2020-09-17 12:50:33.000000000Z)))
          ((relationship_id rb_26zynhm) (username spez) (user_id 1w72)
           (note "blah blah blah: Capricious ban") (days_left ())
           (date (2020-09-17 12:49:32.000000000Z))) |}];
      return ())
;;

let%expect_test "muted" =
  with_cassette "muted" ~f:(fun connection ->
      let%bind mutes = Api.Exn.muted connection ~subreddit >>| Listing.children in
      List.iter mutes ~f:(fun mute ->
          print_s
            [%sexp
              { relationship_id : string = Mute.relationship_id mute
              ; username : Username.t = Mute.username mute
              ; user_id : Thing.User.Id.t = Mute.user_id mute
              ; date : Time_ns.t = Mute.date mute
              }]);
      [%expect
        {|
          ((relationship_id Mute_c00caa20-fac7-11ea-a7ae-22f314d2d040)
           (username BJO_test_user) (user_id xw1ym)
           (date (2020-09-19 22:30:41.000000000Z))) |}];
      return ())
;;
