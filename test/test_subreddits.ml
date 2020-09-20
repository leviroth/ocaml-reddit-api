open! Core
open! Async
open! Import
open Relationship

let subreddit = Subreddit_name.of_string "ThirdRealm"

let%expect_test "banned" =
  with_cassette "banned" ~f:(fun connection ->
      let%bind () =
        Api.Exn.banned connection ~subreddit
        >>| Listing.children
        >>| List.iter ~f:(fun ban ->
                print_s
                  [%sexp
                    { relationship_id : Ban.Id.t = Ban.relationship_id ban
                    ; username : Username.t = Ban.username ban
                    ; user_id : Thing.User.Id.t = Ban.user_id ban
                    ; note : string = Ban.note ban
                    ; days_left : int option = Ban.days_left ban
                    ; date : Time_ns.t = Ban.date ban
                    }])
      in
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
      let%bind () =
        Api.Exn.muted connection ~subreddit
        >>| Listing.children
        >>| List.iter ~f:(fun mute ->
                print_s
                  [%sexp
                    { relationship_id : Mute.Id.t = Mute.relationship_id mute
                    ; username : Username.t = Mute.username mute
                    ; user_id : Thing.User.Id.t = Mute.user_id mute
                    ; date : Time_ns.t = Mute.date mute
                    }])
      in
      [%expect
        {|
          ((relationship_id Mute_c00caa20-fac7-11ea-a7ae-22f314d2d040)
           (username BJO_test_user) (user_id xw1ym)
           (date (2020-09-19 22:30:41.000000000Z))) |}];
      return ())
;;

let%expect_test "wiki_banned" =
  with_cassette "wiki_banned" ~f:(fun connection ->
      let%bind () =
        Api.Exn.wiki_banned connection ~subreddit
        >>| Listing.children
        >>| List.iter ~f:(fun ban ->
                print_s
                  [%sexp
                    { relationship_id : Ban.Id.t = Ban.relationship_id ban
                    ; username : Username.t = Ban.username ban
                    ; user_id : Thing.User.Id.t = Ban.user_id ban
                    ; note : string = Ban.note ban
                    ; days_left : int option = Ban.days_left ban
                    ; date : Time_ns.t = Ban.date ban
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_276wzg3) (username BJO_test_user) (user_id xw1ym)
           (note bar) (days_left (2)) (date (2020-09-19 22:39:05.000000000Z))) |}];
      return ())
;;

let%expect_test "contributors" =
  with_cassette "contributors" ~f:(fun connection ->
      let%bind () =
        Api.Exn.contributors connection ~subreddit
        >>| Listing.children
        >>| List.iter ~f:(fun contributor ->
                print_s
                  [%sexp
                    { relationship_id : Contributor.Id.t =
                        Contributor.relationship_id contributor
                    ; username : Username.t = Contributor.username contributor
                    ; user_id : Thing.User.Id.t = Contributor.user_id contributor
                    ; date : Time_ns.t = Contributor.date contributor
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_rktlv8) (username BJO_test_user) (user_id xw1ym)
           (date (2017-10-17 22:31:49.000000000Z)))
          ((relationship_id rb_g6xsft) (username BJO_test_mod) (user_id xw27h)
           (date (2016-06-05 02:12:22.000000000Z))) |}];
      return ())
;;

let%expect_test "wiki_contributors" =
  with_cassette "wiki_contributors" ~f:(fun connection ->
      let%bind () =
        Api.Exn.wiki_contributors connection ~subreddit
        >>| Listing.children
        >>| List.iter ~f:(fun contributor ->
                print_s
                  [%sexp
                    { relationship_id : Contributor.Id.t =
                        Contributor.relationship_id contributor
                    ; username : Username.t = Contributor.username contributor
                    ; user_id : Thing.User.Id.t = Contributor.user_id contributor
                    ; date : Time_ns.t = Contributor.date contributor
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_278zt5u) (username L72_Elite_Kraken) (user_id 16r83m)
           (date (2020-09-20 16:45:27.000000000Z))) |}];
      return ())
;;
