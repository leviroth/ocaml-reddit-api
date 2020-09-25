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

let%expect_test "moderators" =
  with_cassette "moderators" ~f:(fun connection ->
      let%bind () =
        Api.Exn.moderators connection ~subreddit:(Subreddit_name.of_string "redditdev")
        >>| Listing.children
        >>| List.iter ~f:(fun moderator ->
                print_s
                  [%sexp
                    { relationship_id : Moderator.Id.t =
                        Moderator.relationship_id moderator
                    ; username : Username.t = Moderator.username moderator
                    ; user_id : Thing.User.Id.t = Moderator.user_id moderator
                    ; date : Time_ns.t = Moderator.date moderator
                    ; permissions : string list = Moderator.permissions moderator
                    ; flair_text : string option = Moderator.flair_text moderator
                    ; flair_css_class : string option =
                        Moderator.flair_css_class moderator
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_l5k8) (username ketralnis) (user_id nn0q)
           (date (2008-06-18 15:51:21.000000000Z)) (permissions (all))
           (flair_text ("reddit admin")) (flair_css_class ()))
          ((relationship_id rb_l5ka) (username spez) (user_id 1w72)
           (date (2008-06-18 15:51:23.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_l5kb) (username jedberg) (user_id 1wnj)
           (date (2008-06-18 15:51:25.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_l5kc) (username kn0thing) (user_id 1wh0)
           (date (2008-06-18 15:51:27.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_l5kj) (username KeyserSosa) (user_id 1wjm)
           (date (2008-06-18 15:51:37.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_eb0p7) (username spladug) (user_id 3imtq)
           (date (2011-04-18 04:56:51.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_ggr26) (username chromakode) (user_id 7onf)
           (date (2011-06-14 07:24:34.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_59vvph) (username kemitche) (user_id 3jo4g)
           (date (2014-03-14 21:17:50.000000000Z)) (permissions (all))
           (flair_text ("Reddit Admin")) (flair_css_class ()))
          ((relationship_id rb_lue6xm) (username bboe) (user_id 3pz6e)
           (date (2017-01-31 21:09:49.000000000Z)) (permissions (all))
           (flair_text ("PRAW Author")) (flair_css_class ()))
          ((relationship_id rb_os2pwe) (username taylorkline) (user_id 13jrwt)
           (date (2017-05-25 00:42:54.000000000Z)) (permissions (wiki flair))
           (flair_text ("Bot Developer")) (flair_css_class ()))
          ((relationship_id rb_qv0r5h) (username bsimpson) (user_id 3c639)
           (date (2017-09-07 17:30:32.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_xovcgy) (username Stuck_In_the_Matrix) (user_id bk1iz)
           (date (2018-06-22 06:23:11.000000000Z))
           (permissions (posts access mail config flair))
           (flair_text ("Pushshift.io data scientist")) (flair_css_class ())) |}];
      return ())
;;

let%expect_test "delete_subreddit_banner" =
  with_cassette "delete_subreddit_banner" ~f:(fun connection ->
      let%bind () =
        Api.Exn.delete_subreddit_image connection ~subreddit ~image:Mobile_banner
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "delete_subreddit_header" =
  with_cassette "delete_subreddit_header" ~f:(fun connection ->
      let%bind () = Api.Exn.delete_subreddit_image connection ~subreddit ~image:Header in
      [%expect {| |}];
      return ())
;;

let%expect_test "delete_subreddit_icon" =
  with_cassette "delete_subreddit_icon" ~f:(fun connection ->
      let%bind () =
        Api.Exn.delete_subreddit_image connection ~subreddit ~image:Mobile_icon
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "delete_subreddit_image" =
  with_cassette "delete_subreddit_image" ~f:(fun connection ->
      let%bind () =
        Api.Exn.delete_subreddit_image
          connection
          ~subreddit
          ~image:(Stylesheet_image { name = "leviroth" })
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "search_subreddit_names" =
  with_cassette "search_subreddit_names" ~f:(fun connection ->
      let%bind subreddits = Api.Exn.search_subreddit_names connection ~query:"python" in
      print_s [%sexp (subreddits : Subreddit_name.t list)];
      [%expect
        {|
          (Python pythontips PythonProjects2 pythonforengineers python_netsec
           PythonJobs pythoncoding PythonGUI PythonNoobs pythonclass) |}];
      return ())
;;

let%expect_test "submit_text" =
  with_cassette "submit_text" ~f:(fun connection ->
      let%bind submit_text =
        Api.Exn.submit_text connection ~subreddit:(Subreddit_name.of_string "philosophy")
      in
      List.iter [ `markdown; `HTML ] ~f:(fun markup ->
          print_s
            [%sexp
              (Submit_text.submit_text markup submit_text |> String.sub ~pos:0 ~len:80
                : string)]);
      [%expect
        {|
          "Please make sure you have read the /r/philosophy posting rules which can be foun"
          "<!-- SC_OFF --><div class=\"md\"><p>Please make sure you have read the <a href=\"/r" |}];
      return ())
;;

let%expect_test "subreddit_autocomplete" =
  with_cassette "subreddit_autocomplete" ~f:(fun connection ->
      let%bind () =
        Api.Exn.subreddit_autocomplete connection ~query:"python"
        >>| Listing.children
        >>| List.iter ~f:(fun subreddit ->
                print_s [%sexp (Thing.Subreddit.name subreddit : Subreddit_name.t)])
      in
      [%expect
        {|
          Python
          pythontips
          pythoncoding
          PythonProjects2
          pythonforengineers |}];
      return ())
;;
