open! Core
open! Async
open! Import

let%expect_test "user_fields" =
  with_cassette "user_fields" ~f:(fun connection ->
      let%bind user =
        Api.Exn.about_user ~username:(Username.of_string "spez") connection
      in
      print_s [%sexp (Thing.User.name user : Username.t)];
      let%bind () = [%expect {| spez |}] in
      print_s [%sexp (Thing.User.creation_time user : Time_ns.t)];
      let%bind () = [%expect {| (2005-06-06 04:00:00.000000000Z) |}] in
      print_s [%sexp (Thing.User.link_karma user : int)];
      let%bind () = [%expect {| 138988 |}] in
      print_s [%sexp (Thing.User.comment_karma user : int)];
      let%bind () = [%expect {| 743899 |}] in
      print_s [%sexp (Thing.User.awarder_karma user : int)];
      let%bind () = [%expect {| 625 |}] in
      print_s [%sexp (Thing.User.awardee_karma user : int)];
      let%bind () = [%expect {| 62329 |}] in
      print_s [%sexp (Thing.User.total_karma user : int)];
      let%bind () = [%expect {| 945841 |}] in
      print_s
        [%sexp (Thing.User.subreddit user |> Thing.Subreddit.name : Subreddit_name.t)];
      let%bind () = [%expect {| u_spez |}] in
      return ())
;;
