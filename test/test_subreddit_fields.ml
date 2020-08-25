open! Core
open! Async
open! Import

let%expect_test "subreddit_fields" =
  with_cassette "subreddit_fields" ~f:(fun connection ->
      let%bind subreddit =
        Api.Exn.about_subreddit ~subreddit:(Subreddit_name.of_string "ocaml") connection
      in
      print_s [%sexp (Thing.Subreddit.name subreddit : Subreddit_name.t)];
      let%bind () = [%expect {| ocaml |}] in
      print_s [%sexp (Thing.Subreddit.title subreddit : string)];
      let%bind () = [%expect {| "let reddit = OCaml;;" |}] in
      print_s [%sexp (String.prefix (Thing.Subreddit.description subreddit) 80 : string)];
      let%bind () =
        [%expect
          {|
        "[OCaml](http://ocaml.org/) is a statically typed functional programming language" |}]
      in
      print_s [%sexp (Thing.Subreddit.subscribers subreddit : int)];
      let%bind () = [%expect {| 7554 |}] in
      print_s [%sexp (Thing.Subreddit.active_users subreddit : int)];
      let%bind () = [%expect {| 12 |}] in
      print_s [%sexp (Thing.Subreddit.creation_time subreddit : Time_ns.t)];
      let%bind () = [%expect {| (2008-01-25 13:24:41.000000000Z) |}] in
      return ())
;;
