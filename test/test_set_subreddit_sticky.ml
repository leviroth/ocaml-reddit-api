open! Core
open! Async
open! Import
open Ocaml_reddit

let%expect_test "set_subreddit_sticky" =
  with_cassette "set_subreddit_sticky" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "f7vspj" in
      let%bind () =
        Api.Exn.set_subreddit_sticky connection ~link ~sticky_state:(Sticky { slot = 2 })
      in
      let%bind () = [%expect] in
      let%bind () =
        Api.Exn.set_subreddit_sticky connection ~link ~sticky_state:Unsticky
      in
      [%expect])
;;