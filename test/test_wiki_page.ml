open! Core
open! Async
open! Import

let page : Wiki_page.Id.t =
  { subreddit = Some (Subreddit_name.of_string "ThirdRealm"); page = "index" }
;;

let%expect_test "add_wiki_editor" =
  with_cassette "add_wiki_editor" ~f:(fun connection ->
      Api.Exn.add_wiki_editor
        connection
        ~page
        ~user:(Username.of_string "L72_Elite_Kraken"))
;;

let%expect_test "remove_wiki_editor" =
  with_cassette "remove_wiki_editor" ~f:(fun connection ->
      Api.Exn.remove_wiki_editor
        connection
        ~page
        ~user:(Username.of_string "L72_Elite_Kraken"))
;;
