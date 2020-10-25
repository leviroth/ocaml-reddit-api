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

let%expect_test "toggle_wiki_revision_visibility" =
  with_cassette "toggle_wiki_revision_visibility" ~f:(fun connection ->
      let%bind result =
        Api.Exn.toggle_wiki_revision_visibility
          connection
          ~page
          ~revision:(Uuid.of_string "8048c97c-52ba-11e7-ab00-0ad38c20ef7e")
      in
      print_s [%sexp (result : [ `Became_hidden | `Became_visible ])];
      [%expect {| Became_hidden |}];
      return ())
;;

let%expect_test "revert_wiki_page" =
  with_cassette "revert_wiki_page" ~f:(fun connection ->
      Api.Exn.revert_wiki_page
        connection
        ~page
        ~revision:(Uuid.of_string "e4d3d130-52b9-11e7-9d0c-0e1b806ed802"))
;;
