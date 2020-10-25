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
          ~revision:
            (Wiki_page.Revision.Id.of_string "8048c97c-52ba-11e7-ab00-0ad38c20ef7e")
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
        ~revision:(Wiki_page.Revision.Id.of_string "e4d3d130-52b9-11e7-9d0c-0e1b806ed802"))
;;

let%expect_test "wiki_page_revisions" =
  with_cassette "wiki_page_revisions" ~f:(fun connection ->
      let%bind revisions =
        Api.Exn.wiki_page_revisions
          ~pagination:
            (After
               (Listing.Page_id.of_string
                  "WikiRevision_bde92910-52b1-11e7-bf40-120ea8b0860a"))
          ~limit:3
          connection
          ~page
        >>| Listing.children
      in
      List.iter revisions ~f:(fun revision ->
          print_s
            [%sexp
              { author : Username.t option =
                  Wiki_page.Revision.author revision |> Option.map ~f:Thing.User.name
              ; page_name : string = Wiki_page.Revision.page_name revision
              ; id : Wiki_page.Revision.Id.t = Wiki_page.Revision.id revision
              ; reason : string option = Wiki_page.Revision.reason revision
              ; timestamp : Time_ns.t = Wiki_page.Revision.timestamp revision
              ; hidden : bool = Wiki_page.Revision.hidden revision
              }]);
      [%expect
        {|
          ((author (BJO_test_mod)) (page_name index)
           (id 7b080602-52b1-11e7-9041-0ed4553efb98) (reason ())
           (timestamp (2017-06-16 16:33:08.000000000Z)) (hidden false))
          ((author (BJO_test_mod)) (page_name index)
           (id 7ae7dca6-52b1-11e7-96d7-0a0b84aef9f8) (reason ())
           (timestamp (2017-06-16 16:33:08.000000000Z)) (hidden false))
          ((author (BJO_test_mod)) (page_name index)
           (id 610979ca-52b1-11e7-ad39-0e1b806ed802) (reason ())
           (timestamp (2017-06-16 16:32:24.000000000Z)) (hidden false)) |}];
      return ())
;;
