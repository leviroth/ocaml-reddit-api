open! Core
open! Async
open! Import
open Ocaml_reddit

let%expect_test _ =
  with_cassette "comments" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "g7vyxy" in
      let%bind ({ comment_forest; _ } : Api.Parameters.Comment_response.t) =
        Api.Exn.comments connection ~link
      in
      let top_level = Listing.children comment_forest in
      let ids = List.map top_level ~f:Thing.Poly.fullname in
      print_s [%message "" (ids : Thing.Fullname.t list)];
      [%expect
        {|
    (ids
     ((Comment fojwyxf) (Comment fojwxw7) (Comment fojwi8c) (Comment fojwjxq)
      (Comment fojxm5x) (Comment fojwnxi) (Comment fojx1ud) (Comment fojx7j8)
      (Comment fojwiws) (Comment fojx78o) (Comment fojwkek) (Comment fojx2hj)
      (Comment fojwjwy) (Comment fojwmji) (Comment fojwi6q) (Comment fojwny9)
      (Comment fojwq6k) (Comment fojx8c2) (Comment fojwlfs) (Comment fojwol7)
      (Comment fok2q5c) (Comment fojz23e) (Comment fok11dz) (Comment fokhf2o)
      (Comment fojy5cq) (Comment fojx7rx) (Comment fojwktc) (Comment fojwift)
      (Comment fojwq7e) (Comment fojwqce) (Comment fojwk3e) (Comment fok2tyc)
      (Comment fojwue0) (Comment fojwiqa) (Comment fok23wm) (Comment fokmmen)
      (Comment fok1i42) (Comment fok10en) (Comment fojxc5l) (Comment fok9hr7)
      (Comment fok01r0) (Comment fojwlcf) (More_comments fokiubk))) |}])
;;
