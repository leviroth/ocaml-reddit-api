open! Core
open! Async
open Ocaml_reddit

let%expect_test _ =
  let link = Thing.Link.Id.of_int 0 in
  let json = In_channel.read_all "data/comments.json" in
  let body = Test_api.make_response json in
  let%bind response = Api.For_testing.comments body ~link in
  let ({ comment_forest; _ } : Api.Parameters.Comment_response.t) =
    Result.ok response |> Option.value_exn
  in
  let top_level = Listing.children comment_forest in
  let ids =
    List.map top_level ~f:(function
        | `Comment comment -> `Comment (Thing.Comment.id comment)
        | `More_comments more_comments ->
          `More_comments (Thing.More_comments.id more_comments))
  in
  print_s [%message "" (ids : Fullname.t list)];
  [%expect
    {|
    (ids
     ((Comment fojwyxf) (Comment fojwxw7) (Comment fojwnxi) (Comment fojwi8c)
      (Comment fojwjxq) (Comment fojxm5x) (Comment fojwiws) (Comment fojx1ud)
      (Comment fojx7j8) (Comment fojwi6q) (Comment fojx2hj) (Comment fojwkek)
      (Comment fojwjwy) (Comment fojx78o) (Comment fojwmji) (Comment fojwq6k)
      (Comment fojwny9) (Comment fojwlfs) (Comment fojx8c2) (Comment fojwol7)
      (Comment fojz23e) (Comment fok11dz) (Comment fojy5cq) (Comment fojwk3e)
      (Comment fojwqce) (Comment fojx7rx) (Comment fok2q5c) (Comment fojwift)
      (Comment fojwue0) (Comment fojwktc) (Comment fojwq7e) (Comment fojwiqa)
      (Comment fok1i42) (Comment fojxc5l) (Comment fok23wm) (Comment fok2tyc)
      (Comment fok10en) (Comment fok01r0) (Comment fojxkwf) (Comment fojwij2)
      (More_comments fokiubk))) |}]
;;
