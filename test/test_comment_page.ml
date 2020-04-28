open! Core
open! Async
open Ocaml_reddit

let%expect_test _ =
  let link = Thing.Link.Id36.of_int 0 in
  let json = In_channel.read_all "data/comments.json" in
  let body = Test_api.make_response json in
  let%bind response = Api.For_testing.comments body ~link in
  let comment_forest = Result.ok response |> Option.value_exn |> snd in
  let top_level = Listing.children comment_forest in
  let ids = List.map top_level ~f:Thing.fullname in
  print_s [%message "" (ids : Thing.Fullname.t option list)];
  [%expect
    {|
    (ids
     ((t1_fojwyxf) (t1_fojwxw7) (t1_fojwnxi) (t1_fojwi8c) (t1_fojwjxq)
      (t1_fojxm5x) (t1_fojwiws) (t1_fojx1ud) (t1_fojx7j8) (t1_fojwi6q)
      (t1_fojx2hj) (t1_fojwkek) (t1_fojwjwy) (t1_fojx78o) (t1_fojwmji)
      (t1_fojwq6k) (t1_fojwny9) (t1_fojwlfs) (t1_fojx8c2) (t1_fojwol7)
      (t1_fojz23e) (t1_fok11dz) (t1_fojy5cq) (t1_fojwk3e) (t1_fojwqce)
      (t1_fojx7rx) (t1_fok2q5c) (t1_fojwift) (t1_fojwue0) (t1_fojwktc)
      (t1_fojwq7e) (t1_fojwiqa) (t1_fok1i42) (t1_fojxc5l) (t1_fok23wm)
      (t1_fok2tyc) (t1_fok10en) (t1_fok01r0) (t1_fojxkwf) (t1_fojwij2)
      (more_fokiubk))) |}]
;;
