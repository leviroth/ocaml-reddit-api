open! Core
open! Async
open! Import

let%expect_test "remove" =
  with_cassette "remove" ~f:(fun connection ->
      let id = `Link (Thing.Link.Id.of_string "j4z0ig") in
      let%bind () = Api.Exn.remove connection ~id ~spam:false in
      [%expect];
      return ())
;;

let%expect_test "distinguish" =
  with_cassette "distinguish" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g7ol4ce") in
      let%bind comment = Api.Exn.distinguish connection ~id ~how:Mod in
      print_s [%sexp (Thing.Poly.fullname comment : Thing.Fullname.t)];
      [%expect {| (Comment g7ol4ce) |}];
      return ())
;;

let%expect_test "log" =
  with_cassette "log" ~f:(fun connection ->
      let%bind listing = Api.Exn.log ~limit:2 connection in
      let modactions = Listing.children listing in
      print_s [%sexp (List.map modactions ~f:Mod_action.id : Mod_action.Id.t list)];
      [%expect
        {| (6fbb7e1a-ef15-11ea-a905-0e73145e80df a0c30278-ef13-11ea-b8fd-0e6edeb4a85b) |}];
      print_s [%sexp (List.map modactions ~f:Mod_action.created : Time_ns.t list)];
      [%expect {| ((2020-09-05 01:16:33.000000000Z) (2020-09-05 01:03:36.000000000Z)) |}];
      return ())
;;

let%expect_test "reports" =
  with_cassette "reports" ~f:(fun connection ->
      let%bind listing = Api.Exn.reports connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Link hoeti3)) |}];
      return ())
;;

let%expect_test "spam" =
  with_cassette "spam" ~f:(fun connection ->
      let%bind listing = Api.Exn.spam ~limit:1 connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Link hmjd8r)) |}];
      return ())
;;

let%expect_test "modqueue" =
  with_cassette "modqueue" ~f:(fun connection ->
      let%bind listing = Api.Exn.modqueue connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Link hoeti3)) |}];
      return ())
;;

let%expect_test "unmoderated" =
  with_cassette "unmoderated" ~f:(fun connection ->
      let%bind listing = Api.Exn.unmoderated ~limit:1 connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Link ili4vc)) |}];
      return ())
;;

let%expect_test "edited" =
  with_cassette "edited" ~f:(fun connection ->
      let%bind listing = Api.Exn.edited connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Comment g3krlj5)) |}];
      return ())
;;

let%expect_test "ignore_reports" =
  with_cassette "ignore_reports" ~f:(fun connection ->
      let%bind () =
        Api.Exn.ignore_reports connection ~id:(`Link (Thing.Link.Id.of_string "ili4vc"))
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "unignore_reports" =
  with_cassette "unignore_reports" ~f:(fun connection ->
      let%bind () =
        Api.Exn.unignore_reports connection ~id:(`Link (Thing.Link.Id.of_string "ili4vc"))
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "leavecontributor" =
  with_cassette "leavecontributor" ~f:(fun connection ->
      let%bind () =
        Api.Exn.leavecontributor
          connection
          ~subreddit:(Thing.Subreddit.Id.of_string "390u2")
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "leavemoderator" =
  with_cassette "leavemoderator" ~f:(fun connection ->
      let%bind () =
        Api.Exn.leavemoderator
          connection
          ~subreddit:(Thing.Subreddit.Id.of_string "390u2")
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "stylesheet" =
  with_cassette "stylesheet" ~f:(fun connection ->
      let%bind stylesheet =
        Api.Exn.stylesheet connection ~subreddit:(Subreddit_name.of_string "Thirdrealm")
      in
      let images =
        List.map (Stylesheet.images stylesheet) ~f:(fun image ->
            [%sexp { url : string = Stylesheet.Image.link image }])
      in
      print_s
        [%sexp
          { stylesheet : string = Stylesheet.stylesheet_text stylesheet
          ; images : Sexp.t list
          }];
      [%expect
        {|
          ((stylesheet "body {font-family: monospace}")
           (images (((url "url(%%leviroth%%)"))))) |}];
      return ())
;;
