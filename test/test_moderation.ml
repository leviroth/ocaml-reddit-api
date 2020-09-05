open! Core
open! Async
open! Import

let%expect_test "log" =
  with_cassette "log" ~f:(fun connection ->
      let%bind listing = Api.Exn.log ~limit:2 connection in
      let modactions = Listing.children listing in
      print_s [%sexp (List.map modactions ~f:Mod_action.id : Mod_action.Id.t list)];
      let%bind () =
        [%expect
          {| (6fbb7e1a-ef15-11ea-a905-0e73145e80df a0c30278-ef13-11ea-b8fd-0e6edeb4a85b) |}]
      in
      print_s [%sexp (List.map modactions ~f:Mod_action.created : Time_ns.t list)];
      [%expect
        {| ((2020-09-05 01:16:33.000000000Z) (2020-09-05 01:03:36.000000000Z)) |}])
;;

let%expect_test "reports" =
  with_cassette "reports" ~f:(fun connection ->
      let%bind listing = Api.Exn.reports connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Link hoeti3)) |}])
;;

let%expect_test "spam" =
  with_cassette "spam" ~f:(fun connection ->
      let%bind listing = Api.Exn.spam ~limit:1 connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Link hmjd8r)) |}])
;;

let%expect_test "modqueue" =
  with_cassette "modqueue" ~f:(fun connection ->
      let%bind listing = Api.Exn.modqueue connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Link hoeti3)) |}])
;;

let%expect_test "unmoderated" =
  with_cassette "unmoderated" ~f:(fun connection ->
      let%bind listing = Api.Exn.unmoderated ~limit:1 connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Link ili4vc)) |}])
;;

let%expect_test "edited" =
  with_cassette "edited" ~f:(fun connection ->
      let%bind listing = Api.Exn.edited connection in
      let children = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (children : Thing.Fullname.t list)];
      [%expect {| ((Comment g3krlj5)) |}])
;;
