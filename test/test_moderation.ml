open! Core
open! Async
open! Import

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
