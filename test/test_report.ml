open! Core
open! Async
open! Import
open Ocaml_reddit

let%expect_test "report" =
  with_cassette "report" ~f:(fun connection ->
      let target = `Link (Thing.Link.Id.of_string "hony5b") in
      let%bind () = Api.Exn.report connection ~target ~reason:"Test report" in
      [%expect {| |}])
;;
