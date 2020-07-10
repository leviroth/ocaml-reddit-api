open! Core
open! Async
open! Import
open Ocaml_reddit

let%expect_test "set_suggested_sort" =
  with_cassette "set_suggested_sort" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "hmjghn" in
      let%bind () = Api.Exn.set_suggested_sort connection ~sort:(Some New) ~link in
      [%expect {| |}])
;;
