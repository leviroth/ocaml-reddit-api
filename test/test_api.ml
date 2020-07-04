open! Core
open! Async
open! Import
open Ocaml_reddit

let%expect_test "me" =
  with_cassette "me" ~f:(fun connection ->
      let%bind me = Api.Exn.me connection in
      let id = Thing.User.id me in
      print_s [%sexp (id : Thing.User.Id.t)];
      [%expect {|
    16r83m |}])
;;
