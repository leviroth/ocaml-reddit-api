open! Core
open! Async
open! Import

let%expect_test "friends" =
  with_cassette "friends" ~f:(fun connection ->
      let%bind body = Api.Exn.friends connection in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        (((date (Number 1598656681.0)) (id (String t2_1w72)) (name (String spez))
          (rel_id (String r9_1voxr1)))) |}])
;;

let%expect_test "blocked" =
  with_cassette "blocked" ~f:(fun connection ->
      let%bind body = Api.Exn.blocked connection in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        (((date (Number 1598788910.0)) (id (String t2_nn0q))
          (name (String ketralnis)) (rel_id (String r9_1vt16j)))) |}])
;;

let%expect_test "messaging" =
  with_cassette "messaging" ~f:(fun connection ->
      let%bind body = Api.Exn.messaging connection in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        (((date (Number 1598789198.0)) (id (String t2_1w72)) (name (String spez))
          (rel_id (String r9_1vt1em)))) |}])
;;

let%expect_test "trusted" =
  with_cassette "trusted" ~f:(fun connection ->
      let%bind body = Api.Exn.trusted connection in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        (((date (Number 1598789198.0)) (id (String t2_1w72)) (name (String spez))
          (rel_id (String r9_1vt1em)))) |}])
;;
