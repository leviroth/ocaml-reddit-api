open! Core
open! Async
open! Import

let%expect_test "friends" =
  with_cassette "friends" ~f:(fun connection ->
      let%bind body = Connection.call_exn connection (Endpoint.friends ()) in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        ((Object
          ((date (Number 1598656681.0)) (rel_id (String r9_1voxr1))
           (name (String spez)) (id (String t2_1w72))))) |}];
      return ())
;;

let%expect_test "blocked" =
  with_cassette "blocked" ~f:(fun connection ->
      let%bind body = Connection.call_exn connection (Endpoint.blocked ()) in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        ((Object
          ((date (Number 1598788910.0)) (rel_id (String r9_1vt16j))
           (name (String ketralnis)) (id (String t2_nn0q))))) |}];
      return ())
;;

let%expect_test "messaging" =
  with_cassette "messaging" ~f:(fun connection ->
      let%bind body = Connection.call_exn connection (Endpoint.messaging ()) in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        ((Object
          ((date (Number 1598789198.0)) (rel_id (String r9_1vt1em))
           (name (String spez)) (id (String t2_1w72))))) |}];
      return ())
;;

let%expect_test "trusted" =
  with_cassette "trusted" ~f:(fun connection ->
      let%bind body = Connection.call_exn connection (Endpoint.trusted ()) in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        ((Object
          ((date (Number 1598789198.0)) (rel_id (String r9_1vt1em))
           (name (String spez)) (id (String t2_1w72))))) |}];
      return ())
;;
