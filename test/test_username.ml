open! Core
open! Async
open! Import

let%expect_test "Normalization" =
  let usernames =
    List.map ~f:Username.of_string [ "spez"; "u/spez"; "/u/spez"; "/u/SPEZ" ]
  in
  let set = Set.of_list (module Username) usernames in
  print_s [%message "" (set : Set.M(Username).t)];
  [%expect {| (set (spez)) |}];
  let hash_set = Hash_set.of_list (module Username) usernames in
  print_s [%message "" (hash_set : Hash_set.M(Username).t)];
  [%expect {| (hash_set (spez)) |}];
  return ()
;;
