open! Core
open! Async
open! Import

let%expect_test "Normalization" =
  let subreddit_names =
    List.map
      ~f:Subreddit_name.of_string
      [ "askphilosophy"; "r/askphilosophy"; "/r/askphilosophy"; "/r/AskPhilosophy" ]
  in
  let set = Set.of_list (module Subreddit_name) subreddit_names in
  print_s [%message "" (set : Set.M(Subreddit_name).t)];
  [%expect {| (set (askphilosophy)) |}];
  let hash_set = Hash_set.of_list (module Subreddit_name) subreddit_names in
  print_s [%message "" (hash_set : Hash_set.M(Subreddit_name).t)];
  [%expect {| (hash_set (askphilosophy)) |}];
  return ()
;;

let%expect_test "User subreddits" =
  let username = Username.of_string "spez" in
  let subreddit_name = Subreddit_name.user_subreddit username in
  print_s [%message "" (subreddit_name : Subreddit_name.t)];
  [%expect {| (subreddit_name u_spez) |}];
  return ()
;;
