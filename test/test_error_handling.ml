open! Core
open! Async
open! Import

let%expect_test "Json error" =
  let ban_message =
    List.init
      15
      ~f:
        (const
           "This is a ban message. Because it is so long, it will be rejected by Reddit. ")
    |> String.concat
  in
  print_s [%sexp (String.length ban_message : int)];
  [%expect {| 1155 |}];
  with_cassette "ban__long_message" ~f:(fun connection ->
      let%bind () =
        match%bind
          Connection.call
            connection
            (Endpoint.add_relationship
               ~relationship:Banned
               ~username:(Username.of_string "spez")
               ~duration:Permanent
               ~subreddit:(Subreddit_name.of_string "thirdrealm")
               ~ban_message
               ())
        with
        | Ok () -> raise_s [%message "Expected API error"]
        | Error error ->
          print_s [%message "" (error : Endpoint.Error.t Connection.Error.t)];
          return ()
      in
      [%expect
        {|
          (error
           (Endpoint_error
            (Json_response_errors
             (((error TOO_LONG) (error_type ())
               (details "this is too long (max: 1000)") (fields (ban_message))))))) |}];
      return ())
;;

let%expect_test "HTTP error" =
  with_cassette "ban__wrong_subreddit" ~f:(fun connection ->
      let%bind () =
        match%bind
          Connection.call
            connection
            (Endpoint.add_relationship
               ~relationship:Banned
               ~username:(Username.of_string "spez")
               ~duration:Permanent
               ~subreddit:(Subreddit_name.of_string "thirdrealm")
               ())
        with
        | Error (Endpoint_error (Http_error _)) ->
          print_s [%message "HTTP error"];
          return ()
        | Ok () | Error _ -> raise_s [%message "Expected HTTP error"]
      in
      [%expect {| "HTTP error" |}];
      return ())
;;

let%expect_test "Bad request" =
  with_cassette "user_trophies__nonexistent_user" ~f:(fun connection ->
      let%bind () =
        match%bind
          Connection.call
            connection
            (Endpoint.user_trophies
               ~username:(Username.of_string "thisusershouldnotexist")
               ())
        with
        | Ok _ -> raise_s [%message "Expected error"]
        | Error error ->
          print_s [%message "" (error : Endpoint.Error.t Connection.Error.t)];
          return ()
      in
      [%expect
        {|
        (error
         (Endpoint_error
          (Json_response_errors
           (((error USER_DOESNT_EXIST) (error_type ())
             (details "that user doesn't exist") (fields (id))))))) |}];
      return ())
;;
