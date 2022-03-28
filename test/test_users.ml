open! Core
open! Async
open! Import

let%expect_test "user_upvoted" =
  with_cassette "user_upvoted" ~f:(fun connection ->
      match%bind
        Connection.call_exn
          connection
          (Endpoint.user_upvoted () ~username:(Username.of_string "spez"))
      with
      | `Private -> return ()
      | `Listing _ -> failwith "Got listing unexpectedly")
;;
