open! Core
open! Async
include Reddit_api_async
module Time_ns = Time_ns_unix

let () =
  Time_ns_unix.set_sexp_zone Time_ns.Zone.utc;
  Log.set_level Reddit_api_async.log `Debug;
  Log.set_output Reddit_api_async.log [ Log.For_testing.create_output ~map_output:Fn.id ]
;;

let with_cassette cassette_name ~f =
  let credentials =
    match Sys.getenv "CREDENTIALS" with
    | Some credential_path ->
      Sexp.load_sexp_conv_exn credential_path [%of_sexp: Connection.Credentials.t]
    | None ->
      Password
        { username = "TEST_USERNAME"
        ; password = "TEST_PASSWORD"
        ; client_id = "TEST_CLIENT_ID"
        ; client_secret = "TEST_CLIENT_SECRET"
        }
  in
  let filename = "cassettes" ^/ sprintf "%s.sexp" cassette_name in
  Connection.For_testing.with_cassette filename ~credentials ~f
;;

let get_link_exn connection id =
  let%bind link =
    Connection.call_exn
      connection
      (Endpoint.info (Id [ `Link (Thing.Link.Id.of_string id) ]))
    >>| List.hd_exn
  in
  return
    (match link with
    | `Link link -> link
    | _ -> raise_s [%message "Unexpected response item"])
;;
