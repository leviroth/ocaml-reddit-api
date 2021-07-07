open! Core
open! Async
open! Import

let with_cassette cassette_name ~f ~is_confidential =
  let credentials =
    match Sys.getenv "CREDENTIALS_USERLESS" with
    | Some credential_path ->
      Sexp.load_sexp_conv_exn credential_path [%of_sexp: Connection.Credentials.t]
    | None ->
      (match is_confidential with
      | true ->
        Userless_confidential
          { client_id = "TEST_CLIENT_ID"; client_secret = "TEST_CLIENT_SECRET" }
      | false ->
        Userless_public
          { client_id = "TEST_CLIENT_ID"; device_id = Some "TEST_DEVICE_ID" })
  in
  let filename = "cassettes" ^/ sprintf "%s.sexp" cassette_name in
  Connection.For_testing.with_cassette filename ~credentials ~f
;;

let get_link_exn connection id =
  let%bind link =
    Connection.call_exn
      connection
      (Api.info (Id [ `Link (Thing.Link.Id.of_string id) ]) ())
    >>| List.hd_exn
  in
  return
    (match link with
    | `Link link -> link
    | _ -> raise_s [%message "Unexpected response item"])
;;

let%expect_test "userless_confidential" =
  with_cassette
    "userless_confidential"
    ~f:(fun connection ->
      let%bind link = get_link_exn connection "odlsl2" in
      print_s
        [%sexp
          { id : Thing.Link.Id.t = Thing.Link.id link
          ; title : string = Thing.Link.title link
          }];
      [%expect {| ((id odlsl2) (title test)) |}];
      return ())
    ~is_confidential:true
;;

let%expect_test "userless_public" =
  with_cassette
    "userless_public"
    ~f:(fun connection ->
      let%bind link = get_link_exn connection "odlsl2" in
      print_s
        [%sexp
          { id : Thing.Link.Id.t = Thing.Link.id link
          ; title : string = Thing.Link.title link
          }];
      [%expect {| ((id odlsl2) (title test)) |}];
      return ())
    ~is_confidential:false
;;
