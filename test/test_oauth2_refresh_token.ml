open! Core
open! Async
open! Import

let with_cassette cassette_name ~f =
  let credentials =
    match Sys.getenv "CREDENTIALS_REFRESH_TOKEN" with
    | Some credential_path ->
      Sexp.load_sexp_conv_exn credential_path [%of_sexp: Connection.Credentials.t]
    | None ->
      Refresh_token
        { client_id = "TEST_CLIENT_ID"
        ; client_secret = Some "TEST_CLIENT_SECRET"
        ; refresh_token = "TEST_REFRESH_TOKEN"
        }
  in
  let filename = "cassettes" ^/ sprintf "%s.sexp" cassette_name in
  Connection.For_testing.with_cassette filename ~credentials ~f
;;

let%expect_test "oauth2_refresh_token" =
  with_cassette "oauth2_refresh_token" ~f:(fun connection ->
      let%bind link = get_link_exn connection "odlsl2" in
      print_s
        [%sexp
          { id : Thing.Link.Id.t = Thing.Link.id link
          ; title : string = Thing.Link.title link
          }];
      [%expect {| ((id odlsl2) (title test)) |}];
      return ())
;;
