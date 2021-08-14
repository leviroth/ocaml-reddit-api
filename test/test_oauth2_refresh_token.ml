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

let%expect_test "oauth2_refresh_token_insufficient_scope" =
  let%bind () =
    with_cassette "oauth2_refresh_token_insufficient_scope" ~f:(fun connection ->
        Expect_test_helpers_async.show_raise_async (fun () ->
            let%bind link = get_link_exn connection "odlsl2" in
            print_s
              [%sexp
                { id : Thing.Link.Id.t = Thing.Link.id link
                ; title : string = Thing.Link.title link
                }];
            return ()))
  in
  [%expect
    {|
    (raised (
      Http_error
      (response (
        (encoding (Fixed 38))
        (headers (
          (accept-ranges                 bytes)
          (access-control-allow-origin   *)
          (access-control-expose-headers X-Moose)
          (cache-control  "max-age=0, must-revalidate")
          (connection     keep-alive)
          (content-length 38)
          (content-type   "application/json; charset=UTF-8")
          (date           "Sun, 25 Jul 2021 13:48:26 GMT")
          (server         snooserv)
          (set-cookie
           "edgebucket=DOY2vtwecNrBUXjM3E; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")
          (set-cookie
           "csv=1; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")
          (set-cookie
           "session_tracker=GiIdHGp9KRBNycfn5d.0.1627220906977.Z0FBQUFBQmdfV3VxLUtmQ04tRzlVQlQ4dVVGNFVRWGx0Rm1qX2tqMzlYMERCTWZCWTR6bnV1Wk1na3ladXJMNmo5a1BxQnU4VHZ0aGNwT0RWbEE1NFcwWDdZV3VLMWx5Qlp6a3hJSlhfdXYyOUVMUjlUNlY0N3B3d3JaazBRbmhjS0NncVM1dERtOXI; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sun, 25-Jul-2021 15:48:26 GMT; secure; SameSite=None; Secure")
          (set-cookie
           "loid=0000000000dijrnrio.2.1627220906977.Z0FBQUFBQmdfV3VxY0tER3dTQXZyakFWdnpuSjJFcW1jNmhLeG9CWWNUcHpXMXVTMlpNNXA5YWtYTjJoLWIwNWRDcU5tNFluTkF1cDZRZFZaRElPYzZNdjN6c2VFS01RbkFZMGpsa3AyLTZidElfRlJ1TkpKN2Nzdi1hUG8ycnIwUUxpUlB5eDRCZXg; Domain=reddit.com; Max-Age=63071999; Path=/; expires=Tue, 25-Jul-2023 13:48:26 GMT; secure; SameSite=None; Secure")
          (strict-transport-security
           "max-age=15552000; includeSubDomains; preload")
          (via "1.1 varnish")
          (www-authenticate
           "Bearer realm=\"reddit\", error=\"insufficient_scope\"")
          (x-clacks-overhead      "GNU Terry Pratchett")
          (x-content-type-options nosniff)
          (x-frame-options        SAMEORIGIN)
          (x-moose                majestic)
          (x-ua-compatible        IE=edge)
          (x-xss-protection       "1; mode=block")))
        (version HTTP_1_1)
        (status  Forbidden)
        (flush   false)))
      (body (String "{\"message\": \"Forbidden\", \"error\": 403}")))) |}];
  return ()
;;