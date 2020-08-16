open! Core
open! Async
include Reddit_api

let with_cassette cassette_name ~f =
  let credential_path = Sys.getenv_exn "CREDENTIALS" in
  let credentials =
    Sexp.load_sexp_conv_exn credential_path [%of_sexp: Connection.Credentials.t]
  in
  let filename = "cassettes" ^/ sprintf "%s.sexp" cassette_name in
  Connection.For_testing.with_cassette filename ~credentials ~f
;;
