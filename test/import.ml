open! Core
open! Async
open Ocaml_reddit

let with_cassette cassette_name ~f =
  let credential_path = Sys.getenv_exn "CREDENTIALS" in
  let credentials =
    Sexp.load_sexp_conv_exn credential_path [%of_sexp: Connection.Credentials.t]
  in
  let filename = "cassettes" ^/ sprintf "%s.sexp" cassette_name in
  Connection.For_testing.Cassette.with_t filename ~credentials ~f:(fun client_wrapper ->
      let time_source = Time_source.create ~now:Time_ns.epoch () in
      let connection =
        Connection.For_testing.create
          client_wrapper
          credentials
          ~time_source:(Time_source.read_only time_source)
      in
      f connection)
;;
