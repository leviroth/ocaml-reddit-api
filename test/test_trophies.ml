open! Core
open! Async
open Ocaml_reddit

let%expect_test _ =
  let credential_path = Sys.getenv_exn "CREDENTIALS" in
  let credentials =
    Sexp.load_sexp_conv_exn credential_path [%of_sexp: Connection.Credentials.t]
  in
  Connection.For_testing.Cassette.with_t
    "data/trophies.sexp"
    ~credentials
    ~f:(fun client_wrapper ->
      let time_source = Time_source.create ~now:Time_ns.epoch () in
      let connection =
        Connection.For_testing.create
          client_wrapper
          credentials
          ~time_source:(Time_source.read_only time_source)
      in
      let%bind trophies = Api.Exn.trophies connection in
      print_s [%message "" (trophies : Thing.Award.t list)];
      [%expect
        {|
        (trophies
         (((award_id Null) (description Null)
           (icon_40 (String https://www.redditstatic.com/awards2/3_year_club-40.png))
           (icon_70 (String https://www.redditstatic.com/awards2/3_year_club-70.png))
           (id Null) (name (String "Three-Year Club")) (url Null))
          ((award_id (String o)) (description Null)
           (icon_40
            (String https://www.redditstatic.com/awards2/verified_email-40.png))
           (icon_70
            (String https://www.redditstatic.com/awards2/verified_email-70.png))
           (id (String 1qr5eq)) (name (String "Verified Email")) (url Null)))) |}])
;;
