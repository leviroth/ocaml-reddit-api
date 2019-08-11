open! Core
open! Async
open Ocaml_reddit

module Printing_client_wrapper : Connection.For_testing.Cohttp_client_wrapper = struct
  let get uri ~headers =
    Core.print_s [%message "get" (uri : Uri_sexp.t) (headers : Cohttp.Header.t)];
    Out_channel.flush stdout;
    return
      ( Cohttp.Response.make ()
      , Cohttp_async.Body.of_string {|{"access_token": "foo", "expires_in": 600}|} )
  ;;

  let post_form uri ~headers ~params =
    Core.print_s
      [%message
        "post_from"
          (uri : Uri_sexp.t)
          (headers : Cohttp.Header.t)
          (params : (string * string list) list)];
    Out_channel.flush stdout;
    return
      ( Cohttp.Response.make ()
      , Cohttp_async.Body.of_string {|{"access_token": "foo", "expires_in": 600}|} )
  ;;
end

let%expect_test "info" =
  let time_source = Time_source.create ~now:Time_ns.epoch () in
  let connection =
    Connection.For_testing.create
      (module Printing_client_wrapper)
      { client_id = ""; client_secret = ""; password = ""; username = "" }
      ~time_source:(Time_source.read_only time_source)
  in
  let%bind _response = Api.info (Url (Uri.of_string "http://example.com")) connection in
  let%bind () =
    Time_source.advance_by_alarms time_source ~to_:Time_ns.(add epoch Span.minute)
  in
  [%expect]
;;
