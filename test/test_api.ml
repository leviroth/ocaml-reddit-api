open! Core
open! Async
open Ocaml_reddit

module Harness = struct
  type t =
    { connection : Connection.t
    ; time_source : Time_source.Read_write.t
    }
  [@@deriving sexp_of]

  let make_response ?(header = Cohttp.Response.make ()) body =
    header, Cohttp_async.Body.of_string body
  ;;

  let server_stub responses =
    let responses =
      ref
        (make_response {|{"access_token": "foo", "expires_in": 600}|}
        :: List.map responses ~f:make_response)
    in
    let return_reponse_and_advance () =
      let result = List.hd_exn !responses in
      responses := List.tl_exn !responses;
      return result
    in
    (module struct
      let get uri ~headers =
        Core.print_s [%message "get" (uri : Uri_sexp.t) (headers : Cohttp.Header.t)];
        Out_channel.flush stdout;
        return_reponse_and_advance ()
      ;;

      let post_form uri ~headers ~params =
        Core.print_s
          [%message
            "post_from"
              (uri : Uri_sexp.t)
              (headers : Cohttp.Header.t)
              (params : (string * string list) list)];
        Out_channel.flush stdout;
        return_reponse_and_advance ()
      ;;
    end : Connection.For_testing.Cohttp_client_wrapper)
  ;;

  let create responses =
    let time_source = Time_source.create ~now:Time_ns.epoch () in
    let connection =
      Connection.For_testing.create
        (server_stub responses)
        { client_id = ""; client_secret = ""; password = ""; username = "" }
        ~time_source:(Time_source.read_only time_source)
    in
    { time_source; connection }
  ;;
end

let%expect_test "info" =
  let ({ connection; _ } : Harness.t) = Harness.create [ "" ] in
  let%bind _response = Api.info (Url (Uri.of_string "http://example.com")) connection in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers
      ((authorization "Basic Og==")
       (user-agent "OCaml Api Wrapper/0.1 - developed by /u/L72_Elite_kraken")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (get
     (uri
      ((scheme (https)) (host (oauth.reddit.com)) (path /api/info)
       (query ((raw_json (1)) (url (http://example.com))))))
     (headers
      ((authorization "bearer foo")
       (user-agent "OCaml Api Wrapper/0.1 - developed by /u/L72_Elite_kraken")))) |}]
;;
