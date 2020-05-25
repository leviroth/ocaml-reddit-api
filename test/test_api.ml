open! Core
open! Async
open Ocaml_reddit

let make_response ?headers body =
  let header =
    match headers with
    | None -> Cohttp.Response.make ()
    | Some headers -> Cohttp.Response.make ~headers:(Cohttp.Header.of_list headers) ()
  in
  header, Cohttp_async.Body.of_string body
;;

let server_stub responses : (module Connection.For_testing.Cohttp_client_wrapper) =
  let responses = ref responses in
  let return_reponse_and_advance () =
    match Sequence.next !responses with
    | Some (hd, rest) ->
      responses := rest;
      return hd
    | None -> raise_s [%message "Exhausted responses in test"]
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
  end)
;;

let create responses =
  let time_source = Time_source.create ~now:Time_ns.epoch () in
  Connection.For_testing.create
    (server_stub responses)
    { client_id = ""; client_secret = ""; password = ""; username = "" }
    ~time_source:(Time_source.read_only time_source)
;;

let basic_responses_with_repeating_tail =
  [ Sequence.singleton
      (make_response {|{"access_token": "<FAKE_ACCESS_TOKEN>", "expires_in": 600}|})
  ; Sequence.repeat
      (make_response
         ""
         ~headers:[ "X-Ratelimit-Remaining", "600.0"; "X-Ratelimit-Reset", "600" ])
  ]
  |> Sequence.of_list
  |> Sequence.concat
;;

let%expect_test "info" =
  let connection = create basic_responses_with_repeating_tail in
  let%bind _response =
    Api.Raw.info (Url (Uri.of_string "http://example.com")) connection
  in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers ((authorization "Basic Og==")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (get
     (uri
      ((scheme (https)) (host (oauth.reddit.com)) (path /api/info)
       (query ((raw_json (1)) (url (http://example.com))))))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))) |}]
;;

let%expect_test "me" =
  let connection = create basic_responses_with_repeating_tail in
  let%bind _response = Api.Raw.me connection in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers ((authorization "Basic Og==")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (get
     (uri
      ((scheme (https)) (host (oauth.reddit.com)) (path /api/v1/me)
       (query ((raw_json (1))))))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))) |}]
;;

let%expect_test "karma" =
  let connection = create basic_responses_with_repeating_tail in
  let%bind _response = Api.Raw.karma connection in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers ((authorization "Basic Og==")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (get
     (uri
      ((scheme (https)) (host (oauth.reddit.com)) (path /api/v1/me/karma)
       (query ((raw_json (1))))))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))) |}]
;;

let%expect_test "trophies" =
  let connection = create basic_responses_with_repeating_tail in
  let%bind _response = Api.Raw.trophies connection in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers ((authorization "Basic Og==")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (get
     (uri
      ((scheme (https)) (host (oauth.reddit.com)) (path /api/v1/me/trophies)
       (query ((raw_json (1))))))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))) |}]
;;

let%expect_test "friends" =
  let connection = create basic_responses_with_repeating_tail in
  let%bind _response = Api.Raw.friends connection in
  let%bind () =
    [%expect
      {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers ((authorization "Basic Og==")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (post_from
     (uri ((scheme (https)) (host (oauth.reddit.com)) (path /api/friends)))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))
     (params ((raw_json (1))))) |}]
  in
  let%bind _response =
    Api.Raw.friends ~pagination:(After (Listing.Page_id.of_string "t3_1jklj")) connection
  in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (oauth.reddit.com)) (path /api/friends)))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))
     (params ((raw_json (1)) (after (t3_1jklj))))) |}]
;;

let%expect_test "blocked" =
  let connection = create basic_responses_with_repeating_tail in
  let%bind _response = Api.Raw.blocked connection in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers ((authorization "Basic Og==")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (post_from
     (uri ((scheme (https)) (host (oauth.reddit.com)) (path /api/blocked)))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))
     (params ((raw_json (1))))) |}]
;;

let%expect_test "messaging" =
  let connection = create basic_responses_with_repeating_tail in
  let%bind _response = Api.Raw.messaging connection in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers ((authorization "Basic Og==")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (post_from
     (uri ((scheme (https)) (host (oauth.reddit.com)) (path /api/messaging)))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))
     (params ((raw_json (1))))) |}]
;;

let%expect_test "trusted" =
  let connection = create basic_responses_with_repeating_tail in
  let%bind _response = Api.Raw.trusted connection in
  [%expect
    {|
    (post_from
     (uri ((scheme (https)) (host (www.reddit.com)) (path /api/v1/access_token)))
     (headers ((authorization "Basic Og==")))
     (params ((grant_type (password)) (username ("")) (password ("")))))
    (post_from
     (uri ((scheme (https)) (host (oauth.reddit.com)) (path /api/trusted)))
     (headers ((authorization "bearer <FAKE_ACCESS_TOKEN>")))
     (params ((raw_json (1))))) |}]
;;
