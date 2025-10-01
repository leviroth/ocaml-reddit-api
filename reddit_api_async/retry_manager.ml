open! Core
open! Async
open! Import
open Reddit_api_kernel

module Transience = struct
  type ('response, 'error) t =
    | Transient_error
    | Permanent of ('response, 'error) Result.t

  let map_error t ~f =
    match t with
    | (Transient_error | Permanent (Ok _)) as v -> v
    | Permanent (Error error) -> Permanent (Error (f error))
  ;;
end

module Permanent_error = struct
  module Access_token_request_error = struct
    let classify_error (error : Connection.Access_token_request_error.t)
      : ('a, Connection.Access_token_request_error.t) Transience.t
      =
      match error with
      | Cohttp_raised _ | Json_parsing_error _ -> Transient_error
      | Token_request_rejected { response; body } ->
        Permanent (Error (Token_request_rejected { response; body }))
      | Other_http_error { response; body } ->
        (match Cohttp.Response.status response with
         | #Cohttp.Code.server_error_status -> Transient_error
         | _ -> Permanent (Error (Other_http_error { response; body })))
    ;;
  end

  module Endpoint_error = struct
    let classify_error (error : Endpoint.Error.t) : ('a, Endpoint.Error.t) Transience.t =
      match error with
      | Cohttp_raised _ | Json_parsing_error _ -> Transient_error
      | Json_response_errors errors -> Permanent (Error (Json_response_errors errors))
      | Http_error { response; body } ->
        (match Cohttp.Response.status response with
         | #Cohttp.Code.server_error_status -> Transient_error
         | _ -> Permanent (Error (Http_error { response; body })))
    ;;
  end

  let classify_response (result : (_, Endpoint.Error.t Connection.Error.t) Result.t)
    : (_, _) Transience.t
    =
    match result with
    | Ok result -> Permanent (Ok result)
    | Error (Access_token_request_error error) ->
      Access_token_request_error.classify_error error
      |> Transience.map_error ~f:(fun error ->
        Connection.Error.Access_token_request_error error)
    | Error (Endpoint_error error) ->
      Endpoint_error.classify_error error
      |> Transience.map_error ~f:(fun error -> Connection.Error.Endpoint_error error)
  ;;
end

type state =
  | Working_normally
  | Waiting_for_issue_resolution of { finished : unit Ivar.t }

type t =
  { mutable state : state
  ; connection : Connection.t
  }

let create connection = { connection; state = Working_normally }

let yield_until_reddit_available t =
  match t.state with
  | Working_normally -> return ()
  | Waiting_for_issue_resolution { finished } -> Ivar.read finished
;;

let get_read_only_page t = Connection.call t.connection Endpoint.me

let on_permanent_response t =
  match t.state with
  | Working_normally -> ()
  | Waiting_for_issue_resolution { finished } ->
    Ivar.fill_exn finished ();
    t.state <- Working_normally
;;

let check_server t =
  Deferred.repeat_until_finished () (fun () ->
    let%bind response = get_read_only_page t in
    match Permanent_error.classify_response response, t.state with
    | Permanent _, Working_normally -> return (`Finished ())
    | Permanent _, Waiting_for_issue_resolution { finished } ->
      Ivar.fill_exn finished ();
      t.state <- Working_normally;
      return (`Finished ())
    | Transient_error, Working_normally ->
      t.state <- Waiting_for_issue_resolution { finished = Ivar.create () };
      return (`Repeat ())
    | Transient_error, Waiting_for_issue_resolution _ ->
      let%bind () = Clock_ns.after Time_ns.Span.minute in
      return (`Repeat ()))
;;

let on_transient_error t =
  match t.state with
  | Waiting_for_issue_resolution _ -> return ()
  | Working_normally ->
    t.state <- Waiting_for_issue_resolution { finished = Ivar.create () };
    check_server t
;;

let rec call t endpoint =
  match t.state with
  | Waiting_for_issue_resolution { finished } ->
    let%bind () = Ivar.read finished in
    call t endpoint
  | Working_normally ->
    let%bind response = Connection.call t.connection endpoint in
    (match Permanent_error.classify_response response with
     | Permanent response ->
       on_permanent_response t;
       return response
     | Transient_error ->
       let%bind () = on_transient_error t in
       let request = endpoint.request in
       (match request with
        | Get _ ->
          [%log.error
            log
              "Transient error"
              (request : Endpoint.Request.t)
              (response : (_, Endpoint.Error.t Connection.Error.t) Result.t)];
          call t endpoint
        | Post_form _ ->
          [%log.error
            log
              "Got error for POST request; treating as permanent"
              (request : Endpoint.Request.t)
              (response : (_, Endpoint.Error.t Connection.Error.t) Result.t)];
          return response))
;;
