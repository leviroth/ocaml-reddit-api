open! Core
open! Async
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
    type t =
      | Token_request_rejected of
          { response : Cohttp.Response.t
          ; body : Cohttp.Body.t
          }
      | Other_http_error of
          { response : Cohttp.Response.t
          ; body : Cohttp.Body.t
          }
    [@@deriving sexp_of]

    let classify_error (error : Connection.Access_token_request_error.t)
        : ('a, t) Transience.t
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
    type t =
      | Http_error of
          { response : Cohttp.Response.t
          ; body : Cohttp.Body.t
          }
      | Json_response_errors of Endpoint.Json_response_error.t list
    [@@deriving sexp_of]

    let classify_error (error : Endpoint.Error.t) : ('a, t) Transience.t =
      match error with
      | Cohttp_raised _ | Json_parsing_error _ -> Transient_error
      | Json_response_errors errors -> Permanent (Error (Json_response_errors errors))
      | Http_error { response; body } ->
        (match Cohttp.Response.status response with
        | #Cohttp.Code.server_error_status -> Transient_error
        | _ -> Permanent (Error (Http_error { response; body })))
    ;;
  end

  type t =
    | Access_token_request_error of Access_token_request_error.t
    | Endpoint_error of Endpoint_error.t
  [@@deriving sexp_of]

  let classify_response (result : (_, Endpoint.Error.t Connection.Error.t) Result.t)
      : (_, _) Transience.t
    =
    match result with
    | Ok result -> Permanent (Ok result)
    | Error (Access_token_request_error error) ->
      Access_token_request_error.classify_error error
      |> Transience.map_error ~f:(fun error -> Access_token_request_error error)
    | Error (Endpoint_error error) ->
      Endpoint_error.classify_error error
      |> Transience.map_error ~f:(fun error -> Endpoint_error error)
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
    Ivar.fill finished ();
    t.state <- Working_normally
;;

let check_server t =
  Deferred.repeat_until_finished () (fun () ->
      let%bind response = get_read_only_page t in
      match Permanent_error.classify_response response, t.state with
      | Permanent _, Working_normally -> return (`Finished ())
      | Permanent _, Waiting_for_issue_resolution { finished } ->
        Ivar.fill finished ();
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
  | Waiting_for_issue_resolution _ -> ()
  | Working_normally ->
    t.state <- Waiting_for_issue_resolution { finished = Ivar.create () };
    don't_wait_for (check_server t)
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
      let request = endpoint.request in
      Log.Global.error_s
        [%message
          "Transient error"
            (request : Endpoint.Request.t)
            (response : (_, Endpoint.Error.t Connection.Error.t) Result.t)];
      on_transient_error t;
      call t endpoint)
;;
