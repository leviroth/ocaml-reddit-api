open! Core
open! Async
open Reddit_api_kernel

module Transience = struct
  type ('response, 'error) t =
    | Transient_error
    | Non_transient of ('response, 'error) Result.t

  let map_error t ~f =
    match t with
    | (Transient_error | Non_transient (Ok _)) as v -> v
    | Non_transient (Error error) -> Non_transient (Error (f error))
  ;;
end

module Non_transient_error = struct
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
        Non_transient (Error (Token_request_rejected { response; body }))
      | Other_http_error { response; body } ->
        (match Cohttp.Response.status response with
        | #Cohttp.Code.server_error_status -> Transient_error
        | _ -> Non_transient (Error (Other_http_error { response; body })))
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
      | Json_response_errors errors -> Non_transient (Error (Json_response_errors errors))
      | Http_error { response; body } ->
        (match Cohttp.Response.status response with
        | #Cohttp.Code.server_error_status -> Transient_error
        | _ -> Non_transient (Error (Http_error { response; body })))
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
    | Ok result -> Non_transient (Ok result)
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

let on_non_transient_response t =
  match t.state with
  | Working_normally -> ()
  | Waiting_for_issue_resolution { finished } ->
    Ivar.fill finished ();
    t.state <- Working_normally
;;

let check_server t =
  Deferred.repeat_until_finished () (fun () ->
      let%bind response = get_read_only_page t in
      match Non_transient_error.classify_response response, t.state with
      | Non_transient _, Working_normally -> return (`Finished ())
      | Non_transient _, Waiting_for_issue_resolution { finished } ->
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

let rec call t request =
  match t.state with
  | Waiting_for_issue_resolution { finished } ->
    let%bind () = Ivar.read finished in
    call t request
  | Working_normally ->
    let%bind response = Connection.call t.connection request in
    (match Non_transient_error.classify_response response with
    | Non_transient response ->
      on_non_transient_response t;
      return response
    | Transient_error ->
      on_transient_error t;
      call t request)
;;
