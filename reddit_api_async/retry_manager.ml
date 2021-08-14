open! Core
open! Async
open Reddit_api_kernel

module Non_transient_error = struct
  type t =
    | Http_error of
        { response : Cohttp.Response.t
        ; body : Cohttp.Body.t
        }
    | Json_response_errors of Api.Json_response_error.t list
  [@@deriving sexp_of]
end

let is_good (result : (_, Api.Api_error.t Connection.Error.t) Result.t) =
  match result with
  | Ok result -> `Yes (Ok result)
  | Error
      ( Access_token_error (Cohttp_raised _ | Json_parsing_error _)
      | Endpoint_error (Cohttp_raised _ | Json_parsing_error _) ) -> `No
  | Error (Endpoint_error (Json_response_errors errors)) ->
    `Yes (Error (Non_transient_error.Json_response_errors errors))
  | Error (Endpoint_error (Http_error { response; body })) ->
    (match Cohttp.Response.status response with
    | #Cohttp.Code.server_error_status -> `No
    | _ -> `Yes (Error (Http_error { response; body })))
;;

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

let get_read_only_page t = Connection.call t.connection (Api.me ())

let on_good_response t =
  match t.state with
  | Working_normally -> ()
  | Waiting_for_issue_resolution { finished } ->
    Ivar.fill finished ();
    t.state <- Working_normally
;;

let check_server t =
  Deferred.repeat_until_finished () (fun () ->
      let%bind response = get_read_only_page t in
      match is_good response, t.state with
      | `Yes _, Working_normally -> return (`Finished ())
      | `Yes _, Waiting_for_issue_resolution { finished } ->
        Ivar.fill finished ();
        t.state <- Working_normally;
        return (`Finished ())
      | `No, Working_normally ->
        t.state <- Waiting_for_issue_resolution { finished = Ivar.create () };
        return (`Repeat ())
      | `No, Waiting_for_issue_resolution _ ->
        let%bind () = Clock_ns.after Time_ns.Span.minute in
        return (`Repeat ()))
;;

let on_bad_response t =
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
    (match is_good response with
    | `Yes response ->
      on_good_response t;
      return response
    | `No ->
      on_bad_response t;
      call t request)
;;
