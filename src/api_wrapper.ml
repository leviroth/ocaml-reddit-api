open! Core
open Async

let to_body_json (response, body) =
  match Cohttp.Response.status response with
  | #Cohttp.Code.success_status ->
    let%bind body_s = Cohttp_async.Body.to_string body in
    Yojson.Safe.from_string body_s |> Ok |> return
  | _ -> return (Error (response, body))
;;

let to_body_json' = Deferred.bind ~f:to_body_json
