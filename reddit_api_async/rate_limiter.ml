open! Core
open! Async
module Synchronous_rate_limiter = Reddit_api_kernel.Synchronous_rate_limiter

type t =
  { mutable state : Synchronous_rate_limiter.t
  ; response_received : (unit, read_write) Bvar.t
  ; time_source : (Time_source.t[@sexp.opaque])
  }
[@@deriving sexp_of]

let of_synchronous state time_source =
  let response_received = Bvar.create () in
  { state; response_received; time_source }
;;

let is_ready t =
  let now = Time_source.now t.time_source in
  match Synchronous_rate_limiter.wait_until t.state with
  | Now -> true
  | Check_after_receiving_response -> false
  | After time -> Time_ns.( >= ) now time
;;

let wait_until_ready t =
  Deferred.repeat_until_finished () (fun () ->
      match Synchronous_rate_limiter.wait_until t.state with
      | Now -> return (`Finished ())
      | After time ->
        (match Time_ns.( >= ) (Time_source.now t.time_source) time with
        | true -> return (`Finished ())
        | false ->
          let%bind () = Time_source.at t.time_source time in
          return (`Repeat ()))
      | Check_after_receiving_response ->
        let%bind () = Bvar.wait t.response_received in
        return (`Repeat ()))
;;

let permit_request t =
  Deferred.repeat_until_finished () (fun () ->
      let%bind () = wait_until_ready t in
      match is_ready t with
      | false -> return (`Repeat ())
      | true ->
        t.state
          <- Synchronous_rate_limiter.sent_request_unchecked
               t.state
               ~now:(Time_source.now t.time_source);
        return (`Finished ()))
;;

let notify_response t response =
  t.state <- Synchronous_rate_limiter.received_response t.state response;
  Bvar.broadcast t.response_received ()
;;
