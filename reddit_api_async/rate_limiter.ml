open! Core
open! Async
module Rate_limiter_state_machine = Reddit_api_kernel.Rate_limiter_state_machine

type t =
  { mutable state : Rate_limiter_state_machine.t
  ; response_received : (unit, read_write) Bvar.t
  ; time_source : (Time_source.t[@sexp.opaque])
  }
[@@deriving sexp_of]

let of_state_machine state time_source =
  let response_received = Bvar.create () in
  { state; response_received; time_source }
;;

let is_ready t =
  let now = Time_source.now t.time_source in
  match Rate_limiter_state_machine.wait_until t.state with
  | Now -> true
  | Check_after_receiving_response -> false
  | After time -> Time_ns.( >= ) now time
;;

let wait_until_ready t =
  Deferred.repeat_until_finished () (fun () ->
      match Rate_limiter_state_machine.wait_until t.state with
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
          <- Rate_limiter_state_machine.sent_request_unchecked
               t.state
               ~now:(Time_source.now t.time_source);
        return (`Finished ()))
;;

let notify_response t response =
  t.state <- Rate_limiter_state_machine.received_response t.state response;
  Bvar.broadcast t.response_received ()
;;
