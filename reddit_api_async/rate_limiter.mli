open! Core
open! Async

type t [@@deriving sexp_of]

val of_state_machine
  :  Reddit_api_kernel.Rate_limiter_state_machine.t
  -> Time_source.t
  -> t

val permit_request : t -> unit Deferred.t
val notify_response : t -> Cohttp.Response.t -> unit
val is_ready : t -> bool
val wait_until_ready : t -> unit Deferred.t
