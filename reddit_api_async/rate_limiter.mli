open! Core
open! Async

type t [@@deriving sexp_of]

val by_headers : time_source:Time_source.t -> t
val with_minimum_delay : time_source:Time_source.t -> delay:Time_ns.Span.t -> t
val combine : t list -> t
val permit_request : t -> unit Deferred.t
val notify_response : t -> Cohttp.Response.t -> unit
val is_ready : t -> bool
val wait_until_ready : t -> unit Deferred.t
