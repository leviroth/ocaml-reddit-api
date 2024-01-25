open! Core

module When_to_send : sig
  type t =
    | Now
    | Check_after_receiving_response
    | After of Time_ns.t
end

type t [@@deriving sexp_of]

val by_headers : unit -> t
val with_minimum_delay : delay:Time_ns.Span.t -> t
val combine : t list -> t
val wait_until : t -> When_to_send.t
val sent_request_unchecked : t -> now:Time_ns.t -> unit
val received_response : t -> Cohttp.Response.t -> unit
