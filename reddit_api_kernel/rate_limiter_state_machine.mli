open! Core

module When_to_send : sig
  type t =
    | Now
    | Check_after_receiving_response
    | After of Time_ns.t
end

type t [@@deriving sexp_of]

(** {1 Constructors} *)

val by_headers : t
val with_minimum_delay : delay:Time_ns.Span.t -> t
val combine : t list -> t

(** {1 Events} *)

(** [sent_request_unchecked] should called immediately after sending a request.
    It is the caller's responsibility to first call {!wait_until}. *)
val sent_request_unchecked : t -> now:Time_ns.t -> t

val received_response : t -> Cohttp.Response.t -> t

(** {1 Accessors} *)

val wait_until : t -> When_to_send.t
