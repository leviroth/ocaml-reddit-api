open! Core

module What_to_do : sig
  type t =
    | Get_access_token
    | Wait_for_access_token_response
    | Send_now of { access_token : string }
    | Send_after of Time_ns.t
    | Check_after_receiving_response
end

type t [@@deriving sexp_of]

(** {1 Constructors} *)
val create : credentials:Credentials.t -> rate_limiter:Rate_limiter_state_machine.t -> t

(** {1 Events} *)

(* TODO: Move *)
module Access_token : sig
  type t =
    { token : string
    ; expiration : Time_ns.t
    }
end

val send_request : t -> now:Time_ns.t -> t * What_to_do.t
val got_access_token : t -> Access_token.t -> t
val received_response : t -> Cohttp.Response.t -> t
val credentials : t -> Credentials.t
