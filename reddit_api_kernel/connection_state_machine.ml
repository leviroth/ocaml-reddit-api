open! Core

module What_to_do = struct
  type t =
    | Get_access_token
    | Wait_for_access_token_response
    | Send_now of { access_token : string }
    | Send_after of Time_ns.t
    | Check_after_receiving_response
end

module Access_token = struct
  type t =
    { token : string
    ; expiration : Time_ns.t
    }
  [@@deriving sexp_of]

  let is_almost_expired { expiration; _ } ~now =
    let time_with_padding = Time_ns.add now (Time_ns.Span.of_int_sec 10) in
    Time_ns.( <= ) expiration time_with_padding
  ;;
end

module Access_token_state = struct
  type t =
    | No_outstanding_request of Access_token.t option
    | Outstanding_request
  [@@deriving sexp_of]
end

type t =
  { rate_limiter : Rate_limiter_state_machine.t
  ; access_token : Access_token_state.t
  ; credentials : Credentials.t
  }
[@@deriving sexp_of]

let create ~credentials ~rate_limiter =
  { rate_limiter; access_token = No_outstanding_request None; credentials }
;;

let send_request t ~now : t * What_to_do.t =
  match t.access_token with
  | Outstanding_request -> t, Wait_for_access_token_response
  | No_outstanding_request None ->
    { t with access_token = Outstanding_request }, Get_access_token
  | No_outstanding_request (Some access_token) ->
    (match Access_token.is_almost_expired access_token ~now with
     | true -> { t with access_token = Outstanding_request }, Get_access_token
     | false ->
       let new_rate_limiter, when_to_send =
         Rate_limiter_state_machine.send_request t.rate_limiter ~now
       in
       let what_to_do : What_to_do.t =
         match when_to_send with
         | Now -> Send_now { access_token = access_token.token }
         | After time -> Send_after time
         | Check_after_receiving_response -> Check_after_receiving_response
       in
       { t with rate_limiter = new_rate_limiter }, what_to_do)
;;

let got_access_token t access_token =
  { t with access_token = No_outstanding_request (Some access_token) }
;;

let received_response t response =
  { t with
    rate_limiter = Rate_limiter_state_machine.received_response t.rate_limiter response
  }
;;

let credentials t = t.credentials
