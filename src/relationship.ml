open! Core

module Duration : sig
  type t

  val permanent : t
  val days_exn : int -> t
  val parameter_of_t : t -> string option
end = struct
  type t =
    | Permanent
    | Days of int

  let permanent = Permanent

  let days_exn days =
    let minimum_days = 1 in
    let maximum_days = 999 in
    match
      Maybe_bound.compare_to_interval_exn
        ~lower:(Maybe_bound.Incl minimum_days)
        ~upper:(Maybe_bound.Incl maximum_days)
        ~compare
        days
    with
    | Maybe_bound.In_range -> Days days
    | Maybe_bound.Below_lower_bound | Maybe_bound.Above_upper_bound ->
      raise_s
        [%message
          "Duration disallowed by Reddit"
            (days : int)
            (minimum_days : int)
            (maximum_days : int)]
  ;;

  let parameter_of_t t =
    match t with
    | Permanent -> None
    | Days days -> Some (Int.to_string days)
  ;;
end

type t =
  | Friend
  | Moderator
  | Moderator_invite
  | Contributor
  | Banned
  | Muted
  | Wiki_banned
  | Wiki_contributor
[@@deriving sexp]

let to_string t =
  match t with
  | Friend -> "friend"
  | Moderator -> "moderator"
  | Moderator_invite -> "moderator_invite"
  | Contributor -> "contributor"
  | Banned -> "banned"
  | Muted -> "muted"
  | Wiki_banned -> "wikibanned"
  | Wiki_contributor -> "wikicontributor"
;;

let add
    t
    ~connection
    ~username
    ~subreddit
    ~duration
    ?note
    ?ban_reason
    ?ban_message
    ?ban_context
    ()
  =
  let validate_string_length string ~max_length =
    match String.length string <= max_length with
    | true -> ()
    | false -> raise_s [%message "String exceeds maximum length" (max_length : int)]
  in
  Option.iter note ~f:(validate_string_length ~max_length:300);
  Option.iter ban_reason ~f:(validate_string_length ~max_length:100);
  let uri =
    sprintf !"https://oauth.reddit.com/r/%{Subreddit_name}/api/friend" subreddit
    |> Uri.of_string
  in
  let params =
    Param_dsl.make
      [ "api_type", Required' "json"
      ; "name", Required' (Username.to_string username)
      ; "type", Required' (to_string t)
      ; "duration", Optional' (Duration.parameter_of_t duration)
      ; "note", Optional' note
      ; "ban_reason", Optional' ban_reason
      ; "ban_message", Optional' ban_message
      ; "ban_context", Optional' (Option.map ban_context ~f:Fullname.to_string)
      ]
  in
  Connection.post_form connection ~params uri
;;

let remove t ~connection ~username ~subreddit () =
  let uri =
    sprintf !"https://oauth.reddit.com/r/%{Subreddit_name}/api/unfriend" subreddit
    |> Uri.of_string
  in
  let params =
    [ "api_type", [ "json" ]
    ; "name", [ Username.to_string username ]
    ; "type", [ to_string t ]
    ]
  in
  Connection.post_form connection ~params uri
;;
