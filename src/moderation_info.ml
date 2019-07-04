open! Core

module State = struct
  type t =
    | Approved
    | Removed
  [@@deriving sexp]
end

module History = struct
  type t =
    { moderator : Username.t
    ; time : Time.t
    }
  [@@deriving sexp]
end

type t =
  { state : State.t
  ; history : History.t
  }
[@@deriving sexp]

let of_listing_fields ~approved_by ~approved_at ~banned_by ~banned_at =
  match approved_by, approved_at, banned_by, banned_at with
  | Some moderator, Some time, None, None ->
    Some { state = Approved; history = { moderator; time } }
  | None, None, Some moderator, Some time ->
    Some { state = Removed; history = { moderator; time } }
  | None, None, None, None -> None
  | _ ->
    raise_s
      [%message
        "Got conflicting moderation information"
          (approved_by : Username.t option)
          (approved_at : Time.t option)
          (banned_by : Username.t option)
          (banned_at : Time.t option)]
;;
