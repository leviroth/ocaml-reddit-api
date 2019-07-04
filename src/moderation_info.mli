open! Core

module State : sig
  type t =
    | Approved
    | Removed
  [@@deriving sexp]
end

module History : sig
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

val of_listing_fields
  :  approved_by:Username.t option
  -> approved_at:Time.t option
  -> banned_by:Username.t option
  -> banned_at:Time.t option
  -> t option
