open! Core

module State : sig
  type t =
    | Approved
    | Removed
  [@@deriving sexp]
end

module History : sig
  type t =
    { moderator : User.t
    ; time : Time.t
    }
  [@@deriving sexp]
end

type t =
  { state : State.t
  ; history : History.t
  }
[@@deriving sexp]
