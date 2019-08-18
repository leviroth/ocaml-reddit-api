open! Core

type t =
  { kind : Thing_kind.t
  ; id : Id36.t
  }
[@@deriving sexp]

include Stringable.S with type t := t
