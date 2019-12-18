open! Core

type t [@@deriving sexp]

val create : Thing_kind.t -> Id36.t -> t
val kind : t -> Thing_kind.t
val id36 : t -> Id36.t

include Stringable.S with type t := t
