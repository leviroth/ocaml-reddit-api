open! Core

type t [@@deriving sexp]

include Stringable.S with type t := t

val of_int : int -> t
val to_int : t -> int
