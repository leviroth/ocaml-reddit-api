open! Core

type t [@@deriving sexp]

include Json_object.S with type t := t

val submit_text : [ `markdown | `HTML ] -> t -> string
