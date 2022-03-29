open! Core

type t [@@deriving sexp]

val of_json : Jsonaf.t -> t
val to_json : t -> Jsonaf.t
val submit_text : [ `markdown | `HTML ] -> t -> string
