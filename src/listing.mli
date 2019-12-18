open! Core

type t [@@deriving sexp]

val of_json : Yojson.Safe.t -> t
val after : t -> Fullname.t option
val children : t -> Thing.t list
