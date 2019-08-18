open! Core

type t [@@deriving sexp]

val of_json : Yojson.Safe.t -> t
val to_json : t -> Yojson.Safe.t
val after : t -> Fullname.t option
val children : t -> Thing.t list
