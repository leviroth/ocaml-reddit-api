open! Core

type 'a t [@@deriving sexp]

val of_json : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t
val after : 'a t -> Fullname.t option
val children : 'a t -> 'a list
