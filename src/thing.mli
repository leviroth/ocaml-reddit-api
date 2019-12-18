open! Core

type t [@@deriving sexp]

val kind : t -> Thing_kind.t
val of_json : Yojson.Safe.t -> t
val to_json : t -> Yojson.Safe.t
val fullname : t -> Fullname.t option
val get_field : t -> string -> Json_derivers.Yojson.t option
val author : t -> Username.t option
val moderation_info : t -> Moderation_info.t option
