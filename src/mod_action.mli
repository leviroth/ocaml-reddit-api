open! Core

module Id : sig
  type t [@@deriving sexp]

  val to_uuid : t -> Uuid.t
  val of_uuid : Uuid.t -> t
end

type t

val of_json : Json.t -> t
val to_json : t -> Json.t
val id : t -> Id.t
val action : t -> string
val details : t -> string
val created : t -> Time_ns.t
val target_title : t -> string
val target_fullname : t -> Thing.Fullname.t
val target_permalink : t -> Uri.t
val subreddit_name : t -> Subreddit_name.t
val moderator : t -> Username.t
