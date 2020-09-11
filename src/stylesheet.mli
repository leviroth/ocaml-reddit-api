open! Core

module Image : sig
  type t

  val url : t -> Uri.t
  val link : t -> string
  val name : t -> string
end

type t

val of_json : Json.t -> t
val to_json : t -> Json.t
val images : t -> Image.t list
val subreddit_id : t -> Thing.Subreddit.Id.t
val stylesheet_text : t -> string
