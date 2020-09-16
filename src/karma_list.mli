open! Core

module Entry : sig
  type t [@@deriving sexp]

  val subreddit : t -> Subreddit_name.t
  val link_karma : t -> int
  val comment_karma : t -> int
end

type t = Entry.t list [@@deriving sexp]

val of_json : Json.t -> t
val to_json : t -> Json.t
