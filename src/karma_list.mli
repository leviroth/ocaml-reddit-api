open! Core

module Entry : sig
  type t =
    { subreddit : Subreddit_name.t
    ; comment_karma : int
    ; link_karma : int
    }
  [@@deriving sexp]
end

type t = Entry.t list [@@deriving sexp]

val of_json : Json.t -> t
val to_json : t -> Json.t
