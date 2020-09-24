open! Core

module Entry : sig
  type t [@@deriving sexp]

  val subreddit : t -> Subreddit_name.t
  val link_karma : t -> int
  val comment_karma : t -> int
end

type t = Entry.t list [@@deriving sexp]

include Jsonable.S with type t := t
