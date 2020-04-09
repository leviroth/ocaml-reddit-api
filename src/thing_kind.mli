open! Core

type t =
  | Comment
  | User
  | Link
  | Message
  | Subreddit
  | Award
[@@deriving sexp]

include Stringable.S with type t := t
