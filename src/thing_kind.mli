open! Core

type t =
  | Comment
  | User
  | Submission
  | Message
  | Subreddit
  | Award
[@@deriving sexp]

include Stringable.S with type t := t
