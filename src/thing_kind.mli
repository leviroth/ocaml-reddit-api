open! Core

type t =
  | Comment
  | User
  | Link
  | Message
  | Subreddit
  | Award
  | More_comments
  | Modmail_conversation
[@@deriving sexp, equal]

include Stringable.S with type t := t
