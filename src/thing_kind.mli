open! Core

type t =
  | Comment
  | User
  | Link
  | Message
  | Subreddit
  | Award
  | Modmail_conversation
  | More_comments
[@@deriving sexp]

include Stringable.S with type t := t
