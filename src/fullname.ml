open! Core
open Thing

type t =
  [ `Comment of Comment.Id.t
  | `User of User.Id.t
  | `Link of Link.Id.t
  | `Message of Message.Id.t
  | `Subreddit of Subreddit.Id.t
  | `Award of Award.Id.t
  | `More_comments of More_comments.Id.t
  | `Modmail_conversation of Modmail_conversation.Id.t
  ]
[@@deriving sexp]
