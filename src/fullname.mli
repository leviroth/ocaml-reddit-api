open! Core

type t =
  | Comment of Id36.Comment.t
  | User of Id36.User.t
  | Submission of Id36.Submission.t
  | Message of Id36.Message.t
  | Subreddit of Id36.Subreddit.t
  | Award of Id36.Award.t
[@@deriving sexp]

include Stringable.S with type t := t
