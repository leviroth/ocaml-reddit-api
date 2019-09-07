open! Core

module T = struct
  type t =
    | Comment of Id36.Comment.t
    | User of Id36.User.t
    | Link of Id36.Link.t
    | Message of Id36.Message.t
    | Subreddit of Id36.Subreddit.t
    | Award of Id36.Award.t

  let of_string s =
    let kind, id = String.lsplit2_exn s ~on:'_' in
    match Thing_kind.of_string kind with
    | Comment -> Comment (Id36.Comment.of_string id)
    | User -> User (Id36.User.of_string id)
    | Link -> Link (Id36.Link.of_string id)
    | Message -> Message (Id36.Message.of_string id)
    | Subreddit -> Subreddit (Id36.Subreddit.of_string id)
    | Award -> Award (Id36.Award.of_string id)
  ;;

  let to_string t =
    let (thing_kind, id36_string) : Thing_kind.t * string =
      match t with
      | Comment id36 -> Comment, Id36.Comment.to_string id36
      | User id36 -> User, Id36.User.to_string id36
      | Link id36 -> Link, Id36.Link.to_string id36
      | Message id36 -> Message, Id36.Message.to_string id36
      | Subreddit id36 -> Subreddit, Id36.Subreddit.to_string id36
      | Award id36 -> Award, Id36.Award.to_string id36
    in
    sprintf !"%{Thing_kind}_%s" thing_kind id36_string
  ;;
end

include T
include Sexpable.Of_stringable (T)
