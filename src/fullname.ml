open! Core

module T = struct
  type t =
    | Comment of Id36.Comment.t
    | User of Id36.User.t
    | Submission of Id36.Submission.t
    | Message of Id36.Message.t
    | Subreddit of Id36.Subreddit.t
    | Award of Id36.Award.t

  let of_string s =
    match String.lsplit2_exn s ~on:'_' with
    | "t1", rest -> Comment (Id36.Comment.of_string rest)
    | "t2", rest -> User (Id36.User.of_string rest)
    | "t3", rest -> Submission (Id36.Submission.of_string rest)
    | "t4", rest -> Message (Id36.Message.of_string rest)
    | "t5", rest -> Subreddit (Id36.Subreddit.of_string rest)
    | "t6", rest -> Award (Id36.Award.of_string rest)
    | _ -> raise_s [%message "Unknown thing kind" s]
  ;;

  let to_string t =
    match t with
    | Comment rest -> "t1_" ^ Id36.Comment.to_string rest
    | User rest -> "t2_" ^ Id36.User.to_string rest
    | Submission rest -> "t3_" ^ Id36.Submission.to_string rest
    | Message rest -> "t4_" ^ Id36.Message.to_string rest
    | Subreddit rest -> "t5_" ^ Id36.Subreddit.to_string rest
    | Award rest -> "t6_" ^ Id36.Award.to_string rest
  ;;
end

include T
include Sexpable.Of_stringable (T)
