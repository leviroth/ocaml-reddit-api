open! Core

type t =
  | Comment
  | User
  | Submission
  | Message
  | Subreddit
  | Award
[@@deriving sexp]

let of_string s =
  match s with
  | "t1" -> Comment
  | "t2" -> User
  | "t3" -> Submission
  | "t4" -> Message
  | "t5" -> Subreddit
  | "t6" -> Award
  | _ -> raise_s [%message "Unknown thing kind" s]
;;

let to_string t =
  match t with
  | Comment -> "t1"
  | User -> "t2"
  | Submission -> "t3"
  | Message -> "t4"
  | Subreddit -> "t5"
  | Award -> "t6"
;;
