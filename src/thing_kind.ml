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

let of_string s =
  match s with
  | "t1" -> Comment
  | "t2" -> User
  | "t3" -> Link
  | "t4" -> Message
  | "t5" -> Subreddit
  | "t6" -> Award
  | _ -> raise_s [%message "Unknown thing kind" s]
;;

let to_string t =
  match t with
  | Comment -> "t1"
  | User -> "t2"
  | Link -> "t3"
  | Message -> "t4"
  | Subreddit -> "t5"
  | Award -> "t6"
  | Modmail_conversation -> "modmail"
  | More_comments -> "more"
;;
