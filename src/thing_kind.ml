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

let of_string s =
  match s with
  | "t1" -> Comment
  | "t2" -> User
  | "t3" -> Link
  | "t4" -> Message
  | "t5" -> Subreddit
  | "t6" -> Award
  | "more" -> More_comments
  | "modmail" -> Modmail_conversation
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
  | More_comments -> "more"
  | Modmail_conversation -> "modmail"
;;
