open! Core

module Entry = struct
  type t =
    { subreddit : Subreddit_name.t
    ; comment_karma : int
    ; link_karma : int
    }
  [@@deriving sexp]

  let of_json json =
    match json with
    | `Object
        [ ("sr", `String subreddit_name)
        ; ("comment_karma", `Number comment_karma)
        ; ("link_karma", `Number link_karma)
        ] ->
      { subreddit = Subreddit_name.of_string subreddit_name
      ; comment_karma = Int.of_string comment_karma
      ; link_karma = Int.of_string link_karma
      }
    | _ -> raise_s [%message "Invalid [Karma_list.Entry] JSON" (json : Json.t)]
  ;;

  let to_json { subreddit; comment_karma; link_karma } =
    `Object
      [ "sr", `String (Subreddit_name.to_string subreddit)
      ; "comment_karma", `Number (Int.to_string comment_karma)
      ; "link_karma", `Number (Int.to_string link_karma)
      ]
  ;;
end

type t = Entry.t list [@@deriving sexp]

let of_json json =
  match json with
  | `Object [ ("kind", `String "KarmaList"); ("data", `Array entries) ] ->
    List.map entries ~f:Entry.of_json
  | _ -> raise_s [%message "Invalid [Karma_list] JSON" (json : Json.t)]
;;

let to_json entries =
  `Object
    [ "kind", `String "Karma_list"; "data", `Array (List.map entries ~f:Entry.to_json) ]
;;
