open! Core

module Entry = struct
  include Json_object_utils

  let of_json json =
    match json with
    | `Object alist -> String.Map.of_alist_exn alist
    | _ -> raise_s [%message "Invalid [Karma_list.Entry] JSON" (json : Json.t)]
  ;;

  let to_json t = `Object (Map.to_alist t)
  let subreddit = required_field "sr" subreddit_name
  let link_karma = required_field "link_karma" int
  let comment_karma = required_field "comment_karma" int
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
