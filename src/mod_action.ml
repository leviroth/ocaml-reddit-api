open! Core

module Id = struct
  type t = Uuid.Unstable.t [@@deriving sexp]

  let of_uuid = Fn.id
  let to_uuid = Fn.id

  let of_json_string string =
    match String.lsplit2_exn string ~on:'_' with
    | "ModAction", uuid -> Uuid.of_string uuid
    | _ -> raise_s [%message "Unexpected ModAction string" (string : string)]
  ;;
end

include Json_object_utils

let of_json json =
  (match Json.find json ~key:"kind" with
  | `String "modaction" -> ()
  | _ -> raise_s [%message "Unexpected modaction json" (json : Json.t)]);
  match Json.find json ~key:"data" with
  | `Object alist -> String.Map.of_alist_exn alist
  | _ -> raise_s [%message "Unexpected modaction json" (json : Json.t)]
;;

let to_json t = `Object [ "kind", `String "modaction"; "data", `Object (Map.to_alist t) ]
let id = required_field "id" (string >> Id.of_json_string)
let action = required_field "action" string
let details = required_field "details" string
let created = required_field "created_utc" time
let target_title = required_field "target_title" string
let target_fullname = required_field "target_fullname" (string >> Thing.Fullname.of_string)
let target_permalink = required_field "target_permalink" uri
let subreddit_name = required_field "subreddit_name" subreddit_name
let moderator = required_field "moderator" username
