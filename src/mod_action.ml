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

include Json_object_utils.Kinded (struct
  type nonrec t = t

  let kind = "modaction"
  let of_data_field = Json.get_map
  let to_data_field t = `O (Map.to_alist t)
end)

let id = required_field "id" (string >> Id.of_json_string)
let action = required_field "action" string
let details = required_field "details" string
let created = required_field "created_utc" time
let target_title = required_field "target_title" string
let target_fullname = required_field "target_fullname" (string >> Thing.Fullname.of_string)
let target_permalink = required_field "target_permalink" uri
let subreddit_name = required_field "subreddit_name" subreddit_name
let moderator = required_field "moderator" username
