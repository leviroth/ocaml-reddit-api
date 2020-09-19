open! Core
include Json_object_utils

let of_json = Json.get_map
let to_json t = `O (Map.to_alist t)
let relationship_id = required_field "rel_id" string
let username = required_field "name" username
let user_id = required_field "id" (string >> Thing.User.Id.of_string)
let note = required_field "note" string
let days_left = optional_field "days_left" int
let date = required_field "date" time
