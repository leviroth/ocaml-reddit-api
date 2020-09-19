open! Core
include Json_object_utils

let of_json = Json.to_map
let to_json t = `Object (Map.to_alist t)
let relationship_id = required_field "rel_id" string
let username = required_field "name" username
let user_id = required_field "id" (string >> Thing.User.Id.of_string)
let date = required_field "date" time
