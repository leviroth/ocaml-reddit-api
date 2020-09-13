open! Core

module Item = struct
  module Id = String
  include Json_object_utils

  let of_json = Json.to_map
  let to_json t = `Object (Map.to_alist t)
  let username = required_field "name" username
  let user_id = required_field "id" (string >> Thing.User.Id.of_string)
  let relationship_id = required_field "rel_id" string
  let since = required_field "date" time
end

type t = Item.t list [@@deriving sexp]
