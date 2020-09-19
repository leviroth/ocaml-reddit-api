open! Core

module Common = struct
  module Id = String
  include Json_object_utils

  let of_json = Json.get_map
  let to_json t = `O (Map.to_alist t)
  let relationship_id = required_field "rel_id" string
  let username = required_field "name" username
  let user_id = required_field "id" (string >> Thing.User.Id.of_string)
  let date = required_field "date" time
end

module Contributor = Common

module Mute = struct
  include Common

  module Id = struct
    include Id

    let to_uuid t = String.chop_prefix_exn t ~prefix:"Mute_" |> Uuid.of_string
    let of_uuid uuid = sprintf !"Mute_%{Uuid}" uuid
  end
end

module Ban = struct
  include Common

  let note = required_field "note" string
  let days_left = optional_field "days_left" int
end
