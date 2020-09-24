open! Core

module Conversation = struct
  include Json_object_utils

  let of_json = Json.get_map
  let to_json t = `O (Map.to_alist t)
end
