open! Core

module Conversation = struct
  include Json_object_utils

  let of_json = Json.to_map
  let to_json t = `Object (Map.to_alist t)
end
