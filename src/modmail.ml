open! Core

module Conversation = struct
  type t = Json.t String.Map.t [@@deriving sexp_of]

  let of_json = Json.to_map
  let to_json t = `Object (Map.to_alist t)
end
