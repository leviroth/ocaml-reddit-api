open! Core

module Image = struct
  include Json_object_utils

  let of_json json =
    match json with
    | `O alist -> String.Map.of_alist_exn alist
    | _ -> raise_s [%message "Unexpected stylesheet image json" (json : Json.t)]
  ;;

  let url = required_field "url" uri
  let link = required_field "link" string
  let name = required_field "name" string
end

include Json_object_utils

include Json_object_utils.Kinded (struct
  type nonrec t = t

  let kind = "stylesheet"
  let of_data_field = Json.get_map
  let to_data_field t = `O (Map.to_alist t)
end)

let images = required_field "images" (Json.get_list Image.of_json)
let subreddit_id = required_field "subreddit_id" (string >> Thing.Subreddit.Id.of_string)
let stylesheet_text = required_field "stylesheet" string
