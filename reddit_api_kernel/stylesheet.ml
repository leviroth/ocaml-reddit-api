open! Core

module Image = struct
  include Json_object.Utils

  let of_json json =
    match Jsonaf.assoc_list json with
    | Some alist -> Map.of_alist_exn (module String) alist
    | None -> raise_s [%message "Unexpected stylesheet image json" (json : Jsonaf.t)]
  ;;

  let url = required_field "url" uri
  let link = required_field "link" string
  let name = required_field "name" string
end

include Json_object.Utils

include Json_object.Make_kinded_simple (struct
  let kind = "stylesheet"
end)

let images = required_field "images" (Jsonaf.list_of_jsonaf Image.of_json)
let subreddit_id = required_field "subreddit_id" (string >> Thing.Subreddit.Id.of_string)
let stylesheet_text = required_field "stylesheet" string
