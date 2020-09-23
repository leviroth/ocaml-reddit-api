open! Core
include Json_object_utils

let of_json = Json.get_map
let to_json t = `O (Map.to_alist t)

let submit_text markup =
  let field =
    match markup with
    | `markdown -> "submit_text"
    | `HTML -> "submit_text_html"
  in
  required_field field string
;;
