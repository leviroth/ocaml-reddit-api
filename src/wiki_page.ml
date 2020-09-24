open! Core

module Id = struct
  type t =
    { subreddit : Subreddit_name.t option
    ; page : string
    }
  [@@deriving sexp]
end

include Json_object_utils

include Json_object_utils.Kinded (struct
  type nonrec t = t

  let kind = "wikipage"
  let of_data_field = Json.to_map
  let to_data_field t = `Object (Map.to_alist t)
end)

let may_revise = required_field "may_revise" bool
let revision_id = required_field "revision_id" (string >> Uuid.of_string)
let revision_by = required_field "revision_by" Thing.User.of_json

let content t markup =
  let field =
    match markup with
    | `markdown -> "content_md"
    | `HTML -> "content_html"
  in
  required_field field string t
;;

let revision_time = required_field "revision_date" time
let revision_reason = optional_field "reason" string

module Edit_conflict = struct
  include Json_object_utils

  let of_json = Json.to_map
  let to_json t = `Object (Map.to_alist t)
  let diff = required_field "diffcontent" string
  let message = required_field "message" string
  let new_content = required_field "newcontent" string
  let new_revision = required_field "newrevision" (string >> Uuid.of_string)
  let reason = required_field "reason" string
end
