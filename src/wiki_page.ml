open! Core

module Id = struct
  type t =
    { subreddit : Subreddit_name.t option
    ; page : string
    }
  [@@deriving sexp]
end

include Json_object.Utils

include Json_object.Make_kinded_simple (struct
  let kind = "wikipage"
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
  include Json_object.Utils

  let diff = required_field "diffcontent" string
  let message = required_field "message" string
  let new_content = required_field "newcontent" string
  let new_revision = required_field "newrevision" (string >> Uuid.of_string)
  let reason = required_field "reason" string
end
