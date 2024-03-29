open! Core
open Jsonaf.Export
include Json_object.Utils

module Rule = struct
  include Json_object.Utils

  module Kind = struct
    type t =
      | Link
      | Comment
      | All
    [@@deriving sexp]

    let of_string string =
      match string with
      | "link" -> Link
      | "comment" -> Comment
      | "all" -> All
      | _ ->
        raise_s
          [%message "Unrecognized [Subreddit_rules.Rule.Kind.t] string" (string : string)]
    ;;
  end

  let kind = required_field "kind" (string >> Kind.of_string)

  let description t markup =
    let field =
      match markup with
      | `markdown -> "description"
      | `HTML -> "description_html"
    in
    required_field field string t
  ;;

  let short_name = required_field "short_name" string
  let report_reason = required_field "violation_reason" string
  let creation_time = required_field "created_utc" time_sec_since_epoch
  let priority = required_field "priority" int
end

let subreddit_rules = required_field "rules" [%of_jsonaf: Rule.t list]
let site_rules = required_field "site_rules" Fn.id
let site_rules_flow = required_field "site_rules_flow" Fn.id
