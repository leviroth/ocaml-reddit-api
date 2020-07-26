open! Core

module Id = struct
  type t =
    { subreddit : Subreddit_name.t option
    ; page : string
    }
  [@@deriving sexp]
end

type t = Json.t String.Map.t [@@deriving sexp]

let to_json t = `Object [ "kind", `String "wikipage"; "data", `Object (Map.to_alist t) ]

let of_json json =
  (match Json.find json ~key:"kind" with
  | `String "wikipage" -> ()
  | kind -> raise_s [%message "Unexpected kind" (kind : Json.t)]);
  Json.find json ~key:"data" |> Json.to_map
;;

let may_revise t = Map.find_exn t "may_revise" |> Json.get_bool
let revision_id t = Map.find_exn t "revision_id" |> Json.get_string |> Uuid.of_string
let revision_by t = Map.find_exn t "revision_by" |> Thing.User.of_json

let content t markup =
  let field =
    match markup with
    | `markdown -> "content_md"
    | `HTML -> "content_html"
  in
  Map.find_exn t field |> Json.get_string
;;

let revision_time t =
  Map.find_exn t "revision_date"
  |> Json.get_int
  |> Time_ns.Span.of_int_sec
  |> Time_ns.of_span_since_epoch
;;

let revision_reason t =
  match Map.find_exn t "reason" with
  | `Null -> None
  | `String s -> Some s
  | json -> raise_s [%message "Expected nullable JSON string" (json : Json.t)]
;;

module Edit_conflict = struct
  type t = Json.t String.Map.t [@@deriving sexp]

  let of_json = Json.to_map
  let get_string_field t ~field = Map.find_exn t field |> Json.get_string
  let diff = get_string_field ~field:"diffcontent"
  let message = get_string_field ~field:"message"
  let new_content = get_string_field ~field:"newcontent"
  let new_revision t = get_string_field t ~field:"newrevision" |> Uuid.of_string
  let reason = get_string_field ~field:"reason"
end
