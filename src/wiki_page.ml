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

open Or_error.Let_syntax

let of_json json =
  let%bind () =
    match%bind Json.find json ~key:"kind" with
    | `String "wikipage" -> return ()
    | kind -> error_s [%message "Unexpected kind" (kind : Json.t)]
  in
  Json.find json ~key:"data" >>= Json.to_map
;;

let may_revise t = Map.find_or_error t "may_revise" >>= Json.get_bool

let revision_id t =
  Map.find_or_error t "revision_id" >>= Json.get_string >>| Uuid.of_string
;;

let revision_by t =
  match%bind Map.find_or_error t "revision_by" >>| Thing.of_json with
  | `User _ as u -> return u
  | thing -> error_s [%message "Unexpected kind" (thing : Thing.t)]
;;

let content t markup =
  let field =
    match markup with
    | `markdown -> "content_md"
    | `HTML -> "content_html"
  in
  Map.find_or_error t field >>= Json.get_string
;;

let revision_time t =
  Map.find_or_error t "revision_date"
  >>= Json.get_int
  >>| Time_ns.Span.of_int_sec
  >>| Time_ns.of_span_since_epoch
;;

let revision_reason t =
  match%bind Map.find_or_error t "reason" with
  | `Null -> return None
  | `String s -> return (Some s)
  | json -> error_s [%message "Expected nullable JSON string" (json : Json.t)]
;;
