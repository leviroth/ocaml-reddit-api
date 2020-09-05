open! Core

module Id = struct
  type t = Uuid.Unstable.t [@@deriving sexp]

  let of_uuid = Fn.id
  let to_uuid = Fn.id

  let of_json_string string =
    match String.lsplit2_exn string ~on:'_' with
    | "ModAction", uuid -> Uuid.of_string uuid
    | _ -> raise_s [%message "Unexpected ModAction string" (string : string)]
  ;;
end

type t = Json.t String.Map.t

let of_json json =
  (match Json.find json ~key:"kind" with
  | `String "modaction" -> ()
  | _ -> raise_s [%message "Unexpected modaction json" (json : Json.t)]);
  match Json.find json ~key:"data" with
  | `Object alist -> String.Map.of_alist_exn alist
  | _ -> raise_s [%message "Unexpected modaction json" (json : Json.t)]
;;

let to_json t = `Object [ "kind", `String "modaction"; "data", `Object (Map.to_alist t) ]
let id t = Map.find_exn t "id" |> Json.get_string |> Id.of_json_string
let action t = Map.find_exn t "action" |> Json.get_string
let details t = Map.find_exn t "details" |> Json.get_string

let created t =
  Map.find_exn t "created_utc"
  |> Json.get_float
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
;;

let target_title t = Map.find_exn t "target_title" |> Json.get_string

let target_fullname t =
  Map.find_exn t "target_fullname" |> Json.get_string |> Thing.Fullname.of_string
;;

let target_permalink t =
  Map.find_exn t "target_permalink" |> Json.get_string |> Uri.of_string
;;

let subreddit_name t =
  Map.find_exn t "subreddit_name" |> Json.get_string |> Subreddit_name.of_string
;;

let moderator t = Map.find_exn t "moderator" |> Json.get_string |> Username.of_string
