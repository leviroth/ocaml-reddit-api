open! Core

module Image = struct
  type t = Json.t String.Map.t

  let of_json json =
    match json with
    | `Object alist -> String.Map.of_alist_exn alist
    | _ -> raise_s [%message "Unexpected stylesheet image json" (json : Json.t)]
  ;;

  let url t = Map.find_exn t "url" |> Json.get_string |> Uri.of_string
  let link t = Map.find_exn t "link" |> Json.get_string
  let name t = Map.find_exn t "name" |> Json.get_string
end

type t = Json.t String.Map.t

let of_json json =
  (match Json.find json ~key:"kind" with
  | `String "stylesheet" -> ()
  | _ -> raise_s [%message "Unexpected stylesheet json" (json : Json.t)]);
  match Json.find json ~key:"data" with
  | `Object alist -> String.Map.of_alist_exn alist
  | _ -> raise_s [%message "Unexpected stylesheet json" (json : Json.t)]
;;

let to_json t = `Object [ "kind", `String "stylesheet"; "data", `Object (Map.to_alist t) ]
let images t = Map.find_exn t "images" |> Json.get_array |> List.map ~f:Image.of_json

let subreddit_id t =
  Map.find_exn t "subreddit_id" |> Json.get_string |> Thing.Subreddit.Id.of_string
;;

let stylesheet_text t = Map.find_exn t "stylesheet" |> Json.get_string
