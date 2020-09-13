open! Core

type t = Json.t String.Map.t [@@deriving sexp, bin_io]

let get_field = Map.find

let get_field_exn t field =
  match Map.find t field with
  | Some value -> value
  | None -> raise_s [%message "Missing JSON field" (t : t) (field : string)]
;;

let optional_field name convert t =
  Map.find t name |> Option.bind ~f:Json.none_if_null |> Option.map ~f:convert
;;

let required_field name convert t = convert (get_field_exn t name)
let ( >> ) f g x = g (f x)
let int = Json.get_int
let float = Json.get_float
let bool = Json.get_bool
let string = Json.get_string
let username = string >> Username.of_string
let subreddit_name = string >> Subreddit_name.of_string
let time = float >> Time_ns.Span.of_sec >> Time_ns.of_span_since_epoch
let uri = string >> Uri.of_string
