open! Core

module Item = struct
  module Id = String

  type t = Json.t String.Map.t [@@deriving sexp]

  let of_json = Json.to_map
  let to_json t = `Object (Map.to_alist t)
  let username t = Map.find_exn t "name" |> Json.get_string |> Username.of_string
  let user_id t = Map.find_exn t "id" |> Json.get_string |> Thing.User.Id.of_string
  let relationship_id t = Map.find_exn t "rel_id" |> Json.get_string

  let since t =
    Map.find_exn t "date"
    |> Json.get_float
    |> Time_ns.Span.of_sec
    |> Time_ns.of_span_since_epoch
  ;;
end

type t = Item.t list [@@deriving sexp]
