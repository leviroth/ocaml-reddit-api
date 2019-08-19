open! Core

module T = struct
  type t =
    { kind : Thing_kind.t
    ; data : Json_derivers.Yojson.t String.Map.t
    }
  [@@deriving sexp]

  let string_map_of_assoc_exn json =
    Yojson.Safe.Util.to_assoc json |> String.Map.of_alist_exn
  ;;

  let of_json json : t =
    let map = string_map_of_assoc_exn json in
    let kind =
      Map.find_exn map "kind" |> Yojson.Safe.Util.to_string |> Thing_kind.of_string
    in
    let data = Map.find_exn map "data" |> string_map_of_assoc_exn in
    { kind; data }
  ;;

  let to_json { kind; data } : Yojson.Safe.t =
    `Assoc
      [ "kind", `String (Thing_kind.to_string kind); "data", `Assoc (Map.to_alist data) ]
  ;;

  let get_field { data; _ } field_name = Map.find data field_name

  let username_of_field t ~field_name =
    let open Option.Monad_infix in
    get_field t field_name >>| Yojson.Safe.Util.to_string >>| Username.of_string
  ;;

  let time_of_field t ~field_name =
    let open Option.Monad_infix in
    get_field t field_name
    >>| Yojson.Safe.Util.to_float
    >>| Time.Span.of_sec
    >>| Time.of_span_since_epoch
  ;;

  let author t = username_of_field t ~field_name:"author"

  let moderation_info t =
    let approved_by = username_of_field t ~field_name:"approved_by" in
    let approved_at = time_of_field t ~field_name:"approved_at_utc" in
    let banned_by = username_of_field t ~field_name:"banned_by" in
    let banned_at = time_of_field t ~field_name:"banned_at_utc" in
    Moderation_info.of_listing_fields ~approved_by ~approved_at ~banned_by ~banned_at
  ;;

  let id t =
    get_field t "id"
    |> Option.map ~f:(Fn.compose Id36.of_string Yojson.Safe.Util.to_string)
  ;;

  let fullname t : Fullname.t option =
    id t |> Option.map ~f:(fun id -> ({ kind = t.kind; id } : Fullname.t))
  ;;
end

include T
module User = T
module Comment = T
module Submission = T
