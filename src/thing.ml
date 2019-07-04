open! Core

module T = struct
  type t = Json_derivers.Yojson.t String.Map.t [@@deriving sexp]

  let of_json json =
    match json with
    | `Assoc list -> String.Map.of_alist_exn list
    | _ ->
      raise_s
        [%message
          (* TODO don't call this [Thing.t] *)
          "Expected JSON map when creating [Thing.t]" (json : Json_derivers.Yojson.t)]
  ;;

  let to_json t = `Assoc (Map.to_alist t)

  let username_of_field t ~field_name =
    let open Option.Monad_infix in
    Map.find t field_name >>| Yojson.Safe.Util.to_string >>| Username.of_string
  ;;

  let time_of_field t ~field_name =
    let open Option.Monad_infix in
    Map.find t field_name
    >>| Yojson.Safe.Util.to_float
    >>| Time.Span.of_sec
    >>| Time.of_span_since_epoch
  ;;

  let author t = username_of_field t ~field_name:"author"

  let fullname t =
    let open Option.Monad_infix in
    Map.find t "fullname" >>| Yojson.Safe.Util.to_string >>| Fullname.of_string
  ;;

  let moderation_info t =
    let approved_by = username_of_field t ~field_name:"approved_by" in
    let approved_at = time_of_field t ~field_name:"approved_at_utc" in
    let banned_by = username_of_field t ~field_name:"banned_by" in
    let banned_at = time_of_field t ~field_name:"banned_at_utc" in
    Moderation_info.of_listing_fields ~approved_by ~approved_at ~banned_by ~banned_at
  ;;
end

module Comment = T
module User = T
module Submission = T

type t =
  | Comment of Comment.t
  | User of User.t
  | Submission of Submission.t
[@@deriving sexp]

let of_json json =
  let map = T.of_json json in
  match Map.find_exn map "kind" |> Yojson.Safe.Util.to_string with
  | "t1" -> Comment map
  | "t2" -> User map
  | "t3" -> Submission map
  | _ -> raise_s [%message "Unknown kind" (json : Json_derivers.Yojson.t)]
;;

let to_json t =
  match t with
  | Comment map | User map | Submission map -> T.to_json map
;;

let fullname t =
  match t with
  | Comment map | User map | Submission map -> T.fullname map
;;
