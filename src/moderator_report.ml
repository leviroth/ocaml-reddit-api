open! Core

type t =
  { moderator : Username.t option
  ; report : string
  }

let of_json json =
  let report, moderator =
    Json.get_pair Json.get_string Json_object.Utils.(or_null username) json
  in
  { report; moderator }
;;
