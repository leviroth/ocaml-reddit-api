open! Core

type t =
  { moderator : Username.t option
  ; report : string
  }

let of_json json =
  let report, moderator_string = [%of_jsonaf: string * string] json in
  { report; moderator = Username.of_string_or_deleted moderator_string }
;;
