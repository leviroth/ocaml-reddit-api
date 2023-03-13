open! Core
open Jsonaf.Export

type t =
  { moderator : Username.t option
  ; report : string
  }

let t_of_jsonaf json =
  let report, moderator_string = [%of_jsonaf: string * string] json in
  { report; moderator = Username.of_string_or_deleted moderator_string }
;;
