open! Core

type t =
  | Required of string * string list
  | Required' of string * string
  | Optional of string * string list option
  | Optional' of string * string option

let make ts =
  List.filter_map ts ~f:(function
      | Required (name, values) | Optional (name, Some values) -> Some (name, values)
      | Required' (name, value) | Optional' (name, Some value) -> Some (name, [ value ])
      | Optional (_, None) | Optional' (_, None) -> None)
;;
