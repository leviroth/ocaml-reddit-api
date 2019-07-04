open! Core

type t =
  | Required of string list
  | Required' of string
  | Optional of string list option
  | Optional' of string option

let make field_names_and_ts =
  List.filter_map field_names_and_ts ~f:(function
      | name, (Required values | Optional (Some values)) -> Some (name, values)
      | name, (Required' value | Optional' (Some value)) -> Some (name, [ value ])
      | _, (Optional None | Optional' None) -> None)
;;
