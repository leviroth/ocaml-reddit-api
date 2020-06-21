open! Core

type t = string

include Identifiable.Make (struct
  include String.Caseless

  let module_name = "Username"
  let to_string = ident

  let of_string string =
    List.find_map [ "u/"; "/u/" ] ~f:(fun prefix -> String.chop_prefix string ~prefix)
    |> Option.value ~default:string
  ;;
end)
