open! Core

module Make (Param : sig
  val prefix : char
  val module_name : string
end) =
struct
  type t = string

  include Identifiable.Make (struct
    include String.Caseless

    let module_name = Param.module_name
    let to_string = ident

    let of_string =
      let prefixes =
        let common = sprintf "%c/" Param.prefix in
        [ common; "/" ^ common ]
      in
      fun string ->
        List.find_map prefixes ~f:(fun prefix -> String.chop_prefix string ~prefix)
        |> Option.value ~default:string
    ;;
  end)
end
