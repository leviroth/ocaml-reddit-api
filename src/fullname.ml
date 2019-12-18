open! Core

module T = struct
  type t =
    { kind : Thing_kind.t
    ; id36 : Id36.t
    }
  [@@deriving fields]

  let of_string s =
    let kind_string, id36_string = String.lsplit2_exn s ~on:'_' in
    { kind = Thing_kind.of_string kind_string; id36 = Id36.of_string id36_string }
  ;;

  let to_string { kind; id36 } = sprintf !"%{Thing_kind}_%{Id36}" kind id36
end

include T
include Sexpable.Of_stringable (T)

let create kind id36 = { kind; id36 }
