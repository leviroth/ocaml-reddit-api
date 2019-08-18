open! Core

module T = struct
  type t =
    { kind : Thing_kind.t
    ; id : Id36.t
    }

  let of_string s =
    let kind, id = String.lsplit2_exn s ~on:'_' in
    { kind = Thing_kind.of_string kind; id = Id36.of_string id }
  ;;

  let to_string { kind; id } = sprintf !"%{Thing_kind}_%{Id36}" kind id
end

include T
include Sexpable.Of_stringable (T)
