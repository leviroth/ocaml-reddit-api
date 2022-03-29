open! Core
include Json_object_intf

module Utils = struct
  type t = Jsonaf.t Map.M(String).t [@@deriving sexp]

  let field_map = Fn.id
  let of_json json = Jsonaf.assoc_list_exn json |> Map.of_alist_exn (module String)
  let to_json t = `Object (Map.to_alist t)
  let get_field = Map.find

  let get_field_exn t field =
    match Map.find t field with
    | Some value -> value
    | None -> raise_s [%message "Missing JSON field" (t : t) (field : string)]
  ;;

  let optional_field name convert t =
    match Map.find t name with
    | None | Some `Null -> None
    | Some v -> Some (convert v)
  ;;

  let required_field name convert t = convert (get_field_exn t name)
  let ( >> ) f g x = g (f x)

  let or_null f json =
    match json with
    | `Null -> None
    | json -> Some (f json)
  ;;

  let int = Jsonaf.int_exn
  let float = Jsonaf.float_exn
  let bool = Jsonaf.bool_exn
  let string = Jsonaf.string_exn
  let username = string >> Username.of_string
  let subreddit_name = string >> Subreddit_name.of_string
  let time = float >> Time_ns.Span.of_sec >> Time_ns.of_span_since_epoch
  let uri = string >> Uri.of_string
end

include Utils

module Make_kinded (Param : Kinded_param) = struct
  include Param

  let of_json json =
    match Jsonaf.member "kind" json with
    | None -> Param.of_data_field json
    | Some (`String kind) ->
      (match String.equal Param.kind kind with
      | true -> Param.of_data_field (Jsonaf.member_exn "data" json)
      | false ->
        raise_s
          [%message
            "Unexpected JSON object kind"
              ~expected:(Param.kind : string)
              (json : Jsonaf.t)])
    | Some kind ->
      raise_s
        [%message "JSON object kind is not a string" (kind : Jsonaf.t) (json : Jsonaf.t)]
  ;;

  let to_json t = `Object [ "kind", `String Param.kind; "data", Param.to_data_field t ]
end

module Make_kinded_simple (Param : sig
  val kind : string
end) =
Make_kinded (struct
  type t = Utils.t

  let kind = Param.kind
  let of_data_field = of_json
  let to_data_field = to_json
end)
