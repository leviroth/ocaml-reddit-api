open! Core
include Json_object_intf

module Utils = struct
  type t = Jsonaf.t [@@deriving sexp, jsonaf]

  include (Of_json : module type of Of_json with type 'a t := 'a Of_json.t)

  let field_map json = Jsonaf.assoc_list_exn json |> Map.of_alist_exn (module String)
  let get_field t field = Jsonaf.member field t
  let get_field_exn t field = Jsonaf.member_exn field t
  let required_field name convert t = convert (get_field_exn t name)
  let username = string @> Username.of_string
  let subreddit_name = string @> Subreddit_name.of_string
  let time_sec_since_epoch = float @> Time_ns.Span.of_sec >>> Time_ns.of_span_since_epoch
  let uri = string @> Uri.of_string
end

include Utils

module Make_kinded (Param : Kinded_param) = struct
  include Param

  let t_of_jsonaf json =
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

  let jsonaf_of_t t =
    `Object [ "kind", `String Param.kind; "data", Param.to_data_field t ]
  ;;
end

module Make_kinded_simple (Param : sig
  val kind : string
end) =
Make_kinded (struct
  type t = Utils.t

  let kind = Param.kind
  let of_data_field = [%of_jsonaf: t]
  let to_data_field = [%jsonaf_of: t]
end)
