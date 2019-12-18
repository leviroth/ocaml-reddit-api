open! Core

type t =
  { children : Thing.t list
  ; after : Fullname.t option
  }
[@@deriving sexp, fields]

let of_json json =
  let data =
    match json with
    | `Assoc list ->
      List.Assoc.find_exn list "data" ~equal:String.equal
      |> Yojson.Safe.Util.to_assoc
      |> String.Map.of_alist_exn
    | _ ->
      raise_s
        [%message
          "Expected JSON map when creating [Listing.t]" (json : Json_derivers.Yojson.t)]
  in
  let children =
    Map.find_exn data "children" |> Yojson.Safe.Util.to_list |> List.map ~f:Thing.of_json
  in
  let after =
    let open Option.Let_syntax in
    Map.find data "after" >>= Yojson.Safe.Util.to_string_option >>| Fullname.of_string
  in
  { children; after }
;;
