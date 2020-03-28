open! Core

module Page_id = struct
  include String

  let of_fullname = Thing.Fullname.to_string
  let to_fullname t = Option.try_with (fun () -> Thing.Fullname.of_string t)
end

type 'a t =
  { children : 'a list
  ; after : Page_id.t option
  }
[@@deriving sexp, fields]

let of_json convert_element json =
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
    Map.find_exn data "children"
    |> Yojson.Safe.Util.to_list
    |> List.map ~f:convert_element
  in
  let after =
    let open Option.Let_syntax in
    Map.find data "after" >>= Yojson.Safe.Util.to_string_option >>| Page_id.of_string
  in
  { children; after }
;;
