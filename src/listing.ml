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
  let open Or_error.Let_syntax in
  let fail s = error_s [%message s (json : Json.t)] in
  let%bind assoc =
    match json with
    | `Object list -> Ok list
    | _ -> fail "Expected JSON map when creating [Listing.t]"
  in
  let%bind data =
    match List.Assoc.find assoc "data" ~equal:String.equal with
    | Some data -> Ok data
    | None -> fail "Missing field \"data\""
  in
  let%bind data =
    match data with
    | `Object list -> Ok list
    | _ -> fail "Expected \"data\" to be an object"
  in
  let%bind data =
    match String.Map.of_alist data with
    | `Ok map -> Ok map
    | `Duplicate_key key -> fail (sprintf "Duplicate key: \"%s\"" key)
  in
  let%bind children =
    match Map.find data "children" with
    | Some (`Array l) -> Ok l
    | Some _ -> fail "Expected \"children\" to be a list"
    | None -> fail "Missing key \"children\""
  in
  let%bind children = List.map children ~f:convert_element |> Or_error.all in
  let%bind after =
    match Map.find data "after" with
    | None -> Ok None
    | Some (`String s) -> Ok (Some (Page_id.of_string s))
    | Some _ -> fail "Expected \"after\" to be a string"
  in
  return { children; after }
;;
