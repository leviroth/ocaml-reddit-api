open! Core

type t = Json_derivers.Yojson.t String.Map.t [@@deriving sexp]

let of_json json =
  match json with
  | `Assoc list ->
    List.Assoc.find_exn list "data" ~equal:String.equal
    |> Yojson.Safe.Util.to_assoc
    |> String.Map.of_alist_exn
  | _ ->
    raise_s
      [%message
        "Expected JSON map when creating [Listing.t]" (json : Json_derivers.Yojson.t)]
;;

let to_json t = `Assoc (Map.to_alist t)

let after t =
  Map.find t "after"
  |> Option.bind ~f:Yojson.Safe.Util.to_string_option
  |> Option.map ~f:Fullname.of_string
;;

let children (t : t) =
  Map.find_exn t "children" |> Yojson.Safe.Util.to_list |> List.map ~f:Thing.of_json
;;
