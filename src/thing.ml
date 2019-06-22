open! Core

type t = Json_derivers.Yojson.t String.Map.t [@@deriving sexp]

let of_json json =
  match json with
  | `Assoc list -> String.Map.of_alist_exn list
  | _ ->
    raise_s
      [%message
        "Expected JSON map when creating [Thing.t]" (json : Json_derivers.Yojson.t)]
;;
