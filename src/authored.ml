open! Core
include Thing

let author t = Map.find t "author" |> Option.map ~f:User.of_json
