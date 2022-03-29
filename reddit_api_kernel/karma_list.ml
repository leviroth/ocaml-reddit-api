open! Core

module Entry = struct
  include Json_object.Utils

  let of_json json =
    match Jsonaf.assoc_list json with
    | Some alist -> Map.of_alist_exn (module String) alist
    | None -> raise_s [%message "Invalid [Karma_list.Entry] JSON" (json : Jsonaf.t)]
  ;;

  let to_json t = `Object (Map.to_alist t)
  let subreddit = required_field "sr" subreddit_name
  let link_karma = required_field "link_karma" int
  let comment_karma = required_field "comment_karma" int
end

type t = Entry.t list [@@deriving sexp]

include Json_object.Make_kinded (struct
  type nonrec t = t

  let of_data_field json =
    match Jsonaf.list json with
    | Some entries -> List.map entries ~f:Entry.of_json
    | None -> raise_s [%message "Invalid [Karma_list] JSON" (json : Jsonaf.t)]
  ;;

  let to_data_field t = `Array (List.map t ~f:Entry.to_json)
  let kind = "KarmaList"
end)
