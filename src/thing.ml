open! Core
include Thing_intf

module Make (Param : sig
  val kind : Thing_kind.t
end) =
struct
  type t = Json.t String.Map.t [@@deriving sexp]

  module Id = Id36

  let of_json = Json.to_map

  let of_json_with_tag_exn json =
    let kind = Thing_kind.of_string (Json.find json ~key:"kind" |> Json.get_string) in
    match Thing_kind.equal kind Param.kind with
    | false ->
      raise_s
        [%message
          "Unexpected kind"
            ~expected:(Param.kind : Thing_kind.t)
            (kind : Thing_kind.t)
            (json : Json.t)]
    | true -> of_json (Json.find json ~key:"data")
  ;;

  let to_json t = `Object (Map.to_alist t)
  let get_field = Map.find

  let get_field_exn t field =
    match Map.find t field with
    | Some value -> value
    | None -> raise_s [%message "Field missing in get_field_exn" (t : t) (field : string)]
  ;;

  let id t = get_field_exn t "id" |> Json.get_string |> Id.of_string

  let username_of_field t ~field_name =
    let open Option.Monad_infix in
    get_field t field_name >>| Json.get_string >>| Username.of_string
  ;;

  let time_of_field t ~field_name =
    let open Option.Monad_infix in
    get_field t field_name
    >>| Json.get_string
    >>| Float.of_string
    >>| Time.Span.of_sec
    >>| Time.of_span_since_epoch
  ;;

  let author t =
    match username_of_field t ~field_name:"author" with
    | Some author -> author
    | None -> raise_s [%message "Missing author" (t : t)]
  ;;

  let moderation_info t =
    let approved_by = username_of_field t ~field_name:"approved_by" in
    let approved_at = time_of_field t ~field_name:"approved_at_utc" in
    let banned_by = username_of_field t ~field_name:"banned_by" in
    let banned_at = time_of_field t ~field_name:"banned_at_utc" in
    Moderation_info.of_listing_fields ~approved_by ~approved_at ~banned_by ~banned_at
  ;;
end

module Comment = Make (struct
  let kind = Thing_kind.Comment
end)

module User = Make (struct
  let kind = Thing_kind.User
end)

module Link = Make (struct
  let kind = Thing_kind.Link
end)

module Message = Make (struct
  let kind = Thing_kind.Message
end)

module Subreddit = Make (struct
  let kind = Thing_kind.Subreddit
end)

module Award = Make (struct
  let kind = Thing_kind.Award
end)

module More_comments = Make (struct
  let kind = Thing_kind.More_comments
end)

module Modmail_conversation = Make (struct
  let kind = Thing_kind.Modmail_conversation
end)

module Poly = struct
  type t =
    [ `Comment of Comment.t
    | `User of User.t
    | `Link of Link.t
    | `Message of Message.t
    | `Subreddit of Subreddit.t
    | `Award of Award.t
    | `More_comments of More_comments.t
    | `Modmail_conversation of Modmail_conversation.t
    ]
  [@@deriving sexp]

  let of_json json =
    let data = Json.find json ~key:"data" in
    match Json.find json ~key:"kind" |> Json.get_string |> Thing_kind.of_string with
    | Comment -> `Comment (Comment.of_json data)
    | User -> `User (User.of_json data)
    | Link -> `Link (Link.of_json data)
    | Message -> `Message (Message.of_json data)
    | Subreddit -> `Subreddit (Subreddit.of_json data)
    | Award -> `Award (Award.of_json data)
    | More_comments -> `More_comments (More_comments.of_json data)
    | Modmail_conversation -> `Modmail_conversation (Modmail_conversation.of_json data)
  ;;
end
