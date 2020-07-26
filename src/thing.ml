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
    get_field t field_name
    >>= Json.none_if_null
    >>| Json.get_string
    >>| Username.of_string
  ;;

  let time_of_field t ~field_name =
    let open Option.Monad_infix in
    get_field t field_name
    >>= Json.none_if_null
    >>| Json.get_float
    >>| Time.Span.of_sec
    >>| Time.of_span_since_epoch
  ;;

  let author t =
    match username_of_field t ~field_name:"author" with
    | Some author -> author
    | None -> raise_s [%message "Missing author" (t : t)]
  ;;

  let subreddit t =
    get_field_exn t "subreddit" |> Json.get_string |> Subreddit_name.of_string
  ;;

  let title t = get_field_exn t "title" |> Json.get_string
  let is_stickied t = get_field_exn t "stickied" |> Json.get_bool

  let creation_time t =
    get_field_exn t "created_utc"
    |> Json.get_float
    |> Time.Span.of_sec
    |> Time_ns.Span.of_span_float_round_nearest
    |> Time_ns.of_span_since_epoch
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

module Fullname = struct
  module M = struct
    type t =
      [ `Comment of Comment.Id.t
      | `User of User.Id.t
      | `Link of Link.Id.t
      | `Message of Message.Id.t
      | `Subreddit of Subreddit.Id.t
      | `Award of Award.Id.t
      | `More_comments of More_comments.Id.t
      | `Modmail_conversation of Modmail_conversation.Id.t
      ]
    [@@deriving sexp, bin_io, compare, hash]

    let of_string s =
      let kind_string, id_string = String.lsplit2_exn s ~on:'_' in
      let kind = Thing_kind.of_string kind_string in
      let id = Id36.of_string id_string in
      Thing_kind.to_polymorphic_tag_uniform kind ~data:id
    ;;

    let to_string t =
      let kind, id = Thing_kind.of_polymorphic_tag_with_uniform_data t in
      sprintf !"%{Thing_kind}_%{Id36}" kind id
    ;;

    let module_name = "Thing.Fullname"
  end

  include Identifiable.Make (M)
  include M
end

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
    let kind = Json.find json ~key:"kind" |> Json.get_string |> Thing_kind.of_string in
    let data = Json.find json ~key:"data" |> Comment.of_json in
    Thing_kind.to_polymorphic_tag_uniform kind ~data
  ;;

  let fullname t =
    let (T : (Comment.Id.t, Link.Id.t) Type_equal.t) = Type_equal.T in
    let kind, data = Thing_kind.of_polymorphic_tag_with_uniform_data t in
    let id = Comment.id data in
    Thing_kind.to_polymorphic_tag_uniform kind ~data:id
  ;;
end
