open! Core
include Thing_intf

module Make (Param : sig
  val kind : Thing_kind.t
end) =
struct
  type t = Json.t String.Map.t [@@deriving sexp, bin_io]

  let field_map = Fn.id
  let module_name = Thing_kind.to_string_long Param.kind

  module Id = struct
    include Id36

    include Identifiable.Make (struct
      include Id36

      let module_name = sprintf "%s.Id" module_name

      let of_string s =
        let prefix = sprintf !"%{Thing_kind}_" Param.kind in
        Id36.of_string (String.chop_prefix_if_exists s ~prefix)
      ;;

      let to_string = Id36.to_string
    end)
  end

  let of_json_inner = Json.to_map

  let of_json (json : Json.t) =
    match json with
    | `Object alist ->
      (match List.Assoc.find alist "kind" ~equal:String.equal with
      | None -> of_json_inner json
      | Some (`String kind) ->
        (match Thing_kind.equal Param.kind (Thing_kind.of_string kind) with
        | true -> of_json_inner (Json.find json ~key:"data")
        | false ->
          raise_s
            [%message
              "Unexpected thing kind"
                ~expected:(Param.kind : Thing_kind.t)
                (json : Json.t)])
      | Some kind ->
        raise_s [%message "Thing kind is not a string" (kind : Json.t) (json : Json.t)])
    | _ -> raise_s [%message "Unexpected thing JSON type" (json : Json.t)]
  ;;

  let to_json t =
    `Object
      [ "kind", `String (Thing_kind.to_string Param.kind)
      ; "data", `Object (Map.to_alist t)
      ]
  ;;

  let get_field = Map.find

  let get_field_exn t field =
    match Map.find t field with
    | Some value -> value
    | None -> raise_s [%message "Field missing in get_field_exn" (t : t) (field : string)]
  ;;

  let get_string_field_exn field t = get_field_exn t field |> Json.get_string
  let id t = get_string_field_exn "id" t |> Id.of_string

  let url t =
    get_field t "url" |> Option.map ~f:(Fn.compose Uri.of_string Json.get_string)
  ;;

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

  let title = get_string_field_exn "title"
  let description = get_string_field_exn "description"
  let is_stickied t = get_field_exn t "stickied" |> Json.get_bool
  let active_users t = get_field_exn t "active_user_count" |> Json.get_int
  let subscribers t = get_field_exn t "subscribers" |> Json.get_int

  let creation_time t =
    get_field_exn t "created_utc"
    |> Json.get_float
    |> Time.Span.of_sec
    |> Time_ns.Span.of_span_float_round_nearest
    |> Time_ns.of_span_since_epoch
  ;;

  let depth t = get_field t "depth" |> Option.map ~f:Json.get_int
  let karma_field field t = get_field_exn t field |> Json.get_int
  let link_karma = karma_field "link_karma"
  let comment_karma = karma_field "comment_karma"
  let awarder_karma = karma_field "awarder_karma"
  let awardee_karma = karma_field "awardee_karma"
  let total_karma = karma_field "total_karma"
end

module Comment = struct
  include Make (struct
    let kind = Thing_kind.Comment
  end)

  module Score = struct
    type t =
      | Score of int
      | Hidden
    [@@deriving sexp]
  end

  let score t : Score.t =
    match get_field_exn t "score_hidden" |> Json.get_bool with
    | true -> Hidden
    | false -> get_field_exn t "score" |> Json.get_int |> Score
  ;;

  let body t = get_field_exn t "body" |> Json.get_string
  let subreddit t = get_string_field_exn "subreddit" t |> Subreddit_name.of_string
end

module Link = struct
  include Make (struct
    let kind = Thing_kind.Link
  end)

  let score t = get_field_exn t "score" |> Json.get_int
  let subreddit t = get_string_field_exn "subreddit" t |> Subreddit_name.of_string
end

module Message = Make (struct
  let kind = Thing_kind.Message
end)

module Subreddit = struct
  include Make (struct
    let kind = Thing_kind.Subreddit
  end)

  let name t = get_string_field_exn "display_name" t |> Subreddit_name.of_string
end

module User = struct
  include Make (struct
    let kind = Thing_kind.User
  end)

  let name t = get_string_field_exn "name" t |> Username.of_string
  let subreddit t = get_field_exn t "subreddit" |> Subreddit.of_json
end

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
