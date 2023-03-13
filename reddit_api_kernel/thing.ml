open! Core
open Jsonaf.Export
include Thing_intf

module Make (Param : sig
  val kind : Thing_kind.t
end) =
struct
  include Json_object.Utils

  include Json_object.Make_kinded_simple (struct
    let kind = Thing_kind.to_string Param.kind
  end)

  type t = Jsonaf.t Map.M(String).t [@@deriving sexp]

  let module_name = Thing_kind.to_string_long Param.kind

  module Id = struct
    module T = struct
      include Id36

      include Identifiable.Make (struct
        include Id36

        let module_name = sprintf "%s.Id" module_name

        let of_string s =
          let prefix = sprintf !"%{Thing_kind}_" Param.kind in
          Id36.of_string (String.chop_prefix_if_exists s ~prefix)
        ;;
      end)
    end

    include T
    include Jsonaf.Jsonafable.Of_stringable (T)
  end

  let id = required_field "id" (string >> Id.of_string)
  let url = required_field "url" uri
  let author = required_field "author" (string >> Username.of_string_or_deleted)
  let title = required_field "title" string
  let description = required_field "description" string
  let is_stickied = required_field "stickied" bool
  let active_users = required_field "active_user_count" int
  let subscribers = required_field "subscribers" int
  let creation_time = required_field "created_utc" time_sec_since_epoch
  let depth = optional_field "depth" int
  let karma_field name = required_field name int
  let link_karma = karma_field "link_karma"
  let comment_karma = karma_field "comment_karma"
  let awarder_karma = karma_field "awarder_karma"
  let awardee_karma = karma_field "awardee_karma"
  let total_karma = karma_field "total_karma"

  let moderator_reports =
    required_field "mod_reports" [%of_jsonaf: Moderator_report.t list]
  ;;

  let permalink =
    required_field
      "permalink"
      (string
      >> Uri.of_string
      >> Uri.with_uri ~scheme:(Some "https") ~host:(Some "reddit.com"))
  ;;
end

module Link = struct
  include Make (struct
    let kind = Thing_kind.Link
  end)

  module Id = struct
    include (Id : module type of Id)

    let of_uri uri =
      match Uri.path uri |> String.split ~on:'/' with
      | "" :: "r" :: _subreddit :: "comments" :: id :: _rest -> of_string id
      | _ -> raise_s [%message "Unexpected Uri format" (uri : Uri_with_string_sexp.t)]
    ;;
  end

  let score = required_field "score" int
  let subreddit = required_field "subreddit" subreddit_name
  let domain = required_field "domain" string
  let self_text = required_field "selftext" string
  let is_self = required_field "is_self" bool

  module Contents = struct
    type t =
      | Url of Uri_with_string_sexp.t
      | Self_text of string
    [@@deriving sexp]
  end

  let contents t : Contents.t =
    match is_self t with
    | true -> Self_text (self_text t)
    | false -> Url (url t)
  ;;
end

module Comment' = struct
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
    let score_hidden = required_field "score_hidden" bool in
    let score = required_field "score" int in
    match score_hidden t with
    | true -> Hidden
    | false -> Score (score t)
  ;;

  let body = required_field "body" string
  let subreddit = required_field "subreddit" subreddit_name
  let link = required_field "link_id" (string >> Link.Id.of_string)
  let link_title = optional_field "link_title" string
end

module Message = Make (struct
  let kind = Thing_kind.Message
end)

module Subreddit = struct
  include Make (struct
    let kind = Thing_kind.Subreddit
  end)

  let name = required_field "display_name" subreddit_name
end

module User = struct
  include Make (struct
    let kind = Thing_kind.User
  end)

  let name = required_field "name" username
  let subreddit = required_field "subreddit" [%of_jsonaf: Subreddit.t]
end

module Award = Make (struct
  let kind = Thing_kind.Award
end)

module More_comments = struct
  include Make (struct
    let kind = Thing_kind.More_comments
  end)

  module Details = struct
    module By_children = struct
      type t = Comment'.Id.t list

      let children = Fn.id
    end

    type t =
      | By_children of By_children.t
      | By_parent of Comment'.Id.t
  end

  let count t = get_field_exn t "count" |> Jsonaf.int_exn

  let details t : Details.t =
    match count t with
    | 0 -> By_parent (required_field "parent_id" (string >> Comment'.Id.of_string) t)
    | _ -> By_children (required_field "children" [%of_jsonaf: Comment'.Id.t list] t)
  ;;
end

module Modmail_conversation = Make (struct
  let kind = Thing_kind.Modmail_conversation
end)

module Fullname = struct
  module M = struct
    type t =
      [ `Comment of Comment'.Id.t
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
    [ `Comment of Comment'.t
    | `User of User.t
    | `Link of Link.t
    | `Message of Message.t
    | `Subreddit of Subreddit.t
    | `Award of Award.t
    | `More_comments of More_comments.t
    | `Modmail_conversation of Modmail_conversation.t
    ]
  [@@deriving sexp]

  let t_of_jsonaf json =
    let kind =
      Jsonaf.member_exn "kind" json |> Jsonaf.string_exn |> Thing_kind.of_string
    in
    let data = Jsonaf.member_exn "data" json |> [%of_jsonaf: Comment'.t] in
    Thing_kind.to_polymorphic_tag_uniform kind ~data
  ;;

  let fullname t =
    let kind, data = Thing_kind.of_polymorphic_tag_with_uniform_data t in
    let id = Comment'.id data in
    Thing_kind.to_polymorphic_tag_uniform kind ~data:id
  ;;
end

module Comment = struct
  include Comment'

  let replies t =
    match get_field_exn t "replies" with
    | `String "" -> []
    | json ->
      [%of_jsonaf: Poly.t Listing.t] json
      |> Listing.children
      |> List.map ~f:(function
             | (`Comment _ | `More_comments _) as v -> v
             | _ -> assert false)
  ;;
end
