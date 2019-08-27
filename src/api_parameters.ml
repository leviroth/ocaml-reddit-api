open! Core

module Pagination = struct
  type t =
    | Before of Fullname.t
    | After of Fullname.t
  [@@deriving sexp]

  let params_of_t t =
    let key =
      match t with
      | Before _ -> "before"
      | After _ -> "after"
    in
    let value =
      match t with
      | Before fullname | After fullname -> Fullname.to_string fullname
    in
    [ key, [ value ] ]
  ;;
end

module Comment_sort = struct
  type t =
    | Confidence
    | Top
    | New
    | Controversial
    | Old
    | Random
    | Q_and_a
    | Live
  [@@deriving sexp]

  let to_string t =
    match t with
    | Q_and_a -> "qa"
    | _ -> sexp_of_t t |> Sexp.to_string |> String.lowercase
  ;;
end

module Report_target = struct
  type t =
    | Modmail_conversation of Id36.t
    | Fullname of Fullname.t
  [@@deriving sexp]

  let params_of_t t =
    match t with
    | Modmail_conversation id -> [ "modmail_conv_id", [ Id36.to_string id ] ]
    | Fullname fullname -> [ "thing_id", [ Fullname.to_string fullname ] ]
  ;;
end

module Sticky_state = struct
  type t =
    | Sticky of { slot : int }
    | Unsticky
  [@@deriving sexp]

  let params_of_t t =
    match t with
    | Sticky { slot } ->
      [ "state", [ Bool.to_string true ]; "num", [ Int.to_string slot ] ]
    | Unsticky -> [ "state", [ Bool.to_string false ] ]
  ;;
end

module Submission_kind = struct
  module Self_post_body = struct
    type t =
      | Markdown of string
      | Richtext_json of Json_derivers.Yojson.t
    [@@deriving sexp]
  end

  type t =
    | Link of { url : string }
    | Self of Self_post_body.t
    | Image
    | Video
    | Videogif
  [@@deriving sexp]

  let params_of_t t =
    match t with
    | Link { url } -> [ "kind", [ "link" ]; "url", [ url ] ]
    | Self body ->
      [ "kind", [ "link" ]
      ; ( "text"
        , [ (match body with
            | Markdown markdown -> markdown
            | Richtext_json json -> Yojson.Safe.to_string json)
          ] )
      ]
    (* TODO Should we disallow these? *)
    | Image | Video | Videogif -> assert false
  ;;
end

module Vote_direction = struct
  type t =
    | Up
    | Neutral
    | Down
  [@@deriving sexp]

  let int_of_t t =
    match t with
    | Up -> 1
    | Neutral -> 0
    | Down -> -1
  ;;

  let params_of_t t = [ "dir", [ int_of_t t |> Int.to_string ] ]
end

module Info_query = struct
  type t =
    | Id of Fullname.t list
    | Url of Uri_sexp.t
  [@@deriving sexp]

  let params_of_t t =
    match t with
    | Id fullnames -> [ "id", List.map fullnames ~f:Fullname.to_string ]
    | Url uri -> [ "url", [ Uri.to_string uri ] ]
  ;;
end

module Duplicate_sort = struct
  type t =
    | Number_of_comments
    | New

  let params_of_t t =
    [ ( "sort"
      , match t with
        | Number_of_comments -> [ "number_of_comments" ]
        | New -> [ "new" ] )
    ]
  ;;
end

module Historical_span = struct
  module T = struct
    type t =
      | Hour
      | Day
      | Week
      | Month
      | Year
      | All
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let params_of_t t = [ "t", [ to_string t |> String.lowercase ] ]
end

module Mod_filter = struct
  type t =
    | Moderators of Username.t list
    | Admin

  let params_of_t t =
    [ ( "mod"
      , match t with
        | Admin -> [ "a" ]
        | Moderators moderators -> List.map moderators ~f:Username.to_string )
    ]
  ;;
end

module Links_or_comments = struct
  type t =
    | Links
    | Comments

  let params_of_t t =
    [ ( "only"
      , match t with
        | Links -> [ "links" ]
        | Comments -> [ "comments" ] )
    ]
  ;;
end

module How_to_distinguish = struct
  type t =
    | Mod
    | Admin
    | Special
    | Undistinguish

  let params_of_t t =
    [ ( "how"
      , [ (match t with
          | Mod -> "yes"
          | Admin -> "admin"
          | Special -> "special"
          | Undistinguish -> "no")
        ] )
    ]
  ;;
end

module Search_sort = struct
  type t =
    | Relevance
    | Hot
    | Top
    | New
    | Comments

  let params_of_t t =
    [ ( "sort"
      , [ (match t with
          | Relevance -> "relevance"
          | Hot -> "hot"
          | Top -> "top"
          | New -> "new"
          | Comments -> "comments")
        ] )
    ]
  ;;
end

module Search_type = struct
  module T = struct
    type t =
      | Subreddit
      | Submission
      | User
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let to_string t =
    match t with
    | Subreddit -> "sr"
    | Submission -> "link"
    | User -> "user"
  ;;
end

module Link_type = struct
  type t =
    | Any
    | Link
    | Self

  let params_of_t t =
    [ ( "link_type"
      , [ (match t with
          | Any -> "any"
          | Link -> "link"
          | Self -> "self")
        ] )
    ]
  ;;
end

module Spam_level = struct
  type t =
    | Low
    | High
    | All

  let to_string t =
    match t with
    | Low -> "low"
    | High -> "high"
    | All -> "all"
  ;;
end

module Subreddit_type = struct
  type t =
    | Gold_restricted
    | Archived
    | Restricted
    | Employees_only
    | Gold_only
    | Private
    | User
    | Public

  let to_string t =
    match t with
    | Gold_restricted -> "gold_restricted"
    | Archived -> "archived"
    | Restricted -> "restricted"
    | Employees_only -> "employees_only"
    | Gold_only -> "gold_only"
    | Private -> "private"
    | User -> "user"
    | Public -> "public"
  ;;
end

module Wiki_mode = struct
  type t =
    | Disabled
    | Mod_only
    | Anyone

  let to_string t =
    match t with
    | Disabled -> "disabled"
    | Mod_only -> "modonly"
    | Anyone -> "anyone"
  ;;
end

module Stylesheet_operation = struct
  type t =
    | Save
    | Preview
  [@@deriving sexp]

  let to_string t =
    match t with
    | Save -> "save"
    | Preview -> "preview"
  ;;
end

module Subscription_action = struct
  type t =
    | Subscribe
    | Unsubscribe

  let to_string t =
    match t with
    | Subscribe -> "sub"
    | Unsubscribe -> "unsub"
  ;;
end

module Image_type = struct
  type t =
    | Png
    | Jpg

  let to_string t =
    match t with
    | Png -> "png"
    | Jpg -> "jpg"
  ;;
end

module Upload_type = struct
  type t =
    | Image
    | Header
    | Icon
    | Banner

  let to_string t =
    match t with
    | Image -> "img"
    | Header -> "header"
    | Icon -> "icon"
    | Banner -> "banner"
  ;;
end

module Subreddit_search_sort = struct
  type t =
    | Relevance
    | Activity

  let to_string t =
    match t with
    | Relevance -> "relevance"
    | Activity -> "activity"
  ;;
end

module Subreddit_relationship = struct
  type t =
    | Subscriber
    | Contributor
    | Moderator
    | Stream_subscriber

  let to_string t =
    match t with
    | Subscriber -> "subscriber"
    | Contributor -> "contributor"
    | Moderator -> "moderator"
    | Stream_subscriber -> "streams"
  ;;
end

module Subreddit_listing_sort = struct
  type t =
    | Popular
    | New
    | Gold
    | Default

  let to_string t =
    match t with
    | Popular -> "popular"
    | New -> "new"
    | Gold -> "gold"
    | Default -> "default"
  ;;
end

module User_subreddit_sort = struct
  type t =
    | Popular
    | New

  let to_string t =
    match t with
    | Popular -> "popular"
    | New -> "new"
  ;;
end

module Relationship = struct
  module Duration = struct
    type t =
      | Permanent
      | Days of int
    [@@deriving sexp]

    let params_of_t t =
      [ ( "duration"
        , match t with
          | Permanent -> []
          | Days days -> [ Int.to_string days ] )
      ]
    ;;
  end

  type t =
    | Friend
    | Moderator
    | Moderator_invite
    | Contributor
    | Banned
    | Muted
    | Wiki_banned
    | Wiki_contributor
  [@@deriving sexp]

  let to_string t =
    match t with
    | Friend -> "friend"
    | Moderator -> "moderator"
    | Moderator_invite -> "moderator_invite"
    | Contributor -> "contributor"
    | Banned -> "banned"
    | Muted -> "muted"
    | Wiki_banned -> "wikibanned"
    | Wiki_contributor -> "wikicontributor"
  ;;

  let params_of_t t = [ "type", [ to_string t ] ]
end

module Wiki_page = struct
  type t =
    { subreddit : Subreddit_name.t
    ; page : string
    }
  [@@deriving sexp]
end

module Add_or_remove = struct
  type t =
    | Add
    | Remove

  let to_string t =
    match t with
    | Add -> "add"
    | Remove -> "del"
  ;;
end
