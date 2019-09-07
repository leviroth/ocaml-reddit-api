open! Core

let string_map_of_assoc_exn json =
  Yojson.Safe.Util.to_assoc json |> String.Map.of_alist_exn
;;

module Make (Id36 : sig
  include Id36_intf.S

  val to_fullname : t -> Fullname.t
end) =
struct
  type t = Json_derivers.Yojson.t String.Map.t [@@deriving sexp]

  let of_json = string_map_of_assoc_exn
  let to_json t = `Assoc (Map.to_alist t)
  let get_field = Map.find

  let username_of_field t ~field_name =
    let open Option.Monad_infix in
    get_field t field_name >>| Yojson.Safe.Util.to_string >>| Username.of_string
  ;;

  let time_of_field t ~field_name =
    let open Option.Monad_infix in
    get_field t field_name
    >>| Yojson.Safe.Util.to_float
    >>| Time.Span.of_sec
    >>| Time.of_span_since_epoch
  ;;

  let author t = username_of_field t ~field_name:"author"

  let moderation_info t =
    let approved_by = username_of_field t ~field_name:"approved_by" in
    let approved_at = time_of_field t ~field_name:"approved_at_utc" in
    let banned_by = username_of_field t ~field_name:"banned_by" in
    let banned_at = time_of_field t ~field_name:"banned_at_utc" in
    Moderation_info.of_listing_fields ~approved_by ~approved_at ~banned_by ~banned_at
  ;;

  let id t =
    get_field t "id"
    |> Option.map ~f:(Fn.compose Id36.of_string Yojson.Safe.Util.to_string)
  ;;

  let fullname t : Fullname.t option = id t |> Option.map ~f:Id36.to_fullname
end

module Comment = Make (struct
  include Id36.Comment

  let to_fullname t : Fullname.t = Comment t
end)

module User = Make (struct
  include Id36.User

  let to_fullname t : Fullname.t = User t
end)

module Link = Make (struct
  include Id36.Link

  let to_fullname t : Fullname.t = Link t
end)

module Message = Make (struct
  include Id36.Message

  let to_fullname t : Fullname.t = Message t
end)

module Subreddit = Make (struct
  include Id36.Subreddit

  let to_fullname t : Fullname.t = Subreddit t
end)

module Award = Make (struct
  include Id36.Award

  let to_fullname t : Fullname.t = Award t
end)

type t =
  | Comment of Comment.t
  | User of User.t
  | Link of Link.t
  | Message of Message.t
  | Subreddit of Subreddit.t
  | Award of Award.t
[@@deriving sexp]

let of_json json =
  let map = string_map_of_assoc_exn json in
  let kind =
    Map.find_exn map "kind" |> Yojson.Safe.Util.to_string |> Thing_kind.of_string
  in
  let data = Map.find_exn map "data" |> string_map_of_assoc_exn in
  match kind with
  | Comment -> Comment data
  | User -> User data
  | Link -> Link data
  | Message -> Message data
  | Subreddit -> Subreddit data
  | Award -> Award data
;;

let kind t : Thing_kind.t =
  match t with
  | Comment _ -> Comment
  | User _ -> User
  | Link _ -> Link
  | Message _ -> Message
  | Subreddit _ -> Subreddit
  | Award _ -> Award
;;

let data t : Json_derivers.Yojson.t String.Map.t =
  match t with
  | Comment data | User data | Link data | Message data | Subreddit data | Award data ->
    data
;;

let get_field t = Map.find (data t)

let to_json t : Yojson.Safe.t =
  let kind = kind t in
  let data = data t in
  `Assoc
    [ "kind", `String (Thing_kind.to_string kind); "data", `Assoc (Map.to_alist data) ]
;;

let fullname t =
  match t with
  | Comment comment -> Comment.fullname comment
  | User user -> User.fullname user
  | Link link -> Link.fullname link
  | Message message -> Message.fullname message
  | Subreddit subreddit -> Subreddit.fullname subreddit
  | Award award -> Award.fullname award
;;
