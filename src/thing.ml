open! Core
include Thing_intf

module Id36 = struct
  type t = int

  let of_int = Fn.id
  let to_int = Fn.id
  let base = 36

  let to_string i =
    let rec of_int i acc =
      match i with
      | 0 ->
        (match acc with
        | [] -> "0"
        | _ -> String.of_char_list acc)
      | _ ->
        let current_place = i mod base in
        let character =
          let char_from_base base offset = Char.to_int base + offset |> Char.of_int_exn in
          match current_place < 10 with
          | true -> char_from_base '0' current_place
          | false -> char_from_base 'a' (current_place - 10)
        in
        of_int (i / base) (character :: acc)
    in
    of_int i []
  ;;

  let of_string t =
    let convert_char c =
      let convert_to_offset base_char = Char.to_int c - Char.to_int base_char in
      match Char.is_alpha c with
      | true -> convert_to_offset 'a' + 10
      | false -> convert_to_offset '0'
    in
    String.fold t ~init:0 ~f:(fun acc c -> (acc * base) + convert_char c)
  ;;

  let sexp_of_t t = to_string t |> sexp_of_string
  let t_of_sexp sexp = String.t_of_sexp sexp |> of_string
end

let string_map_of_assoc_exn json =
  Yojson.Safe.Util.to_assoc json |> String.Map.of_alist_exn
;;

module M = struct
  type t = Json_derivers.Yojson.t String.Map.t [@@deriving sexp]

  module Id36 = Id36

  let of_json = string_map_of_assoc_exn
  let to_json t = `Assoc (Map.to_alist t)
  let get_field = Map.find

  let id36 t =
    get_field t "id"
    |> Option.map ~f:(Fn.compose Id36.of_string Yojson.Safe.Util.to_string)
  ;;
end

module Comment = M
module User = M
module Link = M
module Message = M
module Subreddit = M
module Award = M
module More_comments = M
module Modmail_conversation = M

module type Get_kind_module = functor (M : S) -> Sexpable

module type Per_kind = sig
  module F (M : S) : T

  type comment = [ `Comment of F(Comment).t ] [@@deriving sexp]
  type user = [ `User of F(User).t ] [@@deriving sexp]
  type link = [ `Link of F(Link).t ] [@@deriving sexp]
  type message = [ `Message of F(Message).t ] [@@deriving sexp]
  type subreddit = [ `Subreddit of F(Subreddit).t ] [@@deriving sexp]
  type award = [ `Award of F(Award).t ] [@@deriving sexp]
  type more_comments = [ `More_comments of F(More_comments).t ] [@@deriving sexp]

  type modmail_conversation = [ `Modmail_conversation of F(Modmail_conversation).t ]
  [@@deriving sexp]

  type t =
    [ comment
    | user
    | link
    | message
    | subreddit
    | award
    | more_comments
    | modmail_conversation
    ]
  [@@deriving sexp]
end

module Per_kind (F : functor (M : S) -> Sexpable.S) : Per_kind with module F := F = struct
  module Comment = F (Comment)
  module User = F (User)
  module Link = F (Link)
  module Message = F (Message)
  module Subreddit = F (Subreddit)
  module Award = F (Award)
  module More_comments = F (More_comments)
  module Modmail_conversation = F (Modmail_conversation)

  type comment = [ `Comment of Comment.t ] [@@deriving sexp]
  type user = [ `User of User.t ] [@@deriving sexp]
  type link = [ `Link of Link.t ] [@@deriving sexp]
  type message = [ `Message of Message.t ] [@@deriving sexp]
  type subreddit = [ `Subreddit of Subreddit.t ] [@@deriving sexp]
  type award = [ `Award of Award.t ] [@@deriving sexp]
  type more_comments = [ `More_comments of More_comments.t ] [@@deriving sexp]

  type modmail_conversation = [ `Modmail_conversation of Modmail_conversation.t ]
  [@@deriving sexp]

  type t =
    [ comment
    | user
    | link
    | message
    | subreddit
    | award
    | more_comments
    | modmail_conversation
    ]
  [@@deriving sexp]
end

include Per_kind (Projectors.Ident)

let of_json json =
  let map = string_map_of_assoc_exn json in
  let kind =
    Map.find_exn map "kind" |> Yojson.Safe.Util.to_string |> Thing_kind.of_string
  in
  let data = Map.find_exn map "data" |> string_map_of_assoc_exn in
  match kind with
  | Comment -> `Comment data
  | User -> `User data
  | Link -> `Link data
  | Message -> `Message data
  | Subreddit -> `Subreddit data
  | Award -> `Award data
  | More_comments -> `More_comments data
  | Modmail_conversation -> `Modmail_conversation data
;;

let kind t : Thing_kind.t =
  match t with
  | `Comment _ -> Comment
  | `User _ -> User
  | `Link _ -> Link
  | `Message _ -> Message
  | `Subreddit _ -> Subreddit
  | `Award _ -> Award
  | `More_comments _ -> More_comments
  | `Modmail_conversation _ -> Modmail_conversation
;;

let data t : Json_derivers.Yojson.t String.Map.t =
  match t with
  | `Comment data
  | `User data
  | `Link data
  | `Message data
  | `Subreddit data
  | `Award data
  | `More_comments data
  | `Modmail_conversation data -> data
;;

let get_field t = Map.find (data t)

let to_json t : Yojson.Safe.t =
  let kind = kind t in
  let data = data t in
  `Assoc
    [ "kind", `String (Thing_kind.to_string kind); "data", `Assoc (Map.to_alist data) ]
;;

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

module Fullname = struct
  module T = struct
    include Per_kind (Projectors.Id36)

    let of_string s =
      let kind, id = String.lsplit2_exn s ~on:'_' in
      let id = Id36.of_string id in
      match Thing_kind.of_string kind with
      | Comment -> `Comment id
      | User -> `User id
      | Link -> `Link id
      | Message -> `Message id
      | Subreddit -> `Subreddit id
      | Award -> `Award id
      | More_comments -> `More_comments id
      | Modmail_conversation -> `Modmail_conversation id
    ;;

    let to_string t =
      let (thing_kind, id36_string) : Thing_kind.t * string =
        match t with
        | `Comment id36 -> Comment, Id36.to_string id36
        | `User id36 -> User, Id36.to_string id36
        | `Link id36 -> Link, Id36.to_string id36
        | `Message id36 -> Message, Id36.to_string id36
        | `Subreddit id36 -> Subreddit, Id36.to_string id36
        | `Award id36 -> Award, Id36.to_string id36
        | `More_comments id36 -> More_comments, Id36.to_string id36
        | `Modmail_conversation id36 -> Modmail_conversation, Id36.to_string id36
      in
      sprintf !"%{Thing_kind}_%s" thing_kind id36_string
    ;;
  end

  include T
  include Sexpable.Of_stringable (T)
end
