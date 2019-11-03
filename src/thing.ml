open! Core

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
          let char_from_base base offset =
            Char.to_int base + offset |> Char.of_int_exn
          in
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

module type Get_kind_module = functor (M : Thing_intf.Common) -> Sexpable

module By_kind (Get_kind_module : Get_kind_module) = struct
  module Comment' = Get_kind_module (Comment)
  module User' = Get_kind_module (User)
  module Link' = Get_kind_module (Link)
  module Message' = Get_kind_module (Message)
  module Subreddit' = Get_kind_module (Subreddit)
  module Award' = Get_kind_module (Award)

  type t =
    | Comment of Comment'.t
    | User of User'.t
    | Link of Link'.t
    | Message of Message'.t
    | Subreddit of Subreddit'.t
    | Award of Award'.t
  [@@deriving sexp]

  module Link_or_comment = struct
    type t =
      | Link of Link'.t
      | Comment of Comment'.t
    [@@deriving sexp]
  end

  module Link_or_comment_or_subreddit = struct
    type t =
      | Link of Link'.t
      | Comment of Comment'.t
      | Subreddit of Subreddit'.t
    [@@deriving sexp]
  end
end

include By_kind ((functor (M : Thing_intf.Common) -> M))
module Fullname = By_kind ((functor (M : Thing_intf.Common) -> M.Id36))

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
