open! Core

module type Thing_id = sig
  type t [@@deriving sexp]

  include Stringable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module Thing_id : Thing_id = struct
  type t = string

  let of_string s = s
  let to_string t = t
  let sexp_of_t t = to_string t |> sexp_of_string
  let t_of_sexp sexp = String.t_of_sexp sexp |> of_string
  let base = 36

  let of_int i =
    let rec of_int i acc =
      match i with
      | 0 -> String.of_char_list acc |> String.rev
      | _ ->
        let current_place = i mod base in
        let character =
          let offset_from_char char =
            Char.to_int char + current_place |> Char.of_int_exn
          in
          match current_place < 10 with
          | true -> offset_from_char '0'
          | false -> offset_from_char 'a'
        in
        of_int (i / base) (character :: acc)
    in
    of_int i []
  ;;

  let to_int t =
    let convert_char c =
      let convert_to_offset base_char = Char.to_int c - Char.to_int base_char in
      match Char.is_alpha c with
      | true -> convert_to_offset 'a'
      | false -> convert_to_offset '0'
    in
    String.fold t ~init:0 ~f:(fun acc c -> (acc * base) + convert_char c)
  ;;
end

module Submission_id = Thing_id
module User_id = Thing_id
module Comment_id = Thing_id

type t =
  | Comment of Comment_id.t
  | User of User_id.t
  | Submission of Submission_id.t

let of_string s =
  match String.lsplit2_exn s ~on:'_' with
  | "t1", rest -> Comment (Comment_id.of_string rest)
  | "t2", rest -> User (User_id.of_string rest)
  | "t3", rest -> Submission (Submission_id.of_string rest)
  | _ -> raise_s [%message "Unknown thing kind" s]
;;

let to_string t =
  match t with
  | Comment rest -> "t1_" ^ Thing_id.to_string rest
  | User rest -> "t2_" ^ Thing_id.to_string rest
  | Submission rest -> "t3_" ^ Thing_id.to_string rest
;;

let sexp_of_t t = to_string t |> sexp_of_string
let t_of_sexp sexp = String.t_of_sexp sexp |> of_string
