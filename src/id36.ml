open! Core

module T = struct
  type t = int

  let of_int = Fn.id
  let to_int = Fn.id
  let base = 36

  let to_string i =
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

  let of_string t =
    let convert_char c =
      let convert_to_offset base_char = Char.to_int c - Char.to_int base_char in
      match Char.is_alpha c with
      | true -> convert_to_offset 'a'
      | false -> convert_to_offset '0'
    in
    String.fold t ~init:0 ~f:(fun acc c -> (acc * base) + convert_char c)
  ;;

  let sexp_of_t t = to_string t |> sexp_of_string
  let t_of_sexp sexp = String.t_of_sexp sexp |> of_string
end

module Comment = T
module User = T
module Submission = T
module Message = T
module Subreddit = T
module Award = T
module More_children = T
module Modmail_conversation = T
