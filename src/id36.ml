open! Core
include Id36_intf

module M = struct
  type t = int [@@deriving compare, hash, bin_io]

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
  let module_name = "Id36"
end

include M
include Identifiable.Make (M)
