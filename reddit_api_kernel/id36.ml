open! Core_kernel
include Id36_intf

module M = struct
  type t = Bigint.Stable.V1.t [@@deriving compare, bin_io]

  include (
    Bigint.Unstable :
      sig
        type nonrec t = t [@@deriving hash]
      end
      with type t := t)

  let of_bigint = ident
  let to_bigint = ident
  let base = 36
  let base_bigint = Bigint.of_int base

  let to_string (i : t) =
    let rec of_int i acc =
      match Bigint.equal i Bigint.zero with
      | true ->
        (match acc with
        | [] -> "0"
        | _ -> String.of_char_list acc)
      | false ->
        let current_place = Option.value_exn Bigint.O.(Bigint.to_int (i % base_bigint)) in
        let character =
          let char_from_base base offset = Char.to_int base + offset |> Char.of_int_exn in
          match current_place < 10 with
          | true -> char_from_base '0' current_place
          | false -> char_from_base 'a' (current_place - 10)
        in
        of_int Bigint.O.(i / base_bigint) (character :: acc)
    in
    of_int i []
  ;;

  let of_string t : t =
    let convert_char c =
      let convert_to_offset base_char = Char.to_int c - Char.to_int base_char in
      Bigint.of_int
        (match Char.is_alpha c with
        | true -> convert_to_offset 'a' + 10
        | false -> convert_to_offset '0')
    in
    String.fold t ~init:Bigint.zero ~f:(fun acc c ->
        Bigint.O.((acc * base_bigint) + convert_char c))
  ;;

  let sexp_of_t t = to_string t |> sexp_of_string
  let t_of_sexp sexp = String.t_of_sexp sexp |> of_string
  let module_name = "Id36"
end

include M
include Identifiable.Make (M)
