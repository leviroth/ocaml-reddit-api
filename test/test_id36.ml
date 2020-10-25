open! Core
open! Async
open! Import

let%expect_test "roundtrip: int -> string -> int" =
  Quickcheck.test
    Int.quickcheck_generator
    ~f:(fun n -> assert (Id36.to_int (Id36.of_int n) = n))
    ~examples:[ 85562; 18527; 61915; 6302; 60590; 2723; 97946; 78956; 48802; 84286 ]
    ~sexp_of:[%sexp_of: int];
  return ()
;;

let%expect_test "roundtrip: string -> int -> string" =
  let rec drop_unnecessary_zeroes_from_string string =
    match string with
    | "0" -> string
    | _ ->
      (match string.[0] with
      | '0' -> drop_unnecessary_zeroes_from_string (String.drop_prefix string 1)
      | _ -> string)
  in
  Quickcheck.test
    (Int.gen_incl 1 11
    |> Quickcheck.Generator.bind ~f:(fun length ->
           String.gen_with_length
             length
             (Quickcheck.Generator.union [ Char.gen_lowercase; Char.gen_digit ])))
    ~f:(fun string ->
      assert (
        String.equal
          (Id36.to_string (Id36.of_string string))
          (drop_unnecessary_zeroes_from_string string)))
    ~examples:
      [ "0"
      ; "10"
      ; "a1"
      ; "aklzj"
      ; "1o6fl"
      ; "wh6sj"
      ; "kldtp"
      ; "mmax3"
      ; "hqa43"
      ; "2p0qz"
      ; "nikcv"
      ; "8a93d"
      ]
    ~sexp_of:[%sexp_of: string];
  return ()
;;

let%expect_test "prefixes" =
  List.iter [ "0"; "a1"; "aklzj"; "t1_0"; "t1_a1"; "t1_aklzj" ] ~f:(fun test_case ->
      Thing.Comment.Id.of_string test_case |> [%sexp_of: Thing.Comment.Id.t] |> print_s);
  [%expect {|
    0
    a1
    aklzj
    0
    a1
    aklzj |}];
  return ()
;;
