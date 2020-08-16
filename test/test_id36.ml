open! Core
open! Import

let%expect_test "roundtrip: int -> string -> int" =
  let test_cases =
    [ 85562; 18527; 61915; 6302; 60590; 2723; 97946; 78956; 48802; 84286 ]
  in
  List.iter test_cases ~f:(fun test_case ->
      let id36 = Id36.of_int test_case in
      let int = Id36.to_int id36 in
      print_s [%message (test_case : int) (id36 : Id36.t) (int : int)]);
  [%expect
    {|
    ((test_case 85562) (id36 1u0q) (int 85562))
    ((test_case 18527) (id36 ean) (int 18527))
    ((test_case 61915) (id36 1brv) (int 61915))
    ((test_case 6302) (id36 4v2) (int 6302))
    ((test_case 60590) (id36 1ar2) (int 60590))
    ((test_case 2723) (id36 23n) (int 2723))
    ((test_case 97946) (id36 23kq) (int 97946))
    ((test_case 78956) (id36 1ox8) (int 78956))
    ((test_case 48802) (id36 11nm) (int 48802))
    ((test_case 84286) (id36 1t1a) (int 84286)) |}]
;;

let%expect_test "roundtrip: string -> int -> string" =
  let test_cases =
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
  in
  List.iter test_cases ~f:(fun test_case ->
      let id36 = Id36.of_string test_case in
      let int = Id36.to_int id36 in
      print_s [%message (test_case : string) (int : int) (id36 : Id36.t)]);
  [%expect
    {|
    ((test_case 0) (int 0) (id36 0))
    ((test_case 10) (int 36) (id36 10))
    ((test_case a1) (int 361) (id36 a1))
    ((test_case aklzj) (int 17757775) (id36 aklzj))
    ((test_case 1o6fl) (int 2807697) (id36 1o6fl))
    ((test_case wh6sj) (int 54549667) (id36 wh6sj))
    ((test_case kldtp) (int 34590013) (id36 kldtp))
    ((test_case mmax3) (int 37992135) (id36 mmax3))
    ((test_case hqa43) (int 29779635) (id36 hqa43))
    ((test_case 2p0qz) (int 4526603) (id36 2p0qz))
    ((test_case nikcv) (int 39497359) (id36 nikcv))
    ((test_case 8a93d) (int 13915273) (id36 8a93d)) |}]
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
    aklzj |}]
;;
