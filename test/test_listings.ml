open! Core
open! Async
open! Import

let%expect_test "best" =
  with_cassette "best" ~f:(fun connection ->
      let%bind listing = Connection.call_exn connection (Endpoint.best ()) in
      let child_ids = Listing.children listing |> List.map ~f:Thing.Link.id in
      print_s [%sexp (child_ids : Thing.Link.Id.t list)];
      [%expect
        {|
        (ikj8uc ikuyq6 iksp32 ikpbi4 ikveil ikdwtu ikg3cx ikksvt ikrful iki288 ikpyqv
         ikl4g6 iknntd ikrrxo ikl05w ikn7gl iksu0i ikqhgj ikp413 ikpayi ikkxfh ikkwlk
         ikoad9 ikudf8 ikmxz9) |}];
      return ())
;;

let%expect_test "links_by_id" =
  with_cassette "links_by_id" ~f:(fun connection ->
      let links = List.map ~f:Thing.Link.Id.of_string [ "icqrut"; "ikksvt" ] in
      let%bind listing = Connection.call_exn connection (Endpoint.links_by_id ~links) in
      let ids = Listing.children listing |> List.map ~f:Thing.Link.id in
      print_s [%sexp (ids : Thing.Link.Id.t list)];
      [%expect {| (icqrut ikksvt) |}];
      return ())
;;

let%expect_test "random" =
  with_cassette "random" ~f:(fun connection ->
      let%bind link_id =
        Connection.call_exn
          connection
          (Endpoint.random ~subreddit:(Subreddit_name.of_string "ocaml") ())
      in
      print_s [%sexp (link_id : Thing.Link.Id.t)];
      [%expect {| feyhbv |}];
      return ())
;;

let%expect_test "user_overview" =
  with_cassette "user_overview" ~f:(fun connection ->
      let%bind listing =
        Connection.call_exn
          connection
          (Endpoint.user_overview () ~username:(Username.of_string "spez"))
      in
      let child_fullnames = Listing.children listing |> List.map ~f:Thing.Poly.fullname in
      print_s [%sexp (child_fullnames : Thing.Fullname.t list)];
      [%expect
        {|
        ((Comment hroelsq) (Comment hrdseug) (Comment hgsd3e4) (Comment hf0kd4r)
         (Comment hbhuafj) (Comment hbhpx8f) (Comment hbhmscu) (Link pbmy5y)
         (Comment h0xuw5y) (Comment gzof6q0) (Comment gvyvlig) (Link mcisdf)
         (Comment grivhb5) (Comment go1nkrg) (Comment go1ixbd) (Comment gmj9aya)
         (Comment glroko6) (Comment gl07p47) (Comment ggbcn1t) (Comment gf2hmo6)
         (Comment gdlk75z) (Comment gbjoobr) (Comment gbdoob3) (Comment ga80wbq)
         (Comment g9u720a)) |}];
      return ())
;;
