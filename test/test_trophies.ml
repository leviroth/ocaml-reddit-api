open! Core
open! Async
open! Import

let%expect_test _ =
  with_cassette "trophies" ~f:(fun connection ->
      let%bind trophies = Connection.call_exn connection (Api.trophies ()) in
      print_s [%message "" (trophies : Thing.Award.t list)];
      [%expect
        {|
        (trophies
         (((award_id (String v)) (description (String "Since January 2021"))
           (granted_at (Float 1609678143))
           (icon_40 (String https://www.redditstatic.com/awards2/reddit_gold-40.png))
           (icon_70 (String https://www.redditstatic.com/awards2/reddit_gold-70.png))
           (id (String 2pgty2)) (name (String "Reddit Premium"))
           (url (String /premium)))
          ((award_id Null) (description Null) (granted_at (Float 1585935110))
           (icon_40 (String https://www.redditstatic.com/awards2/3_year_club-40.png))
           (icon_70 (String https://www.redditstatic.com/awards2/3_year_club-70.png))
           (id Null) (name (String "Three-Year Club")) (url Null))
          ((award_id (String o)) (description Null) (granted_at Null)
           (icon_40
            (String https://www.redditstatic.com/awards2/verified_email-40.png))
           (icon_70
            (String https://www.redditstatic.com/awards2/verified_email-70.png))
           (id (String 1qr5eq)) (name (String "Verified Email")) (url Null)))) |}];
      return ())
;;

let%expect_test _ =
  with_cassette "user_trophies" ~f:(fun connection ->
      let%bind trophies =
        Connection.call_exn
          connection
          (Api.user_trophies ~username:(Username.of_string "spez") ())
      in
      print_s [%message "" ~trophies:(List.take trophies 5 : Thing.Award.t list)];
      [%expect
        {|
        (trophies
         (((award_id Null) (description Null) (granted_at (Float 1591416000))
           (icon_40
            (String https://www.redditstatic.com/awards2/15_year_club-40.png))
           (icon_70
            (String https://www.redditstatic.com/awards2/15_year_club-70.png))
           (id Null) (name (String "15-Year Club")) (url Null))
          ((award_id (String 33)) (description Null) (granted_at Null)
           (icon_40 (String https://www.redditstatic.com/awards2/Argentium-40.png))
           (icon_70 (String https://www.redditstatic.com/awards2/Argentium-70.png))
           (id (String 2mimqo)) (name (String "Argentium Club")) (url Null))
          ((award_id (String 32)) (description Null) (granted_at Null)
           (icon_40
            (String https://www.redditstatic.com/awards2/Wearing-is-Caring-40.png))
           (icon_70
            (String https://www.redditstatic.com/awards2/Wearing-is-Caring-70.png))
           (id (String 2migfn)) (name (String "Wearing is Caring")) (url Null))
          ((award_id (String 31)) (description Null) (granted_at Null)
           (icon_40 (String https://www.redditstatic.com/awards2/100-awards-40.png))
           (icon_70 (String https://www.redditstatic.com/awards2/100-awards-70.png))
           (id (String 2m8tiw)) (name (String "100 Awards Club")) (url Null))
          ((award_id (String 8)) (description (String 2020-06-05)) (granted_at Null)
           (icon_40
            (String https://www.redditstatic.com/awards2/inciteful_comment-40.png))
           (icon_70
            (String https://www.redditstatic.com/awards2/inciteful_comment-70.png))
           (id (String 2houdv)) (name (String "Inciteful Comment"))
           (url
            (String
             /r/announcements/comments/gxas21/upcoming_changes_to_our_content_policy_our_board/ft0ekhk/?context=5#ft0ekhk))))) |}];
      return ())
;;
