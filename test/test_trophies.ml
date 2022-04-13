open! Core
open! Async
open! Import

let%expect_test _ =
  with_cassette "trophies" ~f:(fun connection ->
      let%bind trophies = Connection.call_exn connection Endpoint.trophies in
      print_s [%message "" (trophies : Thing.Award.t list)];
      [%expect
        {|
        (trophies
         ((Object
           ((icon_70
             (String https://www.redditstatic.com/awards2/reddit_gold-70.png))
            (granted_at (Number 1609678143)) (url (String /premium))
            (icon_40
             (String https://www.redditstatic.com/awards2/reddit_gold-40.png))
            (name (String "Reddit Premium")) (award_id (String v))
            (id (String 2pgty2)) (description (String "Since January 2021"))))
          (Object
           ((icon_70
             (String https://www.redditstatic.com/awards2/3_year_club-70.png))
            (granted_at (Number 1585935110)) (url Null)
            (icon_40
             (String https://www.redditstatic.com/awards2/3_year_club-40.png))
            (name (String "Three-Year Club")) (award_id Null) (id Null)
            (description Null)))
          (Object
           ((icon_70
             (String https://www.redditstatic.com/awards2/verified_email-70.png))
            (granted_at Null) (url Null)
            (icon_40
             (String https://www.redditstatic.com/awards2/verified_email-40.png))
            (name (String "Verified Email")) (award_id (String o))
            (id (String 1qr5eq)) (description Null))))) |}];
      return ())
;;

let%expect_test _ =
  with_cassette "user_trophies" ~f:(fun connection ->
      let%bind trophies =
        Connection.call_exn
          connection
          (Endpoint.user_trophies ~username:(Username.of_string "spez"))
      in
      print_s [%message "" ~trophies:(List.take trophies 5 : Thing.Award.t list)];
      [%expect
        {|
        (trophies
         ((Object
           ((icon_70
             (String https://www.redditstatic.com/awards2/15_year_club-70.png))
            (granted_at (Number 1591416000)) (url Null)
            (icon_40
             (String https://www.redditstatic.com/awards2/15_year_club-40.png))
            (name (String "15-Year Club")) (award_id Null) (id Null)
            (description Null)))
          (Object
           ((icon_70 (String https://www.redditstatic.com/awards2/Argentium-70.png))
            (granted_at Null) (url Null)
            (icon_40 (String https://www.redditstatic.com/awards2/Argentium-40.png))
            (name (String "Argentium Club")) (award_id (String 33))
            (id (String 2mimqo)) (description Null)))
          (Object
           ((icon_70
             (String https://www.redditstatic.com/awards2/Wearing-is-Caring-70.png))
            (granted_at Null) (url Null)
            (icon_40
             (String https://www.redditstatic.com/awards2/Wearing-is-Caring-40.png))
            (name (String "Wearing is Caring")) (award_id (String 32))
            (id (String 2migfn)) (description Null)))
          (Object
           ((icon_70 (String https://www.redditstatic.com/awards2/100-awards-70.png))
            (granted_at Null) (url Null)
            (icon_40 (String https://www.redditstatic.com/awards2/100-awards-40.png))
            (name (String "100 Awards Club")) (award_id (String 31))
            (id (String 2m8tiw)) (description Null)))
          (Object
           ((icon_70
             (String https://www.redditstatic.com/awards2/inciteful_comment-70.png))
            (granted_at Null)
            (url
             (String
              /r/announcements/comments/gxas21/upcoming_changes_to_our_content_policy_our_board/ft0ekhk/?context=5#ft0ekhk))
            (icon_40
             (String https://www.redditstatic.com/awards2/inciteful_comment-40.png))
            (name (String "Inciteful Comment")) (award_id (String 8))
            (id (String 2houdv)) (description (String 2020-06-05)))))) |}];
      return ())
;;
