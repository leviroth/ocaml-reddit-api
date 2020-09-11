open! Core
open! Async
open! Import

let%expect_test "create_modmail_conversation" =
  with_cassette "create_modmail_conversation" ~f:(fun connection ->
      let%bind conversation =
        Api.Exn.create_modmail_conversation
          ~subject:"Test subject"
          ~body:"Test body"
          ~subreddit:(Subreddit_name.of_string "ThirdRealm")
          ~to_:(Username.of_string "BJO_test_user")
          ~hide_author:false
          connection
      in
      print_s [%sexp (conversation : Modmail.Conversation.t)];
      [%expect
        {|
        ((conversation
          (Object
           ((isAuto False)
            (objIds (Array ((Object ((id (String osvgj)) (key (String messages)))))))
            (isRepliable True) (lastUserUpdate Null) (isInternal False)
            (lastModUpdate (String 2020-07-26T21:18:43.146061+00:00))
            (lastUpdated (String 2020-07-26T21:18:43.146061+00:00))
            (authors
             (Array
              ((Object
                ((isMod True) (isAdmin False) (name (String L72_Elite_Kraken))
                 (isOp True) (isParticipant False) (isHidden False)
                 (id (Number 71814082)) (isDeleted False))))))
            (owner
             (Object
              ((displayName (String ThirdRealm)) (type (String subreddit))
               (id (String t5_390u2)))))
            (id (String fsv44)) (isHighlighted False)
            (subject (String "Test subject")) (participant (Object ()))
            (state (Number 0)) (lastUnread Null) (numMessages (Number 1)))))
         (messages
          (Object
           ((osvgj
             (Object
              ((body
                (String
                  "<!-- SC_OFF --><div class=\"md\"><p>Test body</p>\
                 \n</div><!-- SC_ON -->"))
               (author
                (Object
                 ((isMod True) (isAdmin False) (name (String L72_Elite_Kraken))
                  (isOp True) (isParticipant False) (isHidden False)
                  (id (Number 71814082)) (isDeleted False))))
               (isInternal False) (date (String 2020-07-26T21:18:43.146061+00:00))
               (bodyMarkdown (String "Test body")) (id (String osvgj))))))))
         (modActions (Object ()))) |}];
      return ())
;;
