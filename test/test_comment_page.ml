open! Core
open! Async
open! Import

let%expect_test _ =
  let link = Thing.Link.Id.of_string "g7vyxy" in
  let%bind ({ comment_forest; _ } : Comment_response.t) =
    with_cassette "comments" ~f:(fun connection ->
        Connection.call_exn connection (Endpoint.comments () ~link))
  in
  let ids = List.map comment_forest ~f:Thing.Poly.fullname in
  print_s [%message "" (ids : Thing.Fullname.t list)];
  let%bind () =
    [%expect
      {|
    (ids
     ((Comment fojwyxf) (Comment fojwxw7) (Comment fojwi8c) (Comment fojwjxq)
      (Comment fojxm5x) (Comment fojwnxi) (Comment fojx1ud) (Comment fojx7j8)
      (Comment fojwiws) (Comment fojx78o) (Comment fojwkek) (Comment fojx2hj)
      (Comment fojwjwy) (Comment fojwmji) (Comment fojwi6q) (Comment fojwny9)
      (Comment fojwq6k) (Comment fojx8c2) (Comment fojwlfs) (Comment fojwol7)
      (Comment fok2q5c) (Comment fojz23e) (Comment fok11dz) (Comment fokhf2o)
      (Comment fojy5cq) (Comment fojx7rx) (Comment fojwktc) (Comment fojwift)
      (Comment fojwq7e) (Comment fojwqce) (Comment fojwk3e) (Comment fok2tyc)
      (Comment fojwue0) (Comment fojwiqa) (Comment fok23wm) (Comment fokmmen)
      (Comment fok1i42) (Comment fok10en) (Comment fojxc5l) (Comment fok9hr7)
      (Comment fok01r0) (Comment fojwlcf) (More_comments fokiubk))) |}];
    return ()
  in
  let more_comments =
    match List.last_exn comment_forest with
    | `More_comments more_comments ->
      (match Thing.More_comments.details more_comments with
      | By_children x -> x
      | By_parent _ -> assert false)
    | _ -> assert false
  in
  let%bind children =
    with_cassette "more_comments" ~f:(fun connection ->
        Connection.call_exn
          connection
          (Endpoint.more_children ~link ~more_comments ~sort:New ()))
  in
  let comments, more_comments =
    List.partition_map children ~f:(function
        | `Comment v -> First v
        | `More_comments v -> Second v)
  in
  let first_comment = List.hd comments in
  print_s
    [%message
      ""
        (first_comment : Thing.Comment.t option)
        (more_comments : Thing.More_comments.t list)];
  [%expect
    {|
    ((first_comment
      ((Object
        ((total_awards_received (Number 0)) (approved_at_utc Null) (edited False)
         (mod_reason_by Null) (banned_by Null)
         (author_flair_type (String richtext)) (removal_reason Null)
         (link_id (String t3_g7vyxy)) (author_flair_template_id Null)
         (likes Null) (replies (String "")) (user_reports (Array ()))
         (saved False) (id (String foosfwm)) (banned_at_utc Null)
         (mod_reason_title Null) (gilded (Number 0)) (archived False)
         (no_follow True) (author (String texans1234)) (can_mod_post False)
         (send_replies True) (parent_id (String t3_g7vyxy)) (score (Number 1))
         (author_fullname (String t2_dn2lt)) (approved_by Null) (mod_note Null)
         (all_awardings (Array ())) (subreddit_id (String t5_2qmg3))
         (body
          (String
           "Had they pulled the trigger last year they would have AT LEAST gotten a 1st round pick."))
         (awarders (Array ())) (downs (Number 0))
         (author_flair_css_class (String texans)) (name (String t1_foosfwm))
         (author_patreon_flair False) (collapsed False)
         (author_flair_richtext
          (Array ((Object ((e (String text)) (t (String Texans)))))))
         (is_submitter False)
         (body_html
          (String
            "<div class=\"md\"><p>Had they pulled the trigger last year they would have AT LEAST gotten a 1st round pick.</p>\
           \n</div>"))
         (gildings (Object ())) (collapsed_reason Null) (distinguished Null)
         (associated_award Null) (stickied False) (author_premium False)
         (can_gild True) (top_awarded_type Null)
         (author_flair_text_color (String dark)) (score_hidden False)
         (permalink
          (String
           /r/nfl/comments/g7vyxy/rapoport_the_redskins_have_agreed_to_terms_on_a/foosfwm/))
         (num_reports Null) (locked False) (report_reasons Null)
         (created (Number 1587974290.0)) (subreddit (String nfl))
         (author_flair_text (String Texans)) (treatment_tags (Array ()))
         (created_utc (Number 1587945490.0))
         (subreddit_name_prefixed (String r/nfl)) (controversiality (Number 0))
         (depth (Number 0)) (author_flair_background_color (String ""))
         (collapsed_because_crowd_control Null) (mod_reports (Array ()))
         (subreddit_type (String public)) (ups (Number 1))))))
     (more_comments
      ((Object
        ((count (Number 268)) (name (String t1_fojx728)) (id (String fojx728))
         (parent_id (String t3_g7vyxy)) (depth (Number 0))
         (children
          (Array
           ((String fojx728) (String fojy1aa) (String fok0tmx) (String fojx824)
            (String fojx6h9) (String fojx3pr) (String fojwidt) (String fojx446)
            (String fok0ro7) (String fojwp43) (String fojwis9) (String fojz38j)
            (String fojxxbq) (String fojwjzo) (String fojyed1) (String fojwx13)
            (String fojx7p5) (String fojwnk5) (String fojxwm9) (String fojwrwv)
            (String fojxt1t) (String fojzy3a) (String fojxi4w) (String fojzzoi)
            (String fojwtwj) (String fojwtic) (String fojwji4) (String fojwium)
            (String fojy0fx) (String fojwpyp) (String fojwkfo) (String fojwrjq)
            (String fok003g) (String fojxc3i) (String fojywzk) (String fojx7d3)
            (String fojwopj) (String fojwkul) (String fojwqs7) (String fojxpy0)
            (String fojwl9f) (String fojwof8) (String fok078m) (String fojy8j6)
            (String fojzf79) (String fojx8zc) (String fojwrqt) (String fojwrcn)
            (String fojwij2) (String fojwloy) (String fojzjkj) (String fojymdy)
            (String fojwlb7) (String fojx690) (String fojx3w7) (String fojwsfu)
            (String fojyqdc) (String fojwoe8) (String fojwx6q) (String fojxb0p)
            (String fojxkwf) (String fojy1w0) (String fojxams) (String fojwqvt)
            (String fojxzx7) (String fojz2n4) (String fojwnc3) (String fojx2qv)
            (String fojwzzl) (String fok09bm) (String fojx35g) (String fojy33v)
            (String fok0jm2) (String fojx8pg) (String fojx9mx) (String fojygka)
            (String fojx55s) (String fojxysp) (String fojym3v) (String fojx20l)
            (String fojww3a) (String fojww6h) (String fojwpz4) (String fojyq2m)
            (String fojx407) (String fojwmmi) (String fojwrde) (String fojzq1h)
            (String fojwknr) (String fojys2p) (String fojyhex) (String fojx60g)
            (String fojwreb) (String fojz7hz) (String fojwl2y) (String fojwo1k)
            (String fojyd30) (String fojymyl) (String fojymkf) (String fojzxz5)
            (String fojxadh) (String fojwj46) (String fok0og8) (String fojx6ff)
            (String fojwu6n) (String fojwnv5) (String fojy1qn) (String fojxxp9)
            (String fojwrfi) (String fojwoo6) (String fok0ezs) (String fojwr24)
            (String fok054f) (String fojwmpf) (String fojyq5y) (String fojwl4n)
            (String fojyygq) (String fojx4vw))))))))) |}];
  return ()
;;
