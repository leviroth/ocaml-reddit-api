((request(Post_form(uri https://www.reddit.com/api/v1/access_token)(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 117))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 117)(content-type"application/json; charset=UTF-8")(date"Sun, 10 Jan 2021 01:51:07 GMT")(server snooserv)(set-cookie"edgebucket=E0k1l1Fa3chzrUqHbp; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Get(uri https://oauth.reddit.com/api/v1/user/spez/trophies?raw_json=1)(headers((authorization"bearer <ACCESS_TOKEN>")))))(response(((encoding(Fixed 9261))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 9261)(content-type"application/json; charset=UTF-8")(date"Sun, 10 Jan 2021 01:51:08 GMT")(expires -1)(server snooserv)(set-cookie"session_tracker=bclpkggrbqlflelgib.0.1610243468033.Z0FBQUFBQmYtbDJNV3RyZDFvUzBaNUVxYWtqYzhORGlUeVpoNm81OFhHWjlIOENndnA4VlB0QjVLcC1ZZzhIZ25OWllNMTBmeC13X09NNzBvcDEzVDAwQ3NFVDdEYjhITW92eW5UakhHVXNtM1p4Y3NfMnJxaWJQeHF2MDQ3NXRwN3k0YmVBS2lnVnY; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sun, 10-Jan-2021 03:51:08 GMT; secure; SameSite=None; Secure")(set-cookie"csv=1; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")(set-cookie"edgebucket=CI3KT11CdhzDG0Cay5; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(vary accept-encoding)(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 599.0)(x-ratelimit-reset 532)(x-ratelimit-used 1)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"kind\": \"TrophyList\", \"data\": {\"trophies\": [{\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/15_year_club-70.png\", \"granted_at\": 1591416000, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/15_year_club-40.png\", \"name\": \"15-Year Club\", \"award_id\": null, \"id\": null, \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/Argentium-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/Argentium-40.png\", \"name\": \"Argentium Club\", \"award_id\": \"33\", \"id\": \"2mimqo\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/Wearing-is-Caring-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/Wearing-is-Caring-40.png\", \"name\": \"Wearing is Caring\", \"award_id\": \"32\", \"id\": \"2migfn\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/100-awards-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/100-awards-40.png\", \"name\": \"100 Awards Club\", \"award_id\": \"31\", \"id\": \"2m8tiw\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_comment-70.png\", \"granted_at\": null, \"url\": \"/r/announcements/comments/gxas21/upcoming_changes_to_our_content_policy_our_board/ft0ekhk/?context=5#ft0ekhk\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_comment-40.png\", \"name\": \"Inciteful Comment\", \"award_id\": \"8\", \"id\": \"2houdv\", \"description\": \"2020-06-05\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_link-70.png\", \"granted_at\": null, \"url\": \"/tb/gxas21\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_link-40.png\", \"name\": \"Inciteful Link\", \"award_id\": \"7\", \"id\": \"2hou19\", \"description\": \"2020-06-05\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/rpan-viewer-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/rpan-viewer-40.png\", \"name\": \"RPAN Viewer\", \"award_id\": \"2r\", \"id\": \"2865oz\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/not_forgotten-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/not_forgotten-40.png\", \"name\": \"Not Forgotten\", \"award_id\": \"2p\", \"id\": \"23y6pl\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/sequence_editor-70.png\", \"granted_at\": null, \"url\": \"https://www.reddit.com/r/sequence/\", \"icon_40\": \"https://www.redditstatic.com/awards2/sequence_editor-40.png\", \"name\": \"Sequence | Editor\", \"award_id\": \"2l\", \"id\": \"21ftyk\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/thanos_spared-70.png\", \"granted_at\": null, \"url\": \"https://www.reddit.com/r/thanosdidnothingwrong/\", \"icon_40\": \"https://www.redditstatic.com/awards2/thanos_spared-40.png\", \"name\": \"Spared\", \"award_id\": \"2b\", \"id\": \"1w20p2\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/alpha_user-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/alpha_user-40.png\", \"name\": \"Alpha Tester\", \"award_id\": \"29\", \"id\": \"1v9cjg\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_comment-70.png\", \"granted_at\": null, \"url\": \"/r/announcements/comments/827zqc/in_response_to_recent_reports_about_the_integrity/dv83xs6/?context=5#dv83xs6\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_comment-40.png\", \"name\": \"Inciteful Comment\", \"award_id\": \"8\", \"id\": \"1uf5vo\", \"description\": \"2018-03-05\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_link-70.png\", \"granted_at\": null, \"url\": \"/tb/827zqc\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_link-40.png\", \"name\": \"Inciteful Link\", \"award_id\": \"7\", \"id\": \"1uf5tz\", \"description\": \"2018-03-05\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/rgexchange-70.png\", \"granted_at\": null, \"url\": \"https://redditgifts.com/profiles/view/spez/\", \"icon_40\": \"https://www.redditstatic.com/awards2/rgexchange-40.png\", \"name\": \"redditgifts Exchanges\", \"award_id\": \"1k\", \"id\": \"1teu74\", \"description\": \"2 Exchanges\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_comment-70.png\", \"granted_at\": null, \"url\": \"/r/announcements/comments/59k22p/hey_its_reddits_totally_politically_neutral_ceo/d9929yb/?context=5#d9929yb\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_comment-40.png\", \"name\": \"Inciteful Comment\", \"award_id\": \"8\", \"id\": \"1izjfq\", \"description\": \"2016-10-26\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/beta_team-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/beta_team-40.png\", \"name\": \"Beta Team\", \"award_id\": \"w\", \"id\": \"1gh8e9\", \"description\": \"Reddit for iOS\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_link-70.png\", \"granted_at\": null, \"url\": \"/tb/4x35a3\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_link-40.png\", \"name\": \"Inciteful Link\", \"award_id\": \"7\", \"id\": \"1f5ifn\", \"description\": \"2016-08-10\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_comment-70.png\", \"granted_at\": null, \"url\": \"/r/announcements/comments/4ny59k/lets_talk_about_orlando/d480txj?context=5#d480txj\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_comment-40.png\", \"name\": \"Inciteful Comment\", \"award_id\": \"8\", \"id\": \"1c6kx9\", \"description\": \"2016-06-13\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_link-70.png\", \"granted_at\": null, \"url\": \"/tb/4ny59k\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_link-40.png\", \"name\": \"Inciteful Link\", \"award_id\": \"7\", \"id\": \"1c6kuc\", \"description\": \"2016-06-13\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_link-70.png\", \"granted_at\": null, \"url\": \"/tb/3fx2au\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_link-40.png\", \"name\": \"Inciteful Link\", \"award_id\": \"7\", \"id\": \"wt7cq\", \"description\": \"2015-08-05\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_link-70.png\", \"granted_at\": null, \"url\": \"/tb/3djjxw\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_link-40.png\", \"name\": \"Inciteful Link\", \"award_id\": \"7\", \"id\": \"w0w18\", \"description\": \"2015-07-16\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/inciteful_link-70.png\", \"granted_at\": null, \"url\": \"/tb/3dautm\", \"icon_40\": \"https://www.redditstatic.com/awards2/inciteful_link-40.png\", \"name\": \"Inciteful Link\", \"award_id\": \"7\", \"id\": \"vy4q0\", \"description\": \"2015-07-14\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/best_comment-70.png\", \"granted_at\": null, \"url\": \"/r/IAmA/comments/3cxedn/i_am_steve_huffman_the_new_ceo_of_reddit_ama/cszv2lg?context=5#cszv2lg\", \"icon_40\": \"https://www.redditstatic.com/awards2/best_comment-40.png\", \"name\": \"Best Comment\", \"award_id\": \"4\", \"id\": \"vt6s4\", \"description\": \"2015-07-11\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/best_comment-70.png\", \"granted_at\": null, \"url\": \"/r/announcements/comments/3cucye/an_old_team_at_reddit/csz1fte?context=5#csz1fte\", \"icon_40\": \"https://www.redditstatic.com/awards2/best_comment-40.png\", \"name\": \"Best Comment\", \"award_id\": \"4\", \"id\": \"vrpju\", \"description\": \"2015-07-10\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/reddit_gold-70.png\", \"granted_at\": null, \"url\": \"/gold/about\", \"icon_40\": \"https://www.redditstatic.com/awards2/reddit_gold-40.png\", \"name\": \"Reddit Premium\", \"award_id\": \"v\", \"id\": \"vr3vt\", \"description\": \"Since July 2015\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/mold-participant-70.png\", \"granted_at\": null, \"url\": \"http://blog.reddit.com/2011/03/reddit-mold-is-now-live.html\", \"icon_40\": \"https://www.redditstatic.com/awards2/mold-participant-40.png\", \"name\": \"reddit mold\", \"award_id\": \"15\", \"id\": \"1r0kj\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/represent-70.png\", \"granted_at\": null, \"url\": \"http://www.reddit.com/r/ColbertRally\", \"icon_40\": \"https://www.redditstatic.com/awards2/represent-40.png\", \"name\": \"Rally Monkey\", \"award_id\": \"11\", \"id\": \"12ahw\", \"description\": \"Century Club\"}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/verified_email-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/verified_email-40.png\", \"name\": \"Verified Email\", \"award_id\": \"o\", \"id\": \"4zhv\", \"description\": null}}, {\"kind\": \"t6\", \"data\": {\"icon_70\": \"https://www.redditstatic.com/awards2/combocommenter-70.png\", \"granted_at\": null, \"url\": null, \"icon_40\": \"https://www.redditstatic.com/awards2/combocommenter-40.png\", \"name\": \"ComboCommenter\", \"award_id\": \"6\", \"id\": \"12qg\", \"description\": \"2009-10-16\"}}]}}")))