((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Tue, 25 Aug 2020 18:57:06 GMT")(server snooserv)(set-cookie"edgebucket=uXejEp3ZnzLywa9p6C; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Get(uri((scheme(https))(host(oauth.reddit.com))(path /user/spez/about)(query((raw_json(1))))))(headers((authorization"bearer <ACCESS_TOKEN>")))))(response(((encoding(Fixed 1665))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 1665)(content-type"application/json; charset=UTF-8")(date"Tue, 25 Aug 2020 18:57:06 GMT")(expires -1)(server snooserv)(set-cookie"session_tracker=gpnfirdicnflqkfnkh.0.1598381826266.Z0FBQUFBQmZSVjhDZ3I1SG1kSmdrVTNLTUlJNHlHRTBQTnhDZmk4U3NHNmRRT1laZ1l4S2xvRGxEQnZiQzFkSXBNcmxjTWRDOE5GT2tsVzdRSHZ5d1V4MFVKZnlBVmdKNlJudmt5MFd6aW1Fd09iRThYVDZTemZKclhuRjE2MFpDdU1BUm4yd1NhQlc; Domain=reddit.com; Max-Age=7199; Path=/; expires=Tue, 25-Aug-2020 20:57:06 GMT; secure; SameSite=None; Secure")(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Fri, 25-Aug-2023 18:57:06 GMT; secure")(set-cookie"csv=1; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")(set-cookie"edgebucket=fLoNa0MsTJjHncA4Qd; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(vary accept-encoding)(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 599.0)(x-ratelimit-reset 174)(x-ratelimit-used 1)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"kind\": \"t2\", \"data\": {\"is_employee\": true, \"is_friend\": false, \"subreddit\": {\"default_set\": true, \"user_is_contributor\": false, \"banner_img\": \"https://b.thumbs.redditmedia.com/KWeEpVxXOGLoloMbM0IxGt9EiKPXizpwFgcSeWqtpZM.png\", \"restrict_posting\": true, \"user_is_banned\": false, \"free_form_reports\": true, \"community_icon\": null, \"show_media\": true, \"icon_color\": \"\", \"user_is_muted\": false, \"display_name\": \"u_spez\", \"header_img\": null, \"title\": \"spez\", \"previous_names\": [], \"over_18\": false, \"icon_size\": [256, 256], \"primary_color\": \"\", \"icon_img\": \"https://b.thumbs.redditmedia.com/d7Sw-O_AOlxEtKEeMTrH0xcy9LbLOy0j_KrWGbOFB9w.png\", \"description\": \"\", \"submit_link_label\": \"\", \"header_size\": null, \"restrict_commenting\": false, \"subscribers\": 0, \"submit_text_label\": \"\", \"is_default_icon\": false, \"link_flair_position\": \"\", \"display_name_prefixed\": \"u/spez\", \"key_color\": \"\", \"name\": \"t5_3k30p\", \"is_default_banner\": false, \"url\": \"/user/spez/\", \"banner_size\": [1280, 384], \"user_is_moderator\": false, \"public_description\": \"Reddit CEO\", \"link_flair_enabled\": false, \"disable_contributor_requests\": false, \"subreddit_type\": \"user\", \"user_is_subscriber\": false}, \"awardee_karma\": 62329, \"id\": \"1w72\", \"verified\": true, \"is_gold\": true, \"is_mod\": true, \"awarder_karma\": 625, \"has_verified_email\": true, \"icon_img\": \"https://b.thumbs.redditmedia.com/d7Sw-O_AOlxEtKEeMTrH0xcy9LbLOy0j_KrWGbOFB9w.png\", \"hide_from_robots\": false, \"link_karma\": 138988, \"pref_show_snoovatar\": false, \"total_karma\": 945841, \"accept_chats\": false, \"name\": \"spez\", \"created\": 1118059200.0, \"created_utc\": 1118030400.0, \"comment_karma\": 743899, \"has_subscribed\": true, \"accept_pms\": true}}")))