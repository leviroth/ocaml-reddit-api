    ((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Sun, 25 Oct 2020 16:24:37 GMT")(server snooserv)(set-cookie"edgebucket=bkNNNsUvaD4cMNVybt; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Get(uri((scheme(https))(host(oauth.reddit.com))(path /r/ThirdRealm/wiki/settings/index)(query((raw_json(1))))))(headers((authorization"bearer <ACCESS_TOKEN>")))))(response(((encoding(Fixed 1667))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 1667)(content-type"application/json; charset=UTF-8")(date"Sun, 25 Oct 2020 16:24:37 GMT")(expires -1)(server snooserv)(set-cookie"session_tracker=bpjfngqoilidhcmlla.0.1603643077735.Z0FBQUFBQmZsYWJGZDV5ZmNneEY5N2Itb21rVlVQQW9rbXRmNl9RVDAzQ1ZWMlV2X2xVaWwzc0FNMl9nSkJoOUpOb2F1T0RpMVNLWU5Ca1JuTmlVU3lGbVJqSmJISml0TXpTSjFBZTA2RzhjbUJzQlBCNlItOGhpTUFUR3FjdE1HUFQxNFN0UDYyeG4; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sun, 25-Oct-2020 18:24:37 GMT; secure; SameSite=None; Secure")(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Wed, 25-Oct-2023 16:24:37 GMT; secure")(set-cookie"csv=1; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")(set-cookie"edgebucket=CFqeM8I4WgXvX6fjs3; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(vary accept-encoding)(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 596.0)(x-ratelimit-reset 323)(x-ratelimit-used 4)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"kind\": \"wikipagesettings\", \"data\": {\"permlevel\": 1, \"editors\": [{\"kind\": \"t2\", \"data\": {\"is_employee\": false, \"accept_chats\": false, \"icon_img\": \"https://www.redditstatic.com/avatars/avatar_default_03_FF8717.png\", \"pref_show_snoovatar\": false, \"name\": \"L72_Elite_Kraken\", \"is_friend\": false, \"created\": 1491269510.0, \"hide_from_robots\": false, \"verified\": true, \"created_utc\": 1491240710.0, \"snoovatar_img\": \"\", \"subreddit\": {\"default_set\": true, \"user_is_contributor\": false, \"banner_img\": \"\", \"restrict_posting\": true, \"user_is_banned\": false, \"free_form_reports\": true, \"community_icon\": null, \"show_media\": true, \"icon_color\": \"#FF8717\", \"user_is_muted\": false, \"display_name\": \"u_L72_Elite_Kraken\", \"header_img\": null, \"title\": \"\", \"previous_names\": [], \"over_18\": false, \"icon_size\": [256, 256], \"primary_color\": \"\", \"icon_img\": \"https://www.redditstatic.com/avatars/avatar_default_03_FF8717.png\", \"description\": \"\", \"submit_link_label\": \"\", \"header_size\": null, \"restrict_commenting\": false, \"subscribers\": 0, \"submit_text_label\": \"\", \"is_default_icon\": true, \"link_flair_position\": \"\", \"display_name_prefixed\": \"u/L72_Elite_Kraken\", \"key_color\": \"\", \"name\": \"t5_16q2ya\", \"is_default_banner\": true, \"url\": \"/user/L72_Elite_Kraken/\", \"quarantine\": false, \"banner_size\": null, \"user_is_moderator\": false, \"public_description\": \"\", \"link_flair_enabled\": false, \"disable_contributor_requests\": false, \"subreddit_type\": \"user\", \"user_is_subscriber\": false}, \"comment_karma\": 497, \"is_gold\": false, \"is_mod\": true, \"has_subscribed\": true, \"snoovatar_size\": null, \"link_karma\": 36, \"has_verified_email\": true, \"id\": \"16r83m\", \"accept_pms\": true}}], \"listed\": true}}")))