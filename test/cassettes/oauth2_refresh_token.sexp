    ((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(refresh_token))(refresh_token(<REFRESH_TOKEN>))))))(response(((encoding(Fixed 180))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 180)(content-type"application/json; charset=UTF-8")(date"Sun, 18 Jul 2021 12:56:44 GMT")(server snooserv)(set-cookie"edgebucket=9s262teSBMHPbhzTHC; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-clacks-overhead"GNU Terry Pratchett")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 299)(x-ratelimit-reset 196)(x-ratelimit-used 1)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"refresh_token\": \"<REFRESH_TOKEN>\", \"scope\": \"read\"}")))((request(Get(uri((scheme(https))(host(oauth.reddit.com))(path /api/info)(query((raw_json(1))(id(t3_odlsl2))))))(headers((authorization"bearer <ACCESS_TOKEN>")))))(response(((encoding(Fixed 2566))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 2566)(content-type"application/json; charset=UTF-8")(date"Sun, 18 Jul 2021 12:56:44 GMT")(expires -1)(server snooserv)(set-cookie"session_tracker=hginijhhqkejjahrkh.0.1626613004460.Z0FBQUFBQmc5Q1VNOGI1Q3JEcTYzcEgzQ3NiSHJwbXBiZEYyMHhoWVRxMTRud2pxcTAzOUhZYzFjNzlSQmxDdW8tWGYyUTQ4TUlEeFpLbXRHQjB6TFVLbjgzM245UTJfbUJ6TWxKVTdUNXlsRkZqTEIxb1lxM1Q4VzRlVFV6NmNFRzByaEd0TFdsNGs; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sun, 18-Jul-2021 14:56:44 GMT; secure; SameSite=None; Secure")(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Wed, 17-Jul-2024 12:56:44 GMT; secure")(set-cookie"csv=1; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")(set-cookie"edgebucket=gmAP1iIGNHYTA0xjpI; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(vary accept-encoding)(via"1.1 varnish")(x-clacks-overhead"GNU Terry Pratchett")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 599.0)(x-ratelimit-reset 196)(x-ratelimit-used 1)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"kind\": \"Listing\", \"data\": {\"after\": null, \"dist\": 1, \"modhash\": null, \"geo_filter\": \"\", \"children\": [{\"kind\": \"t3\", \"data\": {\"approved_at_utc\": null, \"subreddit\": \"test\", \"selftext\": \"\", \"author_fullname\": \"t2_ll32z\", \"saved\": false, \"mod_reason_title\": null, \"gilded\": 0, \"clicked\": false, \"title\": \"test\", \"link_flair_richtext\": [], \"subreddit_name_prefixed\": \"r/test\", \"hidden\": false, \"pwls\": 6, \"link_flair_css_class\": null, \"downs\": 0, \"top_awarded_type\": null, \"hide_score\": false, \"name\": \"t3_odlsl2\", \"quarantine\": false, \"link_flair_text_color\": \"dark\", \"upvote_ratio\": 1.0, \"author_flair_background_color\": null, \"subreddit_type\": \"public\", \"ups\": 2, \"total_awards_received\": 0, \"media_embed\": {}, \"author_flair_template_id\": null, \"is_original_content\": false, \"user_reports\": [], \"secure_media\": null, \"is_reddit_media_domain\": false, \"is_meta\": false, \"category\": null, \"secure_media_embed\": {}, \"link_flair_text\": null, \"can_mod_post\": false, \"score\": 2, \"approved_by\": null, \"is_created_from_ads_ui\": false, \"author_premium\": false, \"thumbnail\": \"\", \"edited\": false, \"author_flair_css_class\": null, \"author_flair_richtext\": [], \"gildings\": {}, \"content_categories\": null, \"is_self\": true, \"mod_note\": null, \"created\": 1625409736.0, \"link_flair_type\": \"text\", \"wls\": 6, \"removed_by_category\": null, \"banned_by\": null, \"author_flair_type\": \"text\", \"domain\": \"self.test\", \"allow_live_comments\": false, \"selftext_html\": null, \"likes\": null, \"suggested_sort\": null, \"banned_at_utc\": null, \"view_count\": null, \"archived\": false, \"no_follow\": false, \"is_crosspostable\": true, \"pinned\": false, \"over_18\": false, \"all_awardings\": [], \"awarders\": [], \"media_only\": false, \"can_gild\": true, \"spoiler\": false, \"locked\": false, \"author_flair_text\": null, \"treatment_tags\": [], \"visited\": false, \"removed_by\": null, \"num_reports\": null, \"distinguished\": null, \"subreddit_id\": \"t5_2qh23\", \"mod_reason_by\": null, \"removal_reason\": null, \"link_flair_background_color\": \"\", \"id\": \"odlsl2\", \"is_robot_indexable\": true, \"report_reasons\": null, \"author\": \"nmtake\", \"discussion_type\": null, \"num_comments\": 3, \"send_replies\": false, \"whitelist_status\": \"all_ads\", \"contest_mode\": false, \"mod_reports\": [], \"author_patreon_flair\": false, \"author_flair_text_color\": null, \"permalink\": \"/r/test/comments/odlsl2/test/\", \"parent_whitelist_status\": \"all_ads\", \"stickied\": false, \"url\": \"https://www.reddit.com/r/test/comments/odlsl2/test/\", \"subreddit_subscribers\": 10644, \"created_utc\": 1625409736.0, \"num_crossposts\": 0, \"media\": null, \"is_video\": false}}], \"before\": null}}")))