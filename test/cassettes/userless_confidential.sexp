((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(client_credentials))))))(response(((encoding(Fixed 109))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 109)(content-type"application/json; charset=UTF-8")(date"Sun, 04 Jul 2021 14:43:34 GMT")(server snooserv)(set-cookie"edgebucket=6PvaV8ZUigTi2URqyJ; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-clacks-overhead"GNU Terry Pratchett")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 298)(x-ratelimit-reset 386)(x-ratelimit-used 2)(x-reddit-loid 0000000000d3sjiamf.2.1625409814183.Z0FBQUFBQmc0Y2tXM0YtSURLejBMLVI0LTV3TEtIQU5jYnk5MWRUNEp1U081bGZvMkNlcl9WTGtsUW81S2tBUzAxWmdJc21ibTYwZEZfU0ROM3BsN2hpRGlJbkFSNV9SRlRIem5EelpWSFVJMjB0b2gyMnVGYXk1dmRWMmd2WjF3Vm9UZ2dvckVvcEw)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Get(uri((scheme(https))(host(oauth.reddit.com))(path /api/info)(query((raw_json(1))(id(t3_odlsl2))))))(headers((authorization"bearer <ACCESS_TOKEN>")))))(response(((encoding(Fixed 2619))(headers((accept-ranges bytes)(access-control-allow-origin *)(access-control-expose-headers X-Moose)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 2619)(content-type"application/json; charset=UTF-8")(date"Sun, 04 Jul 2021 14:43:34 GMT")(server snooserv)(set-cookie"loid=0000000000d3sjiamf.2.1625409814183.Z0FBQUFBQmc0Y2tXQlNuQWM3Y1BHT3F0VDJzWHlreFh4ZkhDekRZcnRsa0pFT1Y1VU1CSEFmRFo1LTU5RkZPaGhGeDhnT3hMOElXVDRjbXFKT0I5UVpZRDBBZnpwUWhxTXJ1NlhGLVYxbjlzclRFOXJZOWFkVG1qbXRxQ0RHUHhFdlFmRXFUUUN1UHk; Domain=reddit.com; Max-Age=63071999; Path=/; expires=Tue, 04-Jul-2023 14:43:34 GMT; secure; SameSite=None; Secure")(set-cookie"session_tracker=ggcjebdoppkhemkjbq.0.1625409814416.Z0FBQUFBQmc0Y2tXdHQ2LS1FVWxQZElqd1Itd2FPU0dqQjhGZ3VNZ3pQQ2dfUXI1bmUxaHlLRlNCelB1eGhGWmhDdGlHUHNTdTlPcDZLaW40RjI1S0ZuNVM0OEZiY2tYUUNnbnVKdVdwU21fZzJyOG5DYjVlS0l2WjFFRFhEd3ljLUpuTVI4ZG54NkE; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sun, 04-Jul-2021 16:43:34 GMT; secure; SameSite=None; Secure")(set-cookie"csv=1; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")(set-cookie"edgebucket=2yPWKFgxmou4JOEjdo; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(vary accept-encoding)(via"1.1 varnish")(x-clacks-overhead"GNU Terry Pratchett")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 299)(x-ratelimit-reset 386)(x-ratelimit-used 1)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"kind\": \"Listing\", \"data\": {\"after\": null, \"dist\": 1, \"modhash\": \"\", \"geo_filter\": \"\", \"children\": [{\"kind\": \"t3\", \"data\": {\"approved_at_utc\": null, \"subreddit\": \"test\", \"selftext\": \"\", \"author_fullname\": \"t2_ll32z\", \"saved\": false, \"mod_reason_title\": null, \"gilded\": 0, \"clicked\": false, \"title\": \"test\", \"link_flair_richtext\": [], \"subreddit_name_prefixed\": \"r/test\", \"hidden\": false, \"pwls\": 6, \"link_flair_css_class\": null, \"downs\": 0, \"thumbnail_height\": null, \"top_awarded_type\": null, \"hide_score\": true, \"name\": \"t3_odlsl2\", \"quarantine\": false, \"link_flair_text_color\": \"dark\", \"upvote_ratio\": 1.0, \"author_flair_background_color\": null, \"subreddit_type\": \"public\", \"ups\": 1, \"total_awards_received\": 0, \"media_embed\": {}, \"thumbnail_width\": null, \"author_flair_template_id\": null, \"is_original_content\": false, \"user_reports\": [], \"secure_media\": null, \"is_reddit_media_domain\": false, \"is_meta\": false, \"category\": null, \"secure_media_embed\": {}, \"link_flair_text\": null, \"can_mod_post\": false, \"score\": 1, \"approved_by\": null, \"is_created_from_ads_ui\": false, \"author_premium\": false, \"thumbnail\": \"self\", \"edited\": false, \"author_flair_css_class\": null, \"author_flair_richtext\": [], \"gildings\": {}, \"content_categories\": null, \"is_self\": true, \"mod_note\": null, \"created\": 1625438536.0, \"link_flair_type\": \"text\", \"wls\": 6, \"removed_by_category\": null, \"banned_by\": null, \"author_flair_type\": \"text\", \"domain\": \"self.test\", \"allow_live_comments\": false, \"selftext_html\": null, \"likes\": null, \"suggested_sort\": null, \"banned_at_utc\": null, \"view_count\": null, \"archived\": false, \"no_follow\": true, \"is_crosspostable\": false, \"pinned\": false, \"over_18\": false, \"all_awardings\": [], \"awarders\": [], \"media_only\": false, \"can_gild\": false, \"spoiler\": false, \"locked\": false, \"author_flair_text\": null, \"treatment_tags\": [], \"visited\": false, \"removed_by\": null, \"num_reports\": null, \"distinguished\": null, \"subreddit_id\": \"t5_2qh23\", \"mod_reason_by\": null, \"removal_reason\": null, \"link_flair_background_color\": \"\", \"id\": \"odlsl2\", \"is_robot_indexable\": true, \"report_reasons\": null, \"author\": \"nmtake\", \"discussion_type\": null, \"num_comments\": 0, \"send_replies\": false, \"whitelist_status\": \"all_ads\", \"contest_mode\": false, \"mod_reports\": [], \"author_patreon_flair\": false, \"author_flair_text_color\": null, \"permalink\": \"/r/test/comments/odlsl2/test/\", \"parent_whitelist_status\": \"all_ads\", \"stickied\": false, \"url\": \"https://www.reddit.com/r/test/comments/odlsl2/test/\", \"subreddit_subscribers\": 10572, \"created_utc\": 1625409736.0, \"num_crossposts\": 0, \"media\": null, \"is_video\": false}}], \"before\": null}}")))