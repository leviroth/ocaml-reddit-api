((request(Post_form(uri https://www.reddit.com/api/v1/access_token)(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Sun, 26 Jul 2020 21:18:42 GMT")(server snooserv)(set-cookie"edgebucket=5WSriPLjHQCSd884WS; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-cache MISS)(x-cache-hits 0)(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-served-by cache-bos4638-BOS)(x-timer S1595798323.602365,VS0,VE376)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri https://oauth.reddit.com/api/mod/conversations)(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(subject("Test subject"))(body("Test body"))(to(BJO_test_user))(srName(ThirdRealm))(isAuthorHidden(false))))))(response(((encoding(Fixed 1037))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 1037)(content-type"application/json; charset=UTF-8")(date"Sun, 26 Jul 2020 21:18:43 GMT")(expires -1)(server snooserv)(set-cookie"session_tracker=gpfelorklgppjkfnjr.0.1595798323106.Z0FBQUFBQmZIZk16YVdqZzNKM3NqSDJVVzlDMmpfX0tocTZ5REgyM0x3YV9lOGg2NkxENTU3YzBWMlpQY1B6VmZLbWtlekI2YjBmWFpRY3J1RHpqNXFZSGZtYXV4Z1gyRENOOUpMQWdpbjB2alRqdG96N1lLZ3Jva1hEWjY1emczLWZwMklQdjZ1ckg; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sun, 26-Jul-2020 23:18:43 GMT; secure")(set-cookie"edgebucket=21OTP44NjQWn9iugYP; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(vary accept-encoding)(via"1.1 varnish")(x-cache MISS)(x-cache-hits 0)(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 599.0)(x-ratelimit-reset 77)(x-ratelimit-used 1)(x-served-by cache-bos4634-BOS)(x-timer S1595798323.064277,VS0,VE317)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status Created)(flush false))"{\"conversation\": {\"isAuto\": false, \"objIds\": [{\"id\": \"osvgj\", \"key\": \"messages\"}], \"isRepliable\": true, \"lastUserUpdate\": null, \"isInternal\": false, \"lastModUpdate\": \"2020-07-26T21:18:43.146061+00:00\", \"lastUpdated\": \"2020-07-26T21:18:43.146061+00:00\", \"authors\": [{\"isMod\": true, \"isAdmin\": false, \"name\": \"L72_Elite_Kraken\", \"isOp\": true, \"isParticipant\": false, \"isHidden\": false, \"id\": 71814082, \"isDeleted\": false}], \"owner\": {\"displayName\": \"ThirdRealm\", \"type\": \"subreddit\", \"id\": \"t5_390u2\"}, \"id\": \"fsv44\", \"isHighlighted\": false, \"subject\": \"Test subject\", \"participant\": {}, \"state\": 0, \"lastUnread\": null, \"numMessages\": 1}, \"messages\": {\"osvgj\": {\"body\": \"<!-- SC_OFF --><div class=\\\"md\\\"><p>Test body</p>\\n</div><!-- SC_ON -->\", \"author\": {\"isMod\": true, \"isAdmin\": false, \"name\": \"L72_Elite_Kraken\", \"isOp\": true, \"isParticipant\": false, \"isHidden\": false, \"id\": 71814082, \"isDeleted\": false}, \"isInternal\": false, \"date\": \"2020-07-26T21:18:43.146061+00:00\", \"bodyMarkdown\": \"Test body\", \"id\": \"osvgj\"}}, \"modActions\": {}}")))