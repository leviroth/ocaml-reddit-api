((request(Post_form(uri https://www.reddit.com/api/v1/access_token)(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Fri, 28 Aug 2020 23:19:23 GMT")(server snooserv)(set-cookie"edgebucket=oYSUqWDWMyydexNwlz; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Get(uri https://oauth.reddit.com/prefs/friends?raw_json=1)(headers((authorization"bearer <ACCESS_TOKEN>")))))(response(((encoding(Fixed 174))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 174)(content-type"application/json; charset=UTF-8")(date"Fri, 28 Aug 2020 23:19:23 GMT")(expires -1)(server snooserv)(set-cookie"session_tracker=irqbgaomdqafgobrmc.0.1598656763941.Z0FBQUFBQmZTWkQ3VWppdlkyWThtaGJ6alV6ck03TUxDVlU0QVFkRVE1MHB4U3hERWdCSXcwQzZmY3JVZzI3dGVRTS1nZVdpY2sybHA3anJqdWRfS1N5bl9YZ3ZaYURubWZSbVgwbWdPWVZmZGU0QXUyNDhmbXhtX21JeXBHOXJKdnVQRVY4Y0I0VXU; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sat, 29-Aug-2020 01:19:23 GMT; secure; SameSite=None; Secure")(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Mon, 28-Aug-2023 23:19:23 GMT; secure")(set-cookie"csv=1; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")(set-cookie"edgebucket=k6WNhmNChGu7vGW0mu; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 599.0)(x-ratelimit-reset 37)(x-ratelimit-used 1)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"[{\"kind\": \"UserList\", \"data\": {\"children\": [{\"date\": 1598656681.0, \"rel_id\": \"r9_1voxr1\", \"name\": \"spez\", \"id\": \"t2_1w72\"}]}}, {\"kind\": \"UserList\", \"data\": {\"children\": []}}]")))