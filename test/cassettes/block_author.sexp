    ((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Thu, 03 Sep 2020 11:33:54 GMT")(server snooserv)(set-cookie"edgebucket=rjeIjIeCWvMie4u4zJ; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri((scheme(https))(host(oauth.reddit.com))(path /api/block)))(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(id(t4_rdjz4y))))))(response(((encoding(Fixed 2))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 2)(content-type"application/json; charset=UTF-8")(date"Thu, 03 Sep 2020 11:33:54 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Sun, 03-Sep-2023 11:33:54 GMT; secure")(set-cookie"session_tracker=fnadfbgfaepppghipg.0.1599132834170.Z0FBQUFBQmZVTlNpWEFmX3BBVm9fYVNZQmRsZV9GYTYtZ3dldkRPQU5ieTVpamVaZWhOZWIzNHlvZjF3enhOdXV6ZXJYdDk3OXFkTE1VOXROSDk0bGJSWmpxMkVGY0FQWDZOcnRoa2FBVGRzRVJzSTJsRWhsTGMyTWxXX0lWcVRMMl9YRDNKT283ZmY; Domain=reddit.com; Max-Age=7199; Path=/; expires=Thu, 03-Sep-2020 13:33:54 GMT; secure")(set-cookie"edgebucket=I9L5ILObFFCu4jqopM; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 599.0)(x-ratelimit-reset 366)(x-ratelimit-used 1)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false)){})))
