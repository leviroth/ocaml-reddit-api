((request(Post_form(uri https://www.reddit.com/api/v1/access_token)(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Tue, 01 Sep 2020 12:47:50 GMT")(server snooserv)(set-cookie"edgebucket=xpShHXyQVcPBDZlCLD; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri https://oauth.reddit.com/api/save)(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(id(t1_g3krlj5))))))(response(((encoding(Fixed 2))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 2)(content-type"application/json; charset=UTF-8")(date"Tue, 01 Sep 2020 12:47:50 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Fri, 01-Sep-2023 12:47:50 GMT; secure")(set-cookie"session_tracker=dckejhajljhlachcin.0.1598964470591.Z0FBQUFBQmZUa0wyZGNlS0l0ZUpWZlhBS3pVQWhGMmpRcDdXVml0Sm9XWVM0ZFA3SDliM1k0QUFnRGdBeFNKaE1WQ0NvN3VsdmRPQ2xCdDRMem53ZVUycWZVT2FYTGR5dWlScVpSbjRzbzlHTGZjRGtkeXBHWTlBdlJyNjJCa0Y0VV9xcWUxTUNpV1Y; Domain=reddit.com; Max-Age=7199; Path=/; expires=Tue, 01-Sep-2020 14:47:50 GMT; secure")(set-cookie"edgebucket=HFIVm8onM91PTcZoHy; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 598.0)(x-ratelimit-reset 130)(x-ratelimit-used 2)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false)){})))((request(Post_form(uri https://oauth.reddit.com/api/save)(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(id(t1_g3krlj5))))))(response(((encoding(Fixed 2))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 2)(content-type"application/json; charset=UTF-8")(date"Tue, 01 Sep 2020 12:47:50 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Fri, 01-Sep-2023 12:47:50 GMT; secure")(set-cookie"session_tracker=ebgrmarqeaiqnbbdai.0.1598964470729.Z0FBQUFBQmZUa0wydy1VQ1Awd1lvZDdrZ2ZpU2lZZWdxN3Bua3Ezcjl2dW1ySG9vYllIWWh0WTMtM1ExOGRRTm5GZkI5aFZGZ1FyNnQ0RmVyX25OZ2JVOWh3WVNhc3gydEpUY3ZYR0ZxTk9ta3E1R0FCR2VKbWsxdTJpZ0E1QzRhNG94UTBHUkN5aUI; Domain=reddit.com; Max-Age=7199; Path=/; expires=Tue, 01-Sep-2020 14:47:50 GMT; secure")(set-cookie"edgebucket=vWYTIm3uJ3zPRxEoUW; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 597.0)(x-ratelimit-reset 130)(x-ratelimit-used 3)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false)){})))