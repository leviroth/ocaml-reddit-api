    ((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Thu, 10 Sep 2020 22:27:16 GMT")(server snooserv)(set-cookie"edgebucket=dhnjLZhbhKAu7kmgMk; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri((scheme(https))(host(oauth.reddit.com))(path /api/leavecontributor)))(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(id(t5_390u2))))))(response(((encoding(Fixed 2))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 2)(content-type"application/json; charset=UTF-8")(date"Thu, 10 Sep 2020 22:27:16 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Sun, 10-Sep-2023 22:27:16 GMT; secure")(set-cookie"session_tracker=bnnjnfjfmedpilapkf.0.1599776836415.Z0FBQUFBQmZXcWhFNnBidXZLSEdZWVNxVEh2MDdjeklkaEhBX3hzTlhCMjJsQTl4TGtwVXVnNVUyZUxOZ2xSTlgzTDUySW03RjlkM2RVNE5qdnBvaDBuQjNDVFdWNENiS1hxTU1haG5lT0dabzd4a3F6dDViMWd3YWVDTWExZjhSLURvRFJ3NHlIeGU; Domain=reddit.com; Max-Age=7199; Path=/; expires=Fri, 11-Sep-2020 00:27:16 GMT; secure")(set-cookie"edgebucket=wYrxeIjiAZP92UqcFU; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 598.0)(x-ratelimit-reset 164)(x-ratelimit-used 2)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false)){})))