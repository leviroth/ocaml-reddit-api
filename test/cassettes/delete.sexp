((request(Post_form(uri https://www.reddit.com/api/v1/access_token)(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Mon, 31 Aug 2020 01:08:02 GMT")(server snooserv)(set-cookie"edgebucket=5f9obgvEbEMhfE7ReX; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri https://oauth.reddit.com/api/del)(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(id(t1_g3f4icy))))))(response(((encoding(Fixed 2))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 2)(content-type"application/json; charset=UTF-8")(date"Mon, 31 Aug 2020 01:08:03 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Thu, 31-Aug-2023 01:08:02 GMT; secure")(set-cookie"session_tracker=pinpfoeakrgilkiioi.0.1598836082759.Z0FBQUFBQmZURTF6SFpCRktua1FrYThCdWhHTlNhclRGSUNtRFpIZ2xzRXFsTF94NklaRVN0Rk5UVXpBcEZNR3ZibGpmV3hEMElZQzBUTG1WT2hKMDE2azhkWU5iR1ZNVFRLenk3NzhQbGpoX2ZMMm9ZeFZzX3V4M0stM3FiRjlYbUxvTGlPQlYwOG0; Domain=reddit.com; Max-Age=7199; Path=/; expires=Mon, 31-Aug-2020 03:08:03 GMT; secure")(set-cookie"edgebucket=gfOgh04Cj00MuzQBW8; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 598.0)(x-ratelimit-reset 118)(x-ratelimit-used 2)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false)){})))