    ((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Wed, 02 Sep 2020 12:23:31 GMT")(server snooserv)(set-cookie"edgebucket=GDdTlPdOwlgeQY4Dh6; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Get(uri((scheme(https))(host(oauth.reddit.com))(path /r/ocaml/random)(query((raw_json(1))))))(headers((authorization"bearer <ACCESS_TOKEN>")))))(response(((encoding(Fixed 0))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 0)(content-type"application/json; charset=UTF-8")(date"Wed, 02 Sep 2020 12:23:32 GMT")(expires -1)(location https://www.reddit.com/r/ocaml/comments/feyhbv/what/.json?utm_campaign=redirect&utm_medium=desktop&utm_source=reddit&utm_name=random_link)(server snooserv)(set-cookie"session_tracker=fmnbkaodgddgdhigpa.0.1599049411998.Z0FBQUFBQmZUNDdFNzk3SmxxLS1QYWR0akR0U2tzYWhSZG9DcmdfYmdVblhGTnRxY0p0UFkxQkhNM1FmcWV5bVN4b284UUF4bWYtcy1pQ0dOY3VtQnJHekNfTlF2SmxCQlItcEdWa201QURpTWRpY0JWTWp0djJUa0wwZjAzd2hWcXQ3NmZCSTYzMU4; Domain=reddit.com; Max-Age=7199; Path=/; expires=Wed, 02-Sep-2020 14:23:32 GMT; secure; SameSite=None; Secure")(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Sat, 02-Sep-2023 12:23:32 GMT; secure")(set-cookie"csv=1; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")(set-cookie"edgebucket=uplFGI2PGMTK0SZ6D2; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 598.0)(x-ratelimit-reset 388)(x-ratelimit-used 2)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status Found)(flush false))"")))
