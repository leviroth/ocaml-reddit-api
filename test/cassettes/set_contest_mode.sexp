    ((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Tue, 01 Sep 2020 13:02:28 GMT")(server snooserv)(set-cookie"edgebucket=jYfhRAEpFDsQsCcQdX; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri((scheme(https))(host(oauth.reddit.com))(path /api/set_contest_mode)))(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(api_type(json))(id(t3_hofd3k))(state(true))))))(response(((encoding(Fixed 24))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 24)(content-type"application/json; charset=UTF-8")(date"Tue, 01 Sep 2020 13:02:29 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Fri, 01-Sep-2023 13:02:29 GMT; secure")(set-cookie"session_tracker=bblilgoobnejabdbfj.0.1598965349057.Z0FBQUFBQmZUa1psQkFhbGV2SXAxZlVqUXVKWTV1Sm1Nak8taE5RaU8zLVJtdDdUMlRtS3AyZklvYU9oYjBLelRRUkFPRks2LVJvRlZEdFItUHY5ZlRUX3dJVm1YVDNlRTN3dnltalp0LTFaNUxSMFE0a1BxT21aNzh1RHIxMjFMMUlDbFNDdVMyUFQ; Domain=reddit.com; Max-Age=7199; Path=/; expires=Tue, 01-Sep-2020 15:02:29 GMT; secure")(set-cookie"edgebucket=Rvzodz6h7uLDyM0b94; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 596.0)(x-ratelimit-reset 451)(x-ratelimit-used 4)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"json\": {\"errors\": []}}")))((request(Post_form(uri((scheme(https))(host(oauth.reddit.com))(path /api/set_contest_mode)))(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(api_type(json))(id(t3_hofd3k))(state(false))))))(response(((encoding(Fixed 24))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 24)(content-type"application/json; charset=UTF-8")(date"Tue, 01 Sep 2020 13:02:29 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Fri, 01-Sep-2023 13:02:29 GMT; secure")(set-cookie"session_tracker=felpkqlcmajbgdpoek.0.1598965349252.Z0FBQUFBQmZUa1psTDdVbEx3cDdhLUwxTGJXNFdjdXlzcjZCeFRMdGgyUVZBNUxReWxBTmhTd0pHcEN6eFBiYkRHVXN0RXVrdlQwVVliMmJBRVlIUlp1eHhCUk1hQXlYNDRIaHFQcWNnLThGVElVQTZtNktvTjhWQUhiZXR3ZUFfXzZJS25RYWZIaE8; Domain=reddit.com; Max-Age=7199; Path=/; expires=Tue, 01-Sep-2020 15:02:29 GMT; secure")(set-cookie"edgebucket=lMKKVxWZpQr19ACF0e; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 595.0)(x-ratelimit-reset 451)(x-ratelimit-used 5)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"json\": {\"errors\": []}}")))