((request(Post_form(uri https://www.reddit.com/api/v1/access_token)(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Fri, 25 Sep 2020 12:55:39 GMT")(server snooserv)(set-cookie"edgebucket=QiaBuN6Lypq0J5KR3x; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri https://oauth.reddit.com/api/subscribe)(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(action(sub))(sr_name(python))))))(response(((encoding(Fixed 2))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 2)(content-type"application/json; charset=UTF-8")(date"Fri, 25 Sep 2020 12:55:39 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Mon, 25-Sep-2023 12:55:39 GMT; secure")(set-cookie"session_tracker=lioekgfoejjmjkfljq.0.1601038539555.Z0FBQUFBQmZiZWpMNUVKLUQzb2NHZDBOcWRMQmVhb3hZNnlMSVlFY05MWHAtSUE1TzRXMVhUWENyUHJjSi1nd0t4SWRqeUhNOWxVMjFTVGFqRjJvaGdWU2NHUjluTnRhZVYzY3owenhaUWhkTWNQc1JGY0xWYVZjT0t2R2R5T25ScXU1ODhEcnpHal8; Domain=reddit.com; Max-Age=7199; Path=/; expires=Fri, 25-Sep-2020 14:55:39 GMT; secure")(set-cookie"edgebucket=DejYs9Asy90cTxvvqw; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 598.0)(x-ratelimit-reset 261)(x-ratelimit-used 2)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false)){})))