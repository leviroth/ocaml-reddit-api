((request(Post_form(uri((scheme(https))(host(www.reddit.com))(path /api/v1/access_token)))(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 114))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 114)(content-type"application/json; charset=UTF-8")(date"Sat, 11 Jul 2020 11:54:22 GMT")(server snooserv)(set-cookie"edgebucket=bOdkAhXIv6mahgkypQ; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-cache MISS)(x-cache-hits 0)(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-served-by cache-bos4637-BOS)(x-timer S1594468462.213225,VS0,VE321)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri((scheme(https))(host(oauth.reddit.com))(path /api/report)))(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(thing_id(t3_hony5b))(api_type(json))(reason("Test report"))))))(response(((encoding(Fixed 24))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 24)(content-type"application/json; charset=UTF-8")(date"Sat, 11 Jul 2020 11:54:24 GMT")(expires -1)(server snooserv)(set-cookie"session_tracker=ihcgddlpqraarnjofp.0.1594468462673.Z0FBQUFBQmZDYWh3ZGRuLWF6cUJzSzlRR2pvcERzRk52S2N1dkhDTWhqXzlPbEFZa2Jub20zckowRGV5N0gteldhZFRfbWZNYnJhOFdWdW05aGhadmtITm5UdzJvREYzNTVleDNyRndpTE1yUnUzTDFIWGVQNmczczZDNDNRYzBUSXdwYmJJRjJsN0Y; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sat, 11-Jul-2020 13:54:24 GMT; secure")(set-cookie"edgebucket=f2WjSnqKT8E88VpxM1; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-cache MISS)(x-cache-hits 0)(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 599.0)(x-ratelimit-reset 338)(x-ratelimit-used 1)(x-served-by cache-bos4634-BOS)(x-timer S1594468463.629564,VS0,VE1881)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"json\": {\"errors\": []}}")))