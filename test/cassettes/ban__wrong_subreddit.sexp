((request(Post_form(uri https://www.reddit.com/api/v1/access_token)(headers((authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 117))(headers((accept-ranges bytes)(cache-control"max-age=0, must-revalidate")(connection keep-alive)(content-length 117)(content-type"application/json; charset=UTF-8")(date"Sun, 10 Jan 2021 00:55:31 GMT")(server snooserv)(set-cookie"edgebucket=HagFKSaocXup9966d9; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Post_form(uri https://oauth.reddit.com/r/thirdrealm/api/friend)(headers((authorization"bearer <ACCESS_TOKEN>")))(params((raw_json(1))(duration())(type(banned))(duration())(type(banned))(api_type(json))(name(spez))))))(response(((encoding(Fixed 38))(headers((accept-ranges bytes)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(connection keep-alive)(content-length 38)(content-type"application/json; charset=UTF-8")(date"Sun, 10 Jan 2021 00:55:32 GMT")(expires -1)(server snooserv)(set-cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Wed, 10-Jan-2024 00:55:32 GMT; secure")(set-cookie"session_tracker=pamdirikqhamckmbrp.0.1610240132005.Z0FBQUFBQmYtbENFTWQza2pPQXR3bnRyOHRKcjFtMUtjbXp5Z2w1Rmo2WmJVQndIQmxmRTN0Q0g5YkluRElNUjVXYWwwTXhvT1JmdzE1NEp0eFEzaWlhQ3Z2YUNISVZqZG5pcXJFTm5aaE1UN3ZPZlQ3d3pHcnlDcHFDSS1GNW45YjJkRUtGUzVuU2M; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sun, 10-Jan-2021 02:55:32 GMT; secure")(set-cookie"loid=00000000009qw1prk0.2.1610240132033.Z0FBQUFBQmYtbENFTk9YUHNiY0tqNEJDcncwckVWNTBIMVoxeGplWkt3TXZDaU9JYk14bU5HeVNjaHlsS0RVOXF3eEhKTXRQNEZyV091bUYtcDJBUmQ2MzVuMTZ4VnlIUkMtMkJFYWhpX0ZucWExdGh6elIzM1NUdDExMU5rWkdZZHlySkZ3a1k2SjA; Domain=reddit.com; Max-Age=63071999; Path=/; expires=Tue, 10-Jan-2023 00:55:32 GMT; secure")(set-cookie"session_tracker=NOOmE3IXW7B1Bb3vgh.0.1610240132033.Z0FBQUFBQmYtbENFNHhtdXN2OU85MDFLME1JcEI4a25YUGwtekFMUGZNN3p6ZjBycGg3UXF4MFJweUtGX0hQZWgtRWR2MmNyV1J0NkJWRmZzUDM3WmlQS3dUUTQzSHhvYTRyOUw5SEx4ay1PQWFuZDBYTTdhR0s0QmJqaWpOTGxxcDRSMGVYRlBvSWQ; Domain=reddit.com; Max-Age=7199; Path=/; expires=Sun, 10-Jan-2021 02:55:32 GMT; secure")(set-cookie"edgebucket=4TDJWzDvZTLgwMNSCI; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(strict-transport-security"max-age=15552000; includeSubDomains; preload")(via"1.1 varnish")(x-content-type-options nosniff)(x-frame-options SAMEORIGIN)(x-moose majestic)(x-ratelimit-remaining 598.0)(x-ratelimit-reset 268)(x-ratelimit-used 2)(x-ua-compatible IE=edge)(x-xss-protection"1; mode=block")))(version HTTP_1_1)(status Forbidden)(flush false))"{\"message\": \"Forbidden\", \"error\": 403}")))