((request(Post_form(uri https://www.reddit.com/api/v1/access_token)(headers((Authorization <AUTHORIZATION>)))(params((grant_type(password))(username(<USERNAME>))(password(<PASSWORD>))))))(response(((encoding(Fixed 117))(headers((Connection keep-alive)(Content-Length 117)(Content-Type"application/json; charset=UTF-8")(x-frame-options SAMEORIGIN)(x-content-type-options nosniff)(x-xss-protection"1; mode=block")(x-ratelimit-remaining 299)(x-ratelimit-used 1)(x-ratelimit-reset 598)(cache-control"max-age=0, must-revalidate")(X-Moose majestic)(Accept-Ranges bytes)(Date"Mon, 28 Mar 2022 11:20:03 GMT")(Via"1.1 varnish")(Set-Cookie"edgebucket=kS4k7oYKp7ZogUYscS; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(Strict-Transport-Security"max-age=15552000; includeSubDomains; preload")(Server snooserv)))(version HTTP_1_1)(status OK)(flush false))"{\"access_token\": \"<ACCESS_TOKEN>\", \"token_type\": \"bearer\", \"expires_in\": 3600, \"scope\": \"*\"}")))((request(Get(uri https://oauth.reddit.com/user/spez/upvoted?raw_json=1)(headers((Authorization"bearer <ACCESS_TOKEN>")))))(response(((encoding(Fixed 38))(headers((Connection keep-alive)(Content-Length 38)(Content-Type"application/json; charset=UTF-8")(x-ua-compatible IE=edge)(x-frame-options SAMEORIGIN)(x-content-type-options nosniff)(x-xss-protection"1; mode=block")(expires -1)(cache-control"private, s-maxage=0, max-age=0, must-revalidate, no-store, max-age=0, must-revalidate")(x-ratelimit-remaining 599.0)(x-ratelimit-used 1)(x-ratelimit-reset 597)(access-control-allow-origin *)(access-control-expose-headers X-Moose)(X-Moose majestic)(Accept-Ranges bytes)(Date"Mon, 28 Mar 2022 11:20:03 GMT")(Via"1.1 varnish")(Set-Cookie"loid=0000000000000xw1ym.2.1463096550337.Z0FBQUFBQmlRWm5qczN3Nk9rUURTdXgydk03THFtUmQtbkpPd1NGR3MtRzZyV2xfUlhTcnNyd2VnR2daTE9oYmRPWUdVMm5RMUJpcVVyVnBkdFNZZ25CNUpNWGEwLVNtMVZ1aDhaUXk5WEZrYmZXRWlUaXl4U1FlakpZNjlOZE85S0c1aGtMWjFQNEc; Domain=reddit.com; Max-Age=63071999; Path=/; expires=Wed, 27-Mar-2024 11:20:03 GMT; secure; SameSite=None; Secure")(Set-Cookie"session_tracker=dfkcfmoqciacjhbldm.0.1648466403464.Z0FBQUFBQmlRWm5qS2loS0wtSkw4STBkWEYwRVFrUVBJY0RxRHJNRU52bkI4blVFQldsZEEzYWVlaUtVTXFEaS1KUTQteC1aanNqZWVySmVZTi1CSXdiOW1QQXAwNlNZeWExMmctZ1hMWG1WN2FvOWlVeUFzMFJzSGk3NjVLVmRodGdBVDItQlRCeTI; Domain=reddit.com; Max-Age=7199; Path=/; expires=Mon, 28-Mar-2022 13:20:03 GMT; secure; SameSite=None; Secure")(Set-Cookie"redesign_optout=true; Domain=reddit.com; Max-Age=94607999; Path=/; expires=Thu, 27-Mar-2025 11:20:03 GMT; secure")(Set-Cookie"csv=2; Max-Age=63072000; Domain=.reddit.com; Path=/; Secure; SameSite=None")(Set-Cookie"edgebucket=Wq9GiBPaoyej3uJnTO; Domain=reddit.com; Max-Age=63071999; Path=/;  secure")(Strict-Transport-Security"max-age=15552000; includeSubDomains; preload")(Server snooserv)))(version HTTP_1_1)(status Forbidden)(flush false))"{\"message\": \"Forbidden\", \"error\": 403}")))