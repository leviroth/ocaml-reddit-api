open! Core

include Name_utils.Make (struct
  let prefix = 'r'
  let module_name = "Subreddit_name"
end)

let user_subreddit username = of_string ("u_" ^ Username.to_string username)
