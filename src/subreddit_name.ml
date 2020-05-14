open! Core

include String_id.Make
          (struct
            let module_name = "Subreddit_name"
          end)
          ()
