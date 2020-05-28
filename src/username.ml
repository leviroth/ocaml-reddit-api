open! Core

include Name_utils.Make (struct
  let prefix = 'u'
  let module_name = "Username"
end)
