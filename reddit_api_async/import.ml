open! Core
open! Async

let log =
  let log = Log.copy (force Log.Global.log) in
  Log.set_level log `Error;
  log
;;
