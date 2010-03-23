#use "topfind";;
#require "nlog";;

let log = Nlog.getLogger("mylog");;
Nlog.setLogLevel "mylog" Nlog.Debug;;

(* Curry the log level into a new function... *)
let loginfo = log Nlog.Info;;
let logdebug =log Nlog.Debug;;

(* makes logging in larger modules more convenient *)
loginfo "Testing, three, four (information)";;
logdebug "Testing, three, four (debug)";;


