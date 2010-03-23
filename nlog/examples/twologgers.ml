#use "topfind";;
#require "nlog";;

let log1 = Nlog.getLogger("log1");;
Nlog.setLogLevel "log1" Nlog.Debug;;
(* Now let's see how this works *)
log1 Nlog.Info "Testing, one, two, this is information";;
log1 Nlog.Debug "Testing, one, two, this is a debug statement";;

Nlog.setLogLevel "log1" Nlog.Info;;
log1 Nlog.Info "Testing, three, four (information)";;
log1 Nlog.Debug "Testing, three, four (debug)";;

(* Let's now start up a second logger *)
let log2 = Nlog.getLogger("log2");;
log2 Nlog.Warn "This is a new logger";;

Nlog.printLoggerInfo ();;
