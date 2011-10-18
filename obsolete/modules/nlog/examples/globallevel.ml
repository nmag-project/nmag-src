#use "topfind";;
#require "nlog";;

(* get a logger, we choose the name "root" *)
let logroot = Nlog.getLogger("root");;
(* We may want to set a level for this logger *)
Nlog.setLogLevel "root" Nlog.Debug;;
(* Now let's see how this works *)
logroot Nlog.Info "Testing, one, two, this is information";;
logroot Nlog.Debug "Testing, one, two, this is a debug statement";;
Nlog.setLogLevel "root" Nlog.Info;;
logroot Nlog.Info "Testing, three, four (information)";;
logroot Nlog.Debug "Testing, three, four (debug)";;
logroot Nlog.Warn "Testing1  (warning). Will now set global level to warn";;

Nlog.setGlobalLevel Nlog.Warn;;
logroot Nlog.Info "Testing2, (information)";;
logroot Nlog.Debug "Testing3 (debug)";;
logroot Nlog.Warn "Testing4  (warning)";;

(* We can get a summary of the status of the loggers: *)
Nlog.printLoggerInfo ();;
