#use "topfind";;
#require "nlog";;

(* get a logger, we choose the name "root" *)
let logroot = Nlog.getLogger("root");;

(* define our own handler, which could write to a file or syslog etc *)
let myhandler levelint msg =
  let level_string = Nlog.string_of_level (Nlog.level_of_int levelint) in
  Printf.printf "myhandler for root: %s %s\n%!"  
    level_string   
    msg;;

(* and ADD this handler to the root logger *)
Nlog.addLogHandler "root" "myhandlername" myhandler;;

logroot Nlog.Info "Testing1";;
logroot Nlog.Debug "Testing2";;

(* We can get a summary of the status of the loggers: *)
Nlog.printLoggerInfo ();;

(* Let's filter out debug messages for the "default" handler for the our "root" logger:*)
Nlog.setLogHandlerLevel "root" "default" Nlog.Info;;

Nlog.printLoggerInfo ();;

logroot Nlog.Info "Testing3";;
logroot Nlog.Debug "Testing4";;
