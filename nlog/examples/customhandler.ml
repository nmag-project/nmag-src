#use "topfind";;
#require "nlog";;

(* get a logger, we choose the name "root" *)
let logroot = Nlog.getLogger("root");;

(* define our own handler *)
let myhandler levelint msg =
  Printf.printf "myhandler (could go to file): %s %s\n%!" 
    (Nlog.string_of_level (Nlog.level_of_int levelint)) 
    msg;;

Nlog.setLogHandler "root" myhandler;;

logroot Nlog.Info "Testing1, one, two, this is information";;
logroot Nlog.Debug "Testing2 one, two, this is a debug statement";;

(* We can get a summary of the status of the loggers: *)
Nlog.printLoggerInfo ();;
