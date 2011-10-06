(* #use "topfind";; *)
(* #require "unix";; *)


(* Terminology:

 "emitter": fun: int -> string -> unit 

     Function that actually delivers message to stdout/file/...

 "handler": emitter and associated levels

 "logger": collection of handlers an associated logger level.

 *)



let version () = "$Id$";;


(* See http://docs.python.org/lib/module-logging.html for Level values *)
type level_type = Notset | Debug | Info | Info2 | Warn | Error | Critical | Level of int;;

(* Handlers are in charge of actually delivering ("emitting") a message *)
type handler_type = {
    mutable emitter : int -> string -> unit;
    mutable handlerlevel : level_type;
  } 
    

(* Data type for each logger. *)
type logger_type = 
    {
      handlers: (string, handler_type) Hashtbl.t;
      mutable loggerlevel : level_type;
    };;



  
(* Translate level names into numerical values as used by Python 
   See http://docs.python.org/lib/module-logging.html for Level values *)
let int_of_level name =
  match name with
    | Notset   -> 0
    | Debug    -> 10
    | Info2    -> 15 
    | Info     -> 20 
    | Warn     -> 30
    | Error    -> 40
    | Critical -> 50
    | Level (n)-> n
;;

let level_of_int levelint =
  match levelint with
    | 0  -> Notset    
    | 10 -> Debug     
    | 15 -> Info2       
    | 20 -> Info       
    | 30 -> Warn      
    | 40 -> Error     
    | 50 -> Critical  
    | n  -> Level (n) 
;;


let string_of_level levelint =
  match levelint with
    | Notset  -> "NOTSET"
    | Debug   -> "DEBUG"
    | Info    -> "INFO"
    | Info2   -> "INFO2"
    | Warn    -> "WARN"     
    | Error   -> "ERROR"    
    | Critical-> "CRITICAL" 
    | Level n -> string_of_int n
;;


(* This is the central hash table containing all the loggers. The keys are strings containing
the name of the logger, the values are logger_type structures. *)
let _loggers = Hashtbl.create 10;;

(* global logger level. This rules over all local levels (which are associated
   with individual loggers  *)

type global_type = { mutable globallevel : level_type; };;
let global = {globallevel = Notset;};;

(* and access functions *)
let setGlobalLevel level =
  let old_globallevel = global.globallevel in
  let () = global.globallevel <- level in
  old_globallevel;;

let getGlobalLevel () =
  global.globallevel;;

let localtime () =
  let time1 = Unix.time() in           (* this is returned the integer number of seconds since 1970
				          (although the return type is float) *)
  let time2 = Unix.gettimeofday() in   (* this is float, i.e. resolves the seconds *)
  let millisec = int_of_float( (time2-.time1) *. 1000.) in
  let tm = Unix.gmtime( time1) in
  let year = tm.Unix.tm_year +1900 in
  let month = tm.Unix.tm_mon +1 in
  let day = tm.Unix.tm_mday +1 in
  let hour = tm.Unix.tm_hour in
  let minutes = tm.Unix.tm_min in
  let sec = tm.Unix.tm_sec in
  Printf.sprintf "%04d-%02d-%02d %2d:%02d:%02d,%03d" year month day hour minutes sec millisec;;

(* Provide a default emitter *)
let _default_emitter name = 
  (fun lev_int msg -> 
     Printf.printf "%10s:%s %6s %s%!\n%!" 
       name 
       (localtime ()) 
       (string_of_level (level_of_int lev_int)) 
       msg);;


let _add_handler ?(handlername="default") loggerrecord emitter =
  let handlerrec = {emitter = emitter; handlerlevel = Notset} in
  let () = Hashtbl.add loggerrecord.handlers handlername handlerrec in 
  handlerrec;;

let _add_logger loggername emitter =
  let handlers = Hashtbl.create 10 in
  let logrec = {handlers = handlers; loggerlevel=Notset} in
  let () = Hashtbl.add _loggers loggername logrec in
  let _ =_add_handler logrec emitter in 
  logrec;;

let _getLoggerRecord ?(add=false) name =
  try
    Hashtbl.find _loggers name 
  with
    | Not_found -> 
	if add=true then
	  _add_logger name (_default_emitter name) 
	else
	  failwith (Printf.sprintf "Couldn't find logger '%s'" name) ;;

let _getHandlerRecord ?(handlername="default") ?(add=true) loggerrecord =
  try
    Hashtbl.find loggerrecord.handlers handlername 
  with
    | Not_found -> 
	if add=true then
	  _add_handler ~handlername:handlername loggerrecord (_default_emitter handlername)
	else
	  failwith (Printf.sprintf "Couldn't find handler '%s'" handlername) ;;


let setLogHandler ?(handlername="default") loggername emitter =
  let logrec = _getLoggerRecord ~add:false loggername in
  let handlerrec = _getHandlerRecord ~add:false ~handlername:handlername logrec in
  handlerrec.emitter <- emitter;;

let addLogHandler loggername handlername emitter =
  (* This will not raise an error if the handler exists already,
  but silently override it with the new one. *)
  let logrec = _getLoggerRecord ~add:false loggername in
  let handlerrec = _getHandlerRecord ~add:true ~handlername:handlername logrec in
  handlerrec.emitter <- emitter;;

let getLogLevel loggername =
    let logrec =_getLoggerRecord ~add:false loggername in
      logrec.loggerlevel;;

let setLogLevel loggername level =
  let logrec =_getLoggerRecord ~add:false loggername in
  let oldlevel = getLogLevel loggername in
  let () = logrec.loggerlevel <- level in
  oldlevel;;

let setLogHandlerLevel loggername handlername level =
  let logrec =_getLoggerRecord ~add:false loggername in
  let handlerrec = _getHandlerRecord ~add:false ~handlername:handlername logrec in
  let oldhandlerlevel = handlerrec.handlerlevel in
  let () = handlerrec.handlerlevel <- level in
  oldhandlerlevel;;

let getLogHandlerLevel loggername handlername =
  let logrec =_getLoggerRecord ~add:false loggername in
  let handlerrec = _getHandlerRecord ~add:false ~handlername:handlername logrec in
  handlerrec.handlerlevel;;

let _logeventhandler logrec level msg =
  let levelint = int_of_level level in
  
  (* first check if level > globallevel *)
  if levelint >= int_of_level (getGlobalLevel () ) 
  then
    (* then check if level > level of this particular logger *)
    if levelint >= (int_of_level logrec.loggerlevel) 
    then
      (* now need to iterate over all handlers *)
      let walk name handler =
	if levelint >= (int_of_level handler.handlerlevel)
	then
	  handler.emitter levelint msg
      in
      Hashtbl.iter walk logrec.handlers;;

let _log loggername level msg =
  let logrec = _getLoggerRecord loggername in
    _logeventhandler logrec level msg;;

let getLogger name =
    let logrec = _getLoggerRecord ~add:true name in
    _logeventhandler logrec;;
	  
let logmsg name level msg =
  _log name level msg;;

let printLoggerInfo () = 
  let () = Printf.printf "nLog status: \n Global loglevel is %s\n%!"
      (string_of_level (getGlobalLevel ())) in
  let walkloggers = (fun loggername logrec -> 
    let loglevelstring = string_of_level logrec.loggerlevel in
    let walkhandlers = (fun handlername handlerrec ->
      let handlerlevelstring = string_of_level handlerrec.handlerlevel in
      Printf.printf " logger:%s (%s) -> handler:%s (%s)\n%!" 
	loggername loglevelstring handlername handlerlevelstring)
    in
    Hashtbl.iter walkhandlers logrec.handlers )
  in
  Hashtbl.iter walkloggers _loggers;;



