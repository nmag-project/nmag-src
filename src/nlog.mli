(*
  (C) 2006 Dr. Hans Fangohr, Thomas Fischbacher, Giuliano Bordignon, Matteo Franchin,
  SES, University of Southampton

  $Header$
*)

(** A simple (but flexible) logging facility for OCaml. *) 

(** This [Nlog] module is part of the [nsim] simulation suite (but can be
used on its own).*)

(** {1 Overview (concepts)}    
    {2 Motivation}

    It is often useful to be able to log (via stdout/stderr, files,
    syslog entries, emails, ...) progress or problems of a software project.
    In particular, the source code can be populated with statements of the 
    type 

    log nLog.Info "About to start calculation...";
    log nLog.Debug (Printf.sprintf "n=%d, k=%d%" n k);
    log nLog.Info "Finished calculation"

    (Note that this example is not complete. See the examples subdirectory 
    for a set of complete working examples.)

    When the program is executed, a particular {e log-level} is set,
    and only log-messages with an associated log level higher than
    this will be reported ("emitted").
    
*)


(** {1 The logging system} *)

(** {2 The log levels} *)

(** The [level_type] provides OCaml values for different predefined loglevels: *)
type level_type =
    Notset
  | Debug
  | Info
  | Info2
  | Warn
  | Error
  | Critical
  | Level of int

(** To each of those values there as an associated (and hard-coded) integer value. This can be obtained with the following function*)
val int_of_level : level_type -> int
(** for which the code is shown here:
{v
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
v}
We are using exactly the same log level definitions as used in Python's logging module.
(See [http://docs.python.org/lib/module-logging.html]) 

Other (unnamed) intermediate values can be used by just using integer values >0.

*)


(** The reverse of [int_of_level]: *)
val level_of_int : int -> level_type

(** Function that given a level returns a string with the level name in capital letters *)
val string_of_level : level_type -> string


(** {2 Loggers and Handlers, and the global level} 

Each [logger] has an associated [logger level] (which the user can set and which determines whether events for this logger are filtered out in case the level of the event is smaller than the level of the logger).
Loggers are identified by their name (string).

Each [logger] has at least one [handler]. A handler is used to send a log message to stdout/stderr/a file/syslog/... using an [emitter] function. If no handler is specified, the default emitter will be used. This sends the log message to stdout.

Each [handler] has an associated [handler level]. The usual rules apply: if the level of the event that has been passed from the logger to the handler is greater than the handler level, then the message will be emitted.

In addition to these two stages (logger and handler) that each carry their own levels for filtering events, there is a global log level.

The decision tree whether to emit a message with level l is therefore:
{v if l >=  the global level, then
      if l >=  the logger level of the logger that is being used,  then
         for each handler of that logger:
            if l >= the handler level of that handler, then
               emit message
 otherwise do nothing.
v}

The default level for the global level, all logger levels and handler levels is [Notset] and therefore all messages will be passed on and emitted.
*)



(** {1 User interface} *)

(** Sets the global log level, returns previous global level. *)
val setGlobalLevel : level_type -> level_type

(** Retrieves the current global log level  *)
val getGlobalLevel : unit -> level_type

(** Given a logger-name (string), this function provides a logger instance (function that takes a log-level and a message and will emit the message if necessary.) 

If this logger doesn't exist yet, then it will be created. If the logger exists already, then
the existing logger will be returned. (This means one can use [getLogger "myloggername"] in different parts of a program (or even modules) and share this logger without having to pass around the logger value. 
*)
val getLogger : string -> level_type -> string -> unit

(** An alternative to retrieving an logger function via [getLogger], is to use this [log] function, which needs the name of the logger the message is meant for as the first argument, followed by the level of that message and the message itself. *)
val logmsg : string -> level_type -> string -> unit

(** Advanced: Users can provide their own {e handlers} for each logger. A handler knows about its own handler level and an [emitter] function that will be called to emit the message (if necessary). This function allows users to register their own emitter function for a particular handler. The signature of the emitter function has to be [(fun int -> string -> unit)] where [int] is the integer value of the message [string] to be emitted. The optional argument [handlername] defaults to ["default"] for the default handler (see [addLogHandler]). The second argument is the name of the logger for which the handler should be modified. In summary: first (optional) argument is the name of the handler, second argument is the name of the logger that this handler, the third argument is the emitter function that is to be called by this handler in future. *)
val setLogHandler :
  ?handlername:string -> string -> (int -> string -> unit) -> unit

(** Advanced: An arbitrary number of additional handlers can be created for each logger using this function. The default handler is called ["default"]. To use any additional handlers, these need to be given unique names (string). The arguments for this function are therefore: name of logger to which a handler should be added (string), name of the new handler (string), emitter function as in [setLogHandler]. *)
val addLogHandler : string -> string -> (int -> string -> unit) -> unit


(** To set the level (level_type) for a logger (string). Returns the
previous loglevel. *)
val setLogLevel : string -> level_type -> level_type

(** To get the current level for a logger (string) *)
val getLogLevel : string -> level_type

(** To set the level (level_type) for a handler (second string argument) that belongs to a logger (first string argument). Returns the previous log level. *)
val setLogHandlerLevel : string -> string -> level_type -> level_type

(** To get the level of a particular logger (1st string) handler (2nd string) *)
val getLogHandlerLevel : string -> string -> level_type

(** This function prints an overview of all registered loggers, their handlers and all their levels.
This only is useful to understand how the system works, and for debugging. *)
val printLoggerInfo : unit -> unit

(** Provides a string identifying the version of the code. *)
val version : unit -> string

(** Helper function for default emitter format *)
val localtime : unit -> string


(** {2 Examples } 

A number of examples can be found in [nlog/examples]:
- [basic.ml] demonstrates basic usage
- [convenience.ml] demonstrates how to reduce the characters typed while coding
- [globallevel.ml] demonstrates the role of the global level
- [twologgers.ml] shows how to use two loggers
- [customhandler.ml] shows how to register a custom emitter function
- [twohandlers.ml] shows how to use two handlers for one logger

*)

(* Internal values:

type handler_type = {
  mutable emitter : int -> string -> unit;
  mutable handlerlevel : level_type;
}

type logger_type = {
  handlers : (string, handler_type) Hashtbl.t;
  mutable loggerlevel : level_type;
}

val _default_emitter : string -> int -> string -> unit
val _add_handler :
  ?handlername:string ->
  logger_type -> (int -> string -> unit) -> handler_type
val _add_logger : string -> (int -> string -> unit) -> logger_type
val _getLoggerRecord : ?add:bool -> string -> logger_type
val _getHandlerRecord :
val _logeventhandler : logger_type -> level_type -> string -> unit
val _log : string -> level_type -> string -> unit
val _localtime : unit -> string
  ?handlername:string -> ?add:bool -> logger_type -> handler_type

type global_type = { mutable globallevel : level_type; }
val global : global_type

  *)

