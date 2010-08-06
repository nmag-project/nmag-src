(* Nmag micromagnetic simulator
   Copyright (C) 2010 University of Southampton
   Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others

   WEB:     http://nmag.soton.ac.uk
   CONTACT: nmag@soton.ac.uk

   AUTHOR(S) OF THIS FILE: Matteo Franchin
   LICENSE: GNU General Public License 2.0
            (see <http://www.gnu.org/licenses/>)
 *)

exception DeferredDeleted of string

type 'a t =
  { dfr_name: string;                     (* Name of the deferred quantity *)
    dfr_builder: 'a builder;              (* Builder function *)
    mutable dfr_value: 'a deferred_value; (* Value *)
    mutable dfr_count: int; }             (* Number of builds *)

and 'a builder = unit -> 'a

and 'a deferred_value =
   Deferred
 | Computed of 'a
 | Deleted of string
;;

let init name builder =
  {dfr_name = name;
   dfr_builder = builder;
   dfr_value = Deferred;
   dfr_count = 0;}
;;

(* The current context *)
let context = ref "Unset context";;

(* Activation status of logging *)
let logging = ref false;;

(* Decides whether the logging is activated or not *)
let activate_logging value = logging := value;;

(* Function to set a context name (useful to identify where deferred computations happen) *)
let set_context name = context := name;;

let logmsg what where =
  if !logging then
    Printf.printf "%s: quantity '%s' was %s.\n%!" !context what where
;;

(* Get a deferred quantity (computing it if necessary). *)
let get d =
  match d.dfr_value with
    Deferred ->
      let v = d.dfr_builder () in
      let () = d.dfr_value <- Computed v in
      let () = d.dfr_count <- d.dfr_count + 1 in
      let () = logmsg d.dfr_name "computed in Deferred.get" in
        v
  | Computed v ->
    let () = logmsg d.dfr_name "accessed in Deferred.get" in
      v
  | Deleted err_msg -> raise (DeferredDeleted err_msg)
;;

(* Delete a deferred quantity so that it won't be accessible anymore. *)
let delete ?msg d =
  let m =
    match msg with
      Some msg -> msg
    | None ->
      Printf.sprintf
        "Deferred quantity '%s' was deleted and cannot be rebuilt." d.dfr_name
  in
  let () = d.dfr_value <- Deleted m in
  let () = logmsg d.dfr_name "deleted in Deferred.delete" in
    ()
;;

(* Reset a deferred quantity. The quantity is discarded and recomputed later,
   if necessary. *)
let reset d =
  begin
    d.dfr_value <- Deferred;
    logmsg d.dfr_name "reset in Deferred.reset";
  end
;;

let get_computation_count d = d.dfr_count;;
