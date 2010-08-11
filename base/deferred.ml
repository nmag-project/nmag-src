(* Nmag micromagnetic simulator
   Copyright (C) 2010 University of Southampton
   Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others

   WEB:     http://nmag.soton.ac.uk
   CONTACT: nmag@soton.ac.uk

   AUTHOR(S) OF THIS FILE: Matteo Franchin
   LICENSE: GNU General Public License 2.0
            (see <http://www.gnu.org/licenses/>)
 *)

exception DeferredUnable of string
exception DeferredDeleted of string

type 'a creator = unit -> 'a

type 'a filler = 'a -> 'a

type 'a deferred_value =
   Deferred
 | Created of 'a
 | Filled of 'a
 | Deleted of string

type 'a t =
  { dfr_name: string;                     (* Name of the deferred quantity *)
    dfr_creator: 'a creator option;       (* Creator function *)
    dfr_filler: 'a filler option;         (* Filler function *)
    mutable dfr_value: 'a deferred_value; (* Value *)
    mutable dfr_count: int; }             (* Number of builds *)

let init ?creator ?filler name =
  {dfr_name = name;
   dfr_creator = creator;
   dfr_filler = filler;
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

(* Create a deferred quantity (without filling it) and return it. *)
let create d =
  match d.dfr_value with
    Deferred ->
      let v =
        match d.dfr_creator with
          None ->
            let msg =
              Printf.sprintf
                "Deferred quantity '%s' hasn't got a creator." d.dfr_name
            in raise (DeferredUnable msg)
        | Some creator ->
          let () = logmsg d.dfr_name "created in Deferred.create"
          in creator ()
      in
      let () = d.dfr_value <- Created v in
      let () = d.dfr_count <- d.dfr_count + 1 in
        v
  | Created v | Filled v ->
    let () = logmsg d.dfr_name "accessed in Deferred.create" in
      v
  | Deleted err_msg -> raise (DeferredDeleted err_msg)
;;

(* Get a deferred quantity (computing it if necessary). *)
let get d =
  let fill v =
    match d.dfr_filler with
      None ->
        let () = logmsg d.dfr_name "marked as Filled in Deferred.get" in
        let () = d.dfr_value <- Filled v
        in v
    | Some filler ->
        let () = logmsg d.dfr_name "filled in Deferred.get" in
        let v = filler v in
        let () = d.dfr_value <- Filled v
        in v
  in
    match d.dfr_value with
      Deferred -> fill (create d)
    | Created v -> fill v
    | Filled v -> v
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
