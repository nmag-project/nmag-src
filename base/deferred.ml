(* Nmag micromagnetic simulator
   Copyright (C) 2010 University of Southampton
   Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others

   WEB:     http://nmag.soton.ac.uk
   CONTACT: nmag@soton.ac.uk

   AUTHOR(S) OF THIS FILE: Matteo Franchin
   LICENSE: GNU General Public License 2.0
            (see <http://www.gnu.org/licenses/>)
 *)

exception DeferredError of string
exception DeferredUnable of string
exception DeferredDeleted of string

type 'a creator = unit -> 'a

type 'a filler = 'a -> 'a

type collective_filler = unit -> unit

type 'a deferred_value =
   Deferred
 | Created of 'a
 | Filled of 'a
 | Deleted of string

type 'a t =
  { dfr_name: string;                       (* Name of deferred quantity *)
    mutable dfr_creator: 'a creator option; (* Creator function *)
    mutable dfr_filler: 'a filler option;   (* Filler function *)
    mutable dfr_value: 'a deferred_value;   (* Value *)
    mutable dfr_count: int; }               (* Number of builds *)

let init ?creator ?filler name =
  { dfr_name = name;
    dfr_creator = creator;
    dfr_filler = filler;
    dfr_value = Deferred;
    dfr_count = 0; }
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

let set_filler d filler = d.dfr_filler <- filler

let set_creator d creator = d.dfr_creator <- creator

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

let set_collective_filler_opt5 d1 d2 d3 d4 d5 filler =
  let check opt_d =
    match opt_d with
      None -> ()
    | Some d ->
        match d.dfr_value with
          Created v -> d.dfr_value <- Filled v
        | Filled v -> ()
        | _ -> raise (DeferredError "collective filler didn't fill all \
                                    the associated deferred quantities")
  in
  let check_all () =
    begin check d1; check d2; check d3; check d4; check d5; end
  in
  let individual_filler v =
    let () = filler () in
    let () = check_all ()
    in v
  in
  let set opt_d =
    match opt_d with
      None -> ()
    | Some d -> set_filler d (Some individual_filler)
  in
    begin set d1; set d2; set d3; set d4; set d5; end
;;

let set_collective_filler2 a b =
  set_collective_filler_opt5 None None None (Some a) (Some b);;

(*let set_collective_filler3 a b c =
  set_collective_filler_opt5 None None (Some a) (Some b) (Some c);;
let set_collective_filler4 a b c d =
  set_collective_filler_opt5 None (Some a) (Some b) (Some c) (Some d);;
let set_collective_filler5 a b c d e =
  set_collective_filler_opt5 (Some a) (Some b) (Some c) (Some d) (Some e);;*)

