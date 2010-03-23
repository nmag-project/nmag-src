(* 
   (C) 2008 Dr. Thomas Fischbacher,
   SES, University of Southampton

   The nsim system has to have an idea about dependency trees of
   primary and derived fields. We implement this via an inference
   engine that originally was written in Python, but has to be
   migrated to ML for the sake of flexibility.
*)

type inference_entity =
    {ien_name: string;
     ien_depends_on: inference_entity array;
     ien_prerequisites: inference_entity array;
     ien_remake: unit -> unit;
     ien_also_updates: inference_entity array;
     mutable ien_is_uptodate: bool
    }
;;

let rec rebuild x =
  if x.ien_is_uptodate then ()
  else
    let () = Array.iter rebuild x.ien_prerequisites in
    let () = x.ien_remake () in
    let () = x.ien_is_uptodate <- true in
      Array.iter (fun z -> z.ien_is_uptodate <- true) 
;;


let rec invalidate x =
  if x.ien_is_uptodate
  then
    let () = x.ien_is_uptodate <- false in
    let () = Array.iter invalidate x.ien_depends_on in
      ()
  else 
    (* Do not redo the entire tree in case we are already
       classified as being invalidated. If we did this,
       we may end up walking the same tree multiple times
       unnecessarily...
    *)
    ()
;;

