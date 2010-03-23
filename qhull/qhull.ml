(* 
   (C) 2005 Dr. Thomas Fischbacher
*)

external delaunay_nocheck: float array array -> int array array = "caml_qhull_delaunay"
(*
  Tests showed that the extra time required to do consistency checks
  is absolutely negligible in comparison to libqhull run time.
*)

let delaunay pca =
  let nr_points = Array.length pca in
    if nr_points = 0 
      || nr_points < (Array.length pca.(0)+1)
    then
      failwith 
	(Printf.sprintf "Cannot Delaunay triangulate %d points!" nr_points)
	(* I do not think we should take the effort to create a more useful exception value here. *)
    else
      let rec check_sizes pos dim =
	if pos = nr_points then ()
	else
	  if Array.length pca.(pos) <> dim
	  then failwith "Points with different numbers of coordinates encountered!"
	  else check_sizes (1+pos) dim
      in
      let () = check_sizes 1 (Array.length pca.(0)) in
	delaunay_nocheck pca
;;

let neighbours points =
  let nr_points = Array.length points in
  let triangulation = delaunay points in
  let ht = Hashtbl.create 1987 in
  let v_neighbours = Array.make nr_points [] in
  let () = 
    Array.iter
      (fun simplex ->
	let n = Array.length simplex in
	for i=0 to n-1 do
	  for k=i+1 to n-1 do
	    let p1 = min simplex.(i) simplex.(k)
	    and p2 = max simplex.(i) simplex.(k)
	    in
	    let edge=(p1,p2) in
	    try
	      let _ = Hashtbl.find ht edge in ()
	    with
	    | Not_found ->
		begin
		  Hashtbl.add ht edge true;
		  v_neighbours.(p1) <- p2 :: v_neighbours.(p1);
		  v_neighbours.(p2) <- p1 :: v_neighbours.(p2);
		end
	  done
	done
      )
      triangulation
  in
  Array.map Array.of_list v_neighbours
;;

let version () = "$Id$";;
