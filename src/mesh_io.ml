(* ==== T.F. revised code below === *)

(* developed by Giuliano Bordignon *)

(* write mesh on file with the format
   dim = 3     nodes = 120    simplices = 89
   1.23 4.56 5.47 (* coordinates of first point *)
   .
   .
   .
   3 6 8 9 (* points of first simplex *)
   .
   .
   .
*)

let write_mesh filename mesh =
  let format_version = "1.0" in
  let output_file = open_out filename in
  let points_coords = Array.map (fun p -> p.mp_coords) mesh.mm_points in
  let forall_simplices f = Array.map f mesh.mm_simplices in
  let points_indices_region_of_simplices =
    forall_simplices 
      (fun sx -> 
	let sx_indices = Array.map (fun p -> p.mp_id) sx.ms_points in 
	let sx_region = 
	  match sx.ms_in_body with
	    Body_Nr n -> n
		(*| _ -> failwith "No number of the simplex region"*)
	in
	sx_indices, sx_region 
      ) in
  let dim = Array.length points_coords.(0) in
  let nodes_nr = Array.length points_coords in
  let simplices_nr = Array.length points_indices_region_of_simplices in
  let () =
    Printf.fprintf output_file "PYFEM mesh file version %s\n" format_version
  in
  let () =
    output_string output_file
      ("dim = " ^ string_of_int(dim) ^
       "\t nodes = " ^ string_of_int(nodes_nr) ^
       "\t simplices =" ^ string_of_int(simplices_nr))
  in
  let write_coords coords =
    begin
    (*output_string output_file "["; *)
      Array.iter
	(fun coord ->
	  output_string output_file
	    ( (string_of_float coord)^" ") ) coords;
      output_string output_file "\n"
    end
  in
  let write_simplex_points (points,region) =
    begin
      (*let () = output_string output_file "(" in *)
      Array.iter
	(fun point ->
	  output_string output_file
	    ( (string_of_int point)^" ") ) points;
      output_string output_file ( "| "^string_of_int region) ;
      output_string output_file "\n"
    end
  in
  begin
    Array.iter write_coords points_coords;
    Array.iter write_simplex_points points_indices_region_of_simplices;
    close_out output_file;
  end
;;

(* Important Note: a loading function MUST check the validity of the data file,
   and in particular ensure that it cannot be crashed by providing a broken data file!

 *)

#load "str.cma";;

(* function that returns a mesh structure from
   the file containing mesh data
*)

let read_mesh filename =   
  let rx_nonnumber = Str.regexp "[^0-9\\.eE\\+\\-]+" in
  let parse_type = regexp_decompose 1 "PYFEM mesh file version \\([0-9]+\\).\\([0-9]+\\)" in
  let parse_structure = regexp_decompose 3 "dim[\\ \\t]*=[\\ \\t]*\\([0-9]+\\)[\\ \\t]*nodes[\\ \\t]*=[\\ \\t]*\\([0-9]+\\)[\\ \\t]*simplices[\\ \\t]*=[\\ \\t]*\\([0-9]+\\)"
  in
  try
    let lines = Array.of_list( read_file_as_lines filename) in
    if (Array.length lines < 2) then None
    else
      let parsed_type = parse_type lines.(0) in
      (fun f -> (* Functional hack to have the complicated case
		   at the end of the pattern matching *)
	match parsed_type with
	| ([|Some version_nr|]::empty) ->
	    if version_nr = "1.0" then f () else None
	| _ -> None)
	(fun () ->
	  let parsed_structure = parse_structure lines.(1) in
	  (fun f ->
	    match parsed_structure with
	    | ([|Some dim; Some nodes_nr; Some simplices_nr|]::empty) ->
		f (int_of_string dim) (int_of_string nodes_nr) (int_of_string simplices_nr)
	    | _ -> None
	  )
	    (fun dim nodes_nr simplices_nr -> 
	      let points_coords = Array.make nodes_nr (Array.make dim 0.0) in
	      let points_indices_of_simplices = Array.make simplices_nr (Array.make (dim+1) 0) in
	      try 
		let the_points = 
		  Array.map
		    (fun line ->
		      let pieces = Str.split rx_nonnumber line 
		      in
		      let coords = Array.of_list (List.map float_of_string pieces) in
		      if Array.length coords <> dim then failwith "Bad data file: problem on points data"
		      else coords
		    )
		    (Array.sub lines 2 nodes_nr)
		in	 
		let the_simplices =
		  Array.map
		    (fun line ->
		      let pieces = Str.split (Str.regexp "|" ) line in
		      if List.length pieces <> 2 
		      then failwith "Bad data file: problem on points data"
		      else 
			let sx_indices = List.nth pieces 0 in
			let sx_region = List.nth pieces 1 in  
			let pcs_indices = Str.split rx_nonnumber sx_indices in 
			let indices = Array.of_list (List.map int_of_string pcs_indices ) in
			if Array.length indices <> (dim+1) then failwith "Bad data file: problem on simplices data"
			else 
			  let pcs_region = List.nth (Str.split rx_nonnumber sx_region) 0 in
			  try
			    let region = (fun n -> Body_Nr n ) (int_of_string pcs_region)  
			    in
			    region, indices
			  with
			  | _ -> failwith "Bad data file: problem on simplices data"
		    )
		    ( Array.sub lines (2+nodes_nr) simplices_nr)
		in
		let mesh = mesh_from_known_delaunay the_points the_simplices
		in Some mesh
	      with
	      | _ -> None
	    )
	)
  with
  | _ -> None
;;
