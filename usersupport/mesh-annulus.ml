(* T. Fischbacher, 19.12.2007

   Semi-manually generating a mesh for an annulus
   with rectangular cross-section
*)

#use "topfind";;
#require "snippets";;
#require "qhull";;
#require "mesh";;

open Snippets;;
open Qhull;;

let cross_section_layout_v1=
[|"*-----*-----*------*------*------*------*------*-----*------*------*------*-----*";
  "|                                                                               |";
  "|                                                                               |";
  "|                                                                               |";
  "+------+-----+-----+------+-----+------+------+-----+------+------+------+------+";
|]
;;

let cross_section_layout_v2=
[|"*-----*-----*-----*-----*-----*-----*------*-----*-----*-----*-----*-----*------*";
  "|                                                                               |";
  "|                                                                               |";
  "|                                                                               |";
  "+------+-----+-----+-----+-----+-----+------+-----+-----+-----+-----+-----+-----+";
|]


let cross_section_layout_v3=
[|"*-----*-----*-----*-----*-----*-----*------*-----*-----*-----*-----*-----*------*";
  "|                                                                               |";
  "|  +     +     +     +     +     +     +      +     +     +     +     +     +   |";
  "|                                                                               |";
  "*-----*-----*-----*-----*-----*-----*------*-----*-----*-----*-----*-----*------*";
|]
;;

let cross_section_layout_v4 =
[|"*------*-----*------*------*------*-----*------*------*------*------*-----*-----*";
  "|                                                                               |";
  "|   +     +     +      +      +      +      +      +      +      +     +     +  |";
  "|                                                                               |";
  "*------*-----*------*------*------*-----*------*------*------*------*-----*-----*";
|]
;;

let cross_section_layout_v5 =
[|"*---+-*--+--*--+--*--+--*--+--*--+--*--+---*--+--*--+--*--+--*--+--*--+--*--+---*";
  "|                                                                               |";
  "|                                                                               |";
  "|                                                                               |";
  "*---+-*--+--*--+--*--+--*--+--*--+--*---+--*--+--*--+--*--+--*--+--*--+--*--+---*";
|]
;;


let cross_section_layout_v6=
[|"*-------------------------------------------------------------------------------*";
  "|                                                                               |";
  "|                                                                               |";
  "|                                                                               |";
  "*-------------------------------------------------------------------------------*";
|]
;;


let sx_quality =
  let cc = simplex_circumcircle_midpoint_radius 3
  and ic = simplex_incircle_midpoint_radius 3
  in
    fun vertices sx ->
      let coords = Array.map (fun n -> vertices.(n)) sx in
      let (_,cr) = cc coords
      and (_,ir) = ic coords
      in 3.0*.ir/.cr
;;


let annulus_raw_mesh
    ?(layers="*+")
    ?(width=40.0)
    ?(height=2.0)
    ?(center_radius=200.0)
    ?(nr_copies=400)
    ?(coordinate_fuzz=0.000)
    cross_section_layout =
  let angle=2.0*.4.0*.(atan 1.0)/.(float_of_int nr_copies)
  and nr_layers = String.length layers
  and nr_rows = Array.length cross_section_layout
  and nr_cols = String.length cross_section_layout.(0)
  in
  let scale_row = height/.(float_of_int (nr_rows-1))
  and scale_col = width/.(float_of_int (nr_cols-1))
  in
  let fuzz () = (Random.float coordinate_fuzz)-.0.5*.coordinate_fuzz in
  let coords_by_layer =
    Array.init nr_layers
      (fun nr_layer ->
	 let char_layer = layers.[nr_layer] in
	 let rec walk have_coords nr_row nr_col =
	   if nr_row = Array.length cross_section_layout
	   then Array.of_list (List.rev have_coords)
	   else if nr_col = nr_cols
	   then walk have_coords (1+nr_row) 0
	   else
	     let c = cross_section_layout.(nr_row).[nr_col] in
	       if c <> char_layer
	       then walk have_coords nr_row (1+nr_col)
	       else
		 let new_coords =
		   ((float_of_int nr_col)*.scale_col-.0.5*.width,
		    (float_of_int nr_row)*.scale_row-.0.5*.height)
		 in
		   walk (new_coords::have_coords) nr_row (1+nr_col)
	 in walk [] 0 0)
    in
  let r_result = 
    ref [
      [|0.0;0.0;0.0|] (* Artificially adding a center point *)
    ] in
  let produce x = r_result := x :: !r_result in
  let () =
      for nr_slice=0 to nr_copies-1 do
	for nr_layer=0 to Array.length coords_by_layer-1 do
	  let alpha = 
	    ((float_of_int nr_slice)+.((float_of_int nr_layer)/.(float_of_int nr_layers)))*.angle
	  in
	  let ca = cos alpha
	  and sa = sin alpha
	  in
	    Array.iter 
	      (fun (x_layer,y_layer) ->
		 produce
		   [|(x_layer+.center_radius)*.ca+.fuzz();
		     (x_layer+.center_radius)*.sa+.fuzz();
		     y_layer+.fuzz()
		   |])
	      coords_by_layer.(nr_layer)
	done
      done;
  in
  let vertices = Array.of_list !r_result in
  let simplex_center_of_gravity indices =
    float_arrays_avg (Array.map (fun n -> vertices.(n)) indices)
  in
  let simplex_allowed indices =
    let cog = simplex_center_of_gravity indices in
    let cog_xy = [|cog.(0);cog.(1);0.0|] in
      sqrt(euclidean_len_sq cog_xy) >= center_radius-.0.5*.width
      && (sx_quality vertices indices > 0.01)
  in
  let simplices = array_filter simplex_allowed (delaunay vertices)
  in
    (Array.sub vertices 0 (Array.length vertices-1), simplices)
;;
	     
let (raw_mesh_vertices_v1,raw_mesh_simplices_v1) = annulus_raw_mesh ~nr_copies:330 cross_section_layout_v1;;

(* Note: this takes about 5 minutes on a 1 GHz Pentium-III.
   Highly regular geometries like this one can be a bit 
   unforgiving when it comes to simplicial decomposition...
*)

let mesh_v1 =
  let body1 = Mesh.Body_Nr 1 in
  Mesh.mesh_from_known_delaunay 
    raw_mesh_vertices_v1
    (Array.map (fun s -> (body1,s)) raw_mesh_simplices_v1)
;;

let () = Mesh.write_mesh "/tmp/mesh_v1.nmesh" mesh_v1;;


let (raw_mesh_vertices_v2,raw_mesh_simplices_v2) = annulus_raw_mesh cross_section_layout_v2;;

let mesh_v2 =
  let body1 = Mesh.Body_Nr 1 in
  Mesh.mesh_from_known_delaunay 
    raw_mesh_vertices_v2
    (Array.map (fun s -> (body1,s)) raw_mesh_simplices_v2)
;;

let () = Mesh.write_mesh "/tmp/mesh_v2.nmesh" mesh_v2;
