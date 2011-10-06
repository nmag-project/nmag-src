(* (C) 2007 Dr. Thomas Fischbacher

 ocamlc -i hlib.ml > hlib.mli
*)

open Bigarray;;

exception HLib_exn of string;;

type hmatrix;;

type hlib_algorithm =
    ACA_I | ACA_II | Interpolation | HCA_I | HCA_II
;;

type cluster_strategy =
    CLUSTER_DEFAULT | CLUSTER_GEOMETRIC | CLUSTER_REGULAR | CLUSTER_REGULARBOX
  | CLUSTER_CARDINALITY | CLUSTER_PCA
;;

let hlib_algorithm_id x =
  match x with
    | ACA_I -> 0
    | ACA_II -> 1
    | Interpolation -> 2
    | HCA_I -> 3
    | HCA_II -> 4
;;

let get_cluster_strategy_id cs =
  match cs with
    | CLUSTER_DEFAULT -> 0
    | CLUSTER_GEOMETRIC -> 1
    | CLUSTER_REGULAR -> 2
    | CLUSTER_REGULARBOX -> 3
    | CLUSTER_CARDINALITY -> 4
    | CLUSTER_PCA -> 5
;;

let get_cluster_strategy_from_id id =
  match id with
    | 0 -> CLUSTER_DEFAULT
    | 1 -> CLUSTER_GEOMETRIC
    | 2 -> CLUSTER_REGULAR
    | 3 -> CLUSTER_REGULARBOX
    | 4 -> CLUSTER_CARDINALITY
    | 5 -> CLUSTER_PCA
    | _ -> failwith "Unknown value for ClusterStrategy"
;;

type simple_float_bigarray = (float, Bigarray.float64_elt,
			      Bigarray.c_layout) Bigarray.Array1.t;;

external hlib_init_raw: string -> unit = "caml_hlib_init";;

type indexinfo =
  float array array * int array array * int array array * int array array;;

(* Note: row_info and col_info are:
   (vertex_coords,triangles,edges,triangle_edges)

   The other args are: (algorithm_id,nfdeg,nmin,eta,eps_aca,eps,p,kmax)
*)

(* Code: HLib parallel *)
external raw_make_hmatrix_strip:
  indexinfo -> indexinfo -> int -> (int*int*int*int*float*float*float*int*int)
  -> hmatrix = "caml_hlib_raw_make_hmatrix_strip";;

(*external raw_make_hmatrix:
  float array array -> int array array -> int array array -> int array array
  -> (int*int*int*int*float*float*float*int*int)
  -> hmatrix = "caml_hlib_raw_make_hmatrix";;
NOTE: ^^^ raw_make_hmatrix is now defined in terms of raw_make_hmatrix_strip *)
let raw_make_hmatrix vertices3d triangles edges triangle_edges args =
  let row_info = (vertices3d, triangles, edges, triangle_edges)
  in
  raw_make_hmatrix_strip row_info row_info 1 (* <-- is_square *) args
;;

external write_hmatrix: string -> hmatrix -> unit = "caml_hlib_write_hmatrix";;

external read_hmatrix: string -> hmatrix = "caml_hlib_read_hmatrix";;

(* Return an array [|size_smx; size_adm; size_inadm; size_reg|] made of
   four floats which are the size (in megabytes) of:
    - the hierarchical matrix (size_smx),
    - total size of the admissible leaves (size_adm)
    - total size of the inadmissible leaves (size_inadm)
    - size that a regular dense matrix would require (size_reg) *)
external get_hmatrix_stats: hmatrix -> float array
  = "caml_hlib_get_matrix_stats";;

let get_hmatrix_stats_verbose hmx =
  match (get_hmatrix_stats hmx) with
  | [|size_smx; size_adm; size_inadm; size_reg|] ->
    let compression = 100.0*.(1.0 -. size_smx/.size_reg) in
      [|(size_smx, "MB", "size of the hierarchical matrix");
        (size_adm, "MB", "total size of the admissible leaves");
        (size_inadm, "MB", "total size of the inadmissible leaves");
        (size_reg, "MB", "size that a regular dense matrix would require");
        (compression, "%", "compression rate")|]
  | _ -> failwith "Unexpected return value from get_hmatrix_stats"
;;

external apply_hmatrix:
  hmatrix -> simple_float_bigarray -> simple_float_bigarray ->
  unit = "caml_hlib_apply_hmatrix";;

let hlib_init =
  let r_is_initialized = ref false in
    fun path ->
      if !r_is_initialized then ()	(* make this idempotent *)
      else
	let () = Callback.register_exception "ocaml_exn_hlib_caml_interface" (HLib_exn "") in
	  hlib_init_raw path
;;

external mgdesc_print_debug:
  float array array array * (int * float * float array) array -> unit
  = "caml_mgdesc_print_debug";;


let default_lattice_info =
  let identity_3d = [|[|1.0; 0.0; 0.0;|];
                      [|0.0; 1.0; 0.0|];
                      [|0.0; 0.0; 1.0|]|]
  in ([|identity_3d|], [|(0, 1.0, [|0.0; 0.0; 0.0|])|])
;;

(** Given a sufrace triangulation (array of positive oriented triangles)
  return (edge, triangle_edge) where 'dege' is an int array array where
  edges.(i) is the i-th edge (i.e. an array of two integers denoting the
  indices of the first and of the last vertex of the edge), and
  triangle_edges.(i) is a list of edge indices for the i-th triangle (edge
  indices refer to an element in the previous array). *)
let edges_of_surface_triangulation st =
  let ht_edges = Hashtbl.create 17 in
  let add_edge p q =
    let edge = [|min p q; max p q|] in
      try Hashtbl.find ht_edges edge
      with Not_found ->
        let nr_edge = Hashtbl.length ht_edges in
        let () = Hashtbl.add ht_edges edge nr_edge in
          nr_edge
  in
  let process_tri pqr =
    let e1 = add_edge pqr.(0) pqr.(1) in
    let e2 = add_edge pqr.(0) pqr.(2) in
    let e3 = add_edge pqr.(1) pqr.(2) in
      [|e1; e2; e3|]
  in
  let triangle_edges = Array.map process_tri st in
  let edges = Array.make_matrix (Hashtbl.length ht_edges) 2 0 in
  let () = Hashtbl.iter
    (fun edge nr ->
       begin
         edges.(nr).(0) <- edge.(0);
         edges.(nr).(1) <- edge.(1);
       end)
    ht_edges
  in (edges, triangle_edges)
;;

let positive_surface_triangulation points triangles normals =
  let positive_orientation nr_tri =
    let tri = triangles.(nr_tri) in
    let normal = normals.(nr_tri) in
    let ps = Array.map (fun n -> points.(n)) tri in
    let p10 = [|ps.(1).(0) -. ps.(0).(0);
                ps.(1).(1) -. ps.(0).(1);
                ps.(1).(2) -. ps.(0).(2);|]
    and p20 = [|ps.(2).(0) -. ps.(0).(0);
                ps.(2).(1) -. ps.(0).(1);
                ps.(2).(2) -. ps.(0).(2);|]
    in
    let v1_x_v2 = [|p10.(1)*.p20.(2)-.p10.(2)*.p20.(1);
                    p10.(2)*.p20.(0)-.p10.(0)*.p20.(2);
                    p10.(0)*.p20.(1)-.p10.(1)*.p20.(0);|]
    in
    let sprod =    v1_x_v2.(0)*.normal.(0)
                +. v1_x_v2.(1)*.normal.(1)
                +. v1_x_v2.(2)*.normal.(2)
    in (sprod > 0.0)
  in
    Array.mapi
      (fun nr_tri tri ->
         if positive_orientation nr_tri
         then tri
         else [|tri.(0); tri.(2); tri.(1)|])
      triangles
;;

let make_hmatrix_from_oriented_triangles
    ?(cluster_strategy=2) ?(algorithm=4) ?(nfdeg=2) ?(nmin=50)
    ?(eta=2.0) ?(eps_aca=0.00001) ?(eps=0.00001) ?(p=3) ?(kmax=50)
    vertices3d triangles =
  let edges, triangle_edges = edges_of_surface_triangulation triangles in
  let args =
    (cluster_strategy, algorithm, nfdeg, nmin, eta, eps_aca, eps, p, kmax)
  in
  raw_make_hmatrix vertices3d triangles edges triangle_edges args
;;

let make_hmatrix_old
    ?(algorithm=4) ?(nfdeg=2) ?(nmin=50) ?(eta=2.0) ?(eps_aca=0.00001)
    ?(eps=0.00001) ?(p=3) ?(kmax=50) ?(lattice_info=default_lattice_info)
    vertices3d triangles surface_normals =
  let () = mgdesc_print_debug lattice_info in
  let oriented_triangles =
    positive_surface_triangulation vertices3d triangles surface_normals
  in
    make_hmatrix_from_oriented_triangles
      ~algorithm ~nfdeg ~nmin ~eta ~eps_aca ~eps ~p ~kmax
      vertices3d
      oriented_triangles
;;



(*let default_lattice_info =
  let identity_3d = [|[|1.0; 0.0; 0.0;|];
                      [|0.0; 1.0; 0.0|];
                      [|0.0; 0.0; 1.0|]|]
  in ([|identity_3d|], [|(0, 1.0, [|0.0; 0.0; 0.0|])|])
;;*)

let apply_transformation transformation translation p =
  Array.init 3
    (fun i ->
       let row = transformation.(i)
       in (row.(0)*.p.(0) +.
           row.(1)*.p.(1) +.
           row.(2)*.p.(2) +.
           translation.(i)))
;;

let build_col_surface rpoints rtriangles lattice_info =
  let transformations, copies = lattice_info in
  let nr_copies = Array.length copies in
  let nr_rpoints = Array.length rpoints in
  let cpoints = Array.make (nr_copies*nr_rpoints) [||] in
  (* Build new vertices list *)
  let () =
    Array.iteri
      (fun nr_copy copy_info ->
         let transformation_idx, greyfactor, translation_vec = copy_info in
         let transformation = transformations.(transformation_idx) in
           Array.iteri
             (fun rp_idx rp ->
                let cp =
                  apply_transformation transformation translation_vec rp in
                let cp_idx = nr_rpoints*nr_copy + rp_idx
                in cpoints.(cp_idx) <- cp)
             rpoints)
      copies
  in
  (* Build triangle list *)
  let nr_rtriangles = Array.length rtriangles in
  let ctriangles = Array.make (nr_copies*nr_rtriangles) [||] in
  let () =
    for nr_copy = 0 to nr_copies - 1 do
       for nr_triangle = 0 to nr_rtriangles - 1 do
         ctriangles.(nr_triangle + nr_copy*nr_rtriangles) <-
           Array.map
             (fun p_idx -> p_idx + nr_copy*nr_rpoints)
             rtriangles.(nr_triangle)
       done
    done
  in
    (cpoints, ctriangles)
;;

let make_hmatrix_new
    ?(cluster_strategy=2) ?(algorithm=4) ?(nfdeg=2) ?(nmin=50)
    ?(eta=2.0) ?(eps_aca=0.00001) ?(eps=0.00001) ?(p=3) ?(kmax=50)
    ?(lattice_info=default_lattice_info)
    vertices3d triangles surface_normals =
  let rtriangles =
    positive_surface_triangulation vertices3d triangles surface_normals in
  let redges, rtriangle_edges =
    edges_of_surface_triangulation rtriangles in
  let args = (cluster_strategy, algorithm, nfdeg, nmin, eta, eps_aca, eps,
	      p, kmax) in
  let cpoints, ctriangles =
    build_col_surface vertices3d rtriangles lattice_info in
  let cedges, ctriangle_edges = edges_of_surface_triangulation ctriangles in
  let row_info = (vertices3d, rtriangles, redges, rtriangle_edges) in
  let col_info = (cpoints, ctriangles, cedges, ctriangle_edges)
  in
    raw_make_hmatrix_strip row_info col_info 0 args
;;

let make_hmatrix = make_hmatrix_new;;

(* === Versions for parallel hmatrix set-up
       (eventually to completely supersede the above code!)
   === *)

(* Code: HLib parallel *)
(*
  let make_hmatrix_strip_from_oriented_triangles
  ~surface_vertex_distribution (* [|nr_vertices_node0;nr_vertices_node1;...|] *)
  ~nr_strip
  ?(algorithm=HCA_II)
  ?(nfdeg=2)
  ?(nmin=50)
  ?(eta=2.0)
  ?(eps_aca=0.00001)
  ?(eps=0.00001)
  ?(p=3)
  ?(kmax=50)
  surface_vertices3d
  triangles =
  let alg_args=(hlib_algorithm_id algorithm,nfdeg,nmin,eta,eps_aca,eps,p,kmax) in
  let nr_vertices = Array.length surface_vertices3d in
  let cluster_geom_info vertices triangles =
  let ht_edges = Hashtbl.create 17 in
  let add_edge p q =
  let edge = [|min p q;max p q|] in
  try
  let n = Hashtbl.find ht_edges edge in
  n
  with | Not_found ->
  let nr_edge = Hashtbl.length ht_edges in
  let () = Hashtbl.add ht_edges edge nr_edge in
  nr_edge
  in
  let process_tri pqr =
  let e1 = add_edge pqr.(0) pqr.(1) in
  let e2 = add_edge pqr.(0) pqr.(2) in
  let e3 = add_edge pqr.(1) pqr.(2) in
  [|e1;e2;e3|]
  in
  let triangle_edges = Array.map process_tri triangles in
  let edges = Array.make_matrix (Hashtbl.length ht_edges) 2 0 in
  let () = Hashtbl.iter
  (fun edge nr ->
  begin
  edges.(nr).(0) <- edge.(0);
  edges.(nr).(1) <- edge.(1);
  end)
  ht_edges
  in
  (vertices,triangles,edges,triangle_edges)
  in
  (* Concerning the "input index" (indexing: M_out,in),
     we pass on the same data as is used for the set-up
     of a non-distributed BEM. Concerning the
     "output index", things are slightly more subtle, for:

     - we subdivide the BEM matrix into "strips"
     - this means that, geometrically, we will cut some of the
     surface triangles, due to this distribution.
     - While one may argue that, for the determination of the
     field strength at the observer point caused by a linear
     source distribution over a source triangle, only
     information on the source triangle is needed.
     This is not correct: while topology information is indeed
     not needed to work out field strengths at the observer point,
     it is needed for the set-up of the residual hierarchical
     cluster structure in the matrix strip. (Note: Hierarchical
     subdivision introduces both horizontal and vertical dividing
     lines!)

     The claim is: with that subdivision, geometry information is
     needed *only* for topological reasons. Distribution of vertex
     surfaces across the cluster will "cut" through some surface
     triangles. We do not lose anything by just dropping these from
     consideration.
    *)
  let nr_strips = Array.length surface_vertex_distribution in
  let vertex_strip = Array.make nr_vertices 0 in
  let () =
  let rec walk ix nr_strip rest_len =
  if ix = nr_vertices then ()
  else if rest_len = 0 then
  walk ix (1+nr_strip) surface_vertex_distribution.(1+nr_strip)
  else
  let () = vertex_strip.(ix) <- nr_strip in
  walk (1+ix) nr_strip (rest_len-1)
  in walk 0 0 surface_vertex_distribution.(0)
  in
  let strip_of_triangle abc =
  let sa = vertex_strip.(abc.(0)) in
  let sb = vertex_strip.(abc.(1)) in
  if sa <> sb then -1
  else
  let sc = vertex_strip.(abc.(2)) in
  if sa <> sc then -1
  else sa
  in
  let vertex_offsets =
  let a = Array.make (nr_strips+1) 0 in
  let () =
  for i=1 to nr_strips-1 do
  a.(i) <- a.(i-1) + surface_vertex_distribution.(i)
  done
  in
  let () = a.(nr_strips) <- Array.length surface_vertices3d in
  a
  in
  let strip_vertices =
  Array.sub surface_vertices3d
  vertex_offsets.(nr_strip)
  surface_vertex_distribution.(nr_strip)
  in
  (* This function is stolen from snippets.ml in order to make hlib.ml self-contained! *)
  let array_filter p arr =
  (* One problem here is that we cannot easily record filter results in a
     bit-vector, as OCaml does not provide anything like (simple-array bit ( * ) )

     Hence, we have to do more consing than we would like to.
   *)
  let pre_result = Array.copy arr in
  let len = Array.length arr in
  let rec walk nr_good pos_src =
  if pos_src = len
  then Array.sub pre_result 0 nr_good
  else
  if p arr.(pos_src)
  then
  let () = pre_result.(nr_good) <- arr.(pos_src) in
  walk (1+ nr_good) (1+ pos_src)
  else
  walk nr_good (1+ pos_src)
  in walk 0 0
  in
  let strip_triangles = array_filter (fun abc -> strip_of_triangle abc = nr_strip) triangles in
  raw_make_hmatrix_strip
  (cluster_geom_info strip_vertices strip_triangles)
  (cluster_geom_info surface_vertices3d triangles)
  alg_args
  ;;

  let make_hmatrix_strip
  ~surface_vertex_distribution (* [|nr_vertices_node0;nr_vertices_node1;...|] *)
  ~nr_strip
  ?(algorithm=HCA_II)
  ?(nfdeg=2)
  ?(nmin=50)
  ?(eta=2.0)
  ?(eps_aca=0.00001)
  ?(eps=0.00001)
  ?(p=3)
  ?(kmax=50)
  vertices3d
  triangles
  surface_normals =
  let positive_orientation nr_tri =
  let tri = triangles.(nr_tri) in
  let normal = surface_normals.(nr_tri) in
  let points = Array.map (fun n -> vertices3d.(n)) tri in
  let p10 = [|points.(1).(0)-.points.(0).(0);
  points.(1).(1)-.points.(0).(1);
  points.(1).(2)-.points.(0).(2);
  |]
  and p20 = [|points.(2).(0)-.points.(0).(0);
	      points.(2).(1)-.points.(0).(1);
	      points.(2).(2)-.points.(0).(2);
	    |]
  in
  let v1_x_v2 = [|p10.(1)*.p20.(2)-.p10.(2)*.p20.(1);
		  p10.(2)*.p20.(0)-.p10.(0)*.p20.(2);
		  p10.(0)*.p20.(1)-.p10.(1)*.p20.(0);
		|]
  in
  let sprod = v1_x_v2.(0)*.normal.(0)+.v1_x_v2.(1)*.normal.(1)+.v1_x_v2.(2)*.normal.(2) in
  sprod>0.0
  in
  let oriented_triangles =
    Array.mapi
      (fun nr_tri tri ->
	if positive_orientation nr_tri then tri
	else [|tri.(0);tri.(2);tri.(1)|])
      triangles
  in
  make_hmatrix_strip_from_oriented_triangles
    ~surface_vertex_distribution ~nr_strip
    ~algorithm ~nfdeg ~nmin ~eta ~eps_aca ~eps ~p ~kmax
    vertices3d
    oriented_triangles
;;
*)
