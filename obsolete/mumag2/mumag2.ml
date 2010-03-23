(*
   (C) 2006 Dr. Thomas Fischbacher

   Functions especially relevant to micromagnetism

   Note: make interface with:

  ocamlc -I ../nlog -I ../sundials2 -I ../snippets -I ../mpi_petsc -I ../mesh -I ../fem -i mumag2.ml

 *)

open Snippets;;
open Mesh;;
open Fem;;

(* Get logger: *)
(* This is the logger with the handler from Python (relatively slow, but
   level can be chosen from command line (using '-ddebug', for example)) *)


let logger = Nlog.getLogger "ocaml.ocaml";;
(* let meshlogger = Nlog.getLogger "dev-mesh";; *)
let logdebug = logger Nlog.Debug;;
let loginfo2 = logger (Nlog.Level 15);; (* between debug and info *)
let loginfo = logger Nlog.Info;;
let logwarn = logger Nlog.Warn;;


(* Create second (fast) logger which is only used by development team.
   This is not using Python (unless a emitter-function is registered from 
   the Python side) and is thus fast. Use this where speed matters. *)
let fastlog = Nlog.getLogger "ocaml.dev-mumag2";;
Nlog.setLogLevel "ocaml.dev-mumag2" Nlog.Warn;;

let fastlogdebug = fastlog Nlog.Debug;;
let fastloginfo =  fastlog Nlog.Info;;
let fastlogwarn =  fastlog Nlog.Warn;;


(* Could register our own message handling function like this:
let fastloghandler levelint msg =
  Printf.printf " %d: %s" levelint msg
in
  Nlog.setLogHandler "dev-mesh" fastloghandler;;
*)

(* And can change loglevel if required like this: 

Nlog.setLogLevel "ocaml.dev-mumag2" Nlog.Debug;;
Nlog.setLogLevel "ocaml.dev-mumag2" (Nlog.Level 15);;

*)

let version () = Snippets.md5
  (String.concat ":" ["$Id$";
		      (Snippets.version());
		      (Mesh.version());
		      (Mpi_petsc.version());
		      (Fem.version());
		      (Nlog.version());
		     ])
;;


type llg_material =
  {
    llg_name: string;
    llg_Ms: float; (* M_sat *)
    llg_J: float; (* J_exch *)
    llg_c1: float;
    llg_c2: float;
    llg_c3: float;
    (* coefficients for spin-transfer-torque terms *)
    llg_stt_c1: float; (* Coefficient of m x J*grad m *)
    llg_stt_c2: float; (* Coefficient of m x (m x J*grad m) *)
    llg_anisotropy: (float array -> float) option;
    llg_anisotropy_order: int;
  }
;;



let standard_simplex_subdivide =
	(fun point_meddler vertices ->
	   let dim = Array.length vertices in
	     match dim with
	       | 2 -> 
		   let mid = point_meddler vertices in
		     [|[|vertices.(0);mid|];
		       [|mid;vertices.(1)|]|]
	       | 3 ->
		   let mids =
		     [|point_meddler [|vertices.(1);vertices.(2)|];
		       point_meddler [|vertices.(2);vertices.(0)|];
		       point_meddler [|vertices.(0);vertices.(1)|];
		     |]
		   in
		     [|[|vertices.(0);mids.(2);mids.(1)|];
		       [|vertices.(1);mids.(0);mids.(2)|];
		       [|vertices.(2);mids.(1);mids.(0)|];
		       [|mids.(2);mids.(0);mids.(1)|];
		     |]
	       | n -> failwith
		   (Printf.sprintf 
		      "do_for_simplex_subdivisions: fun_how_to_sundivide  must be user-specified for simplices other than lines and triangles (dim=%d)" n)
	)
;;

let subdivide_if_surface_angle_larger_than dim max_angle pos_observer =
  let angle_ub_lb = d_dimensional_space_angle_ub_lb dim in
    fun point_meddler vertices ->
      let (ub,lb) = angle_ub_lb pos_observer vertices in
	ub > max_angle
;;
    

let do_for_simplex_subdivisions 
    ?(point_meddler=float_arrays_avg)
    ?(fun_how_to_subdivide=standard_simplex_subdivide)
    ~start_level
    ~fun_stop_level
    f
    vertices
    =
  let rec work level vertices =
    if fun_stop_level level
    then ()
    else
      let new_level = f level vertices in
      let sub_vertices = fun_how_to_subdivide point_meddler vertices in
	Array.iter (work new_level) sub_vertices
  in work start_level vertices
;;

(* XXX problem: fun_have_to_subdivide may depend on the actual numfun!
   Furthermore, some numfuns may require finer subdivision than others.
*)

let integrate_femfun_numfuns_over_face
    ?(point_meddler=float_arrays_avg)
    (* Effectively: false (i.e. never subdivide) *)
    ?(fun_how_to_subdivide=standard_simplex_subdivide)
    femfun
    v_numfun__subdivision_fun
    v_integral_accumulators
    simplex nr_face =
  let dim = Array.length simplex.ms_points.(0).mp_coords in
  let nr_numfuns = Array.length v_numfun__subdivision_fun in
  let vol_face = volume_p_simplex_in_q_dimensions dim dim in
  (*
     Note that we internally do the integration in x-coords,
     as this may later on allow us to use the point_meddler
     parameter of do_for_triangle_subdivisions to good effect.
     
     Note that numfun must be written in such a way
     that it does not mind an extra coordinate,
     as we will pass extended point coordinates to it!
   *)
  let points = Array.map (fun p -> p.mp_coords) (array_one_shorter simplex.ms_points nr_face) in
  let un_option x =
    match x with
    | None -> failwith "Required optional argument was given as None"
    | Some z -> z
  in
  let mx_L_to_x = un_option simplex.ms_ext_point_coords in
  let mx_x_to_L = un_option simplex.ms_inv_ext_point_coords in
  let extend_coords v =
    let dim = Array.length v in
    Array.init (1+dim) (fun n -> if n=dim then 1.0 else v.(n))
  in
  let integrals = v_integral_accumulators in
  let filter_level = array_filter (fun x -> x <> -1) in
  let () =
    do_for_simplex_subdivisions ~point_meddler ~fun_how_to_subdivide
      ~start_level:(Array.init nr_numfuns (fun n -> n))
      ~fun_stop_level:(fun todo -> Array.length todo = 0)
      (fun level sub_vertices ->
	 let face_area = vol_face sub_vertices in
	 let midpoint = point_meddler sub_vertices in
	 let midpoint_ext =
	   Array.init (1+dim) (fun n -> if n = dim then 1.0 else midpoint.(n))
	 in
	 let midpoint_L = mx_x_vec mx_x_to_L midpoint_ext in
	 let val_femfun = femfun_eval mx_x_to_L femfun midpoint_L in
	 let next_level_raw = Array.make (Array.length level) (-1) in
	 let () =
	   Array.iteri
	     (fun n ix ->
		let (numfun,sd_fun) = v_numfun__subdivision_fun.(ix) in
		let have_to_subdivide = sd_fun midpoint sub_vertices in
		  if have_to_subdivide then
		    next_level_raw.(n) <- ix
		  else
		    let val_numfun = numfun midpoint in
		      integrals.(ix) <- integrals.(ix) +. val_numfun*.val_femfun*.face_area)
	     level
	 in filter_level next_level_raw)
      points
  in
    v_integral_accumulators
;;




let simplex3d_outward_surface_normal_ext
    ?(normalize=true)
    simplex nr_face =
  let point1 = simplex.ms_points.(if nr_face <= 0 then 1 else 0).mp_coords in
  let point2 = simplex.ms_points.(if nr_face <= 1 then 2 else 1).mp_coords in
  let point3 = simplex.ms_points.(if nr_face <= 2 then 3 else 2).mp_coords in
  let v1 = array_pointwise (-.) point2 point1 in
  let v2 = array_pointwise (-.) point3 point1 in
  let normal =
    [|v1.(1)*.v2.(2) -. v1.(2)*.v2.(1);
      v1.(2)*.v2.(0) -. v1.(0)*.v2.(2);
      v1.(0)*.v2.(1) -. v1.(1)*.v2.(0);
      0.0
    |]
  in
  let () = 
    (if normalize then
      let n_len = sqrt(euclidean_len_sq normal) in
      let inv_n_len = if n_len = 0.0 then 0.0 else 1.0/.n_len in
      for i=0 to 3-1 do
	normal.(i) <- inv_n_len*.normal.(i)
      done
    else ())
  in
  let () = normal.(3) <- 1.0 in
    (* We have to find out whether this normal is inward-pointing
       or outward-pointing *)
    let point0 = simplex.ms_points.(nr_face).mp_coords in
    let v0 = array_pointwise (-.) point0 point1 in
    if v0.(0)*.normal.(0) +. v0.(1)*.normal.(1) +. v0.(2)*.normal.(2) <= 0.0
    then
      normal
    else
      begin
	for i=0 to 3-1 do
	  normal.(i) <- -.normal.(i);
	done;
	normal
      end
;;

(* Note that we make the number of coordinates an extra argument.
   Why so? We may want to consider "2.5-dimensional" situations
   where we have 3d magnetization on a 2d film for which we use
   a 2d mesh - but where the demag field nevertheless reaches out
   into space!
*)

let green_kernel_directional_derivative ?nr_coords dim =
  let nr_coords =
    match nr_coords with
      | None -> dim
      | Some n -> n
  in
  let f_dim = float_of_int dim in
  let surface = surface_d_sphere dim in
  let buffer_dist = Array.make nr_coords 0.0 in
    fun direction point_x point_y ->
      begin
	for i=0 to nr_coords-1 do
	  buffer_dist.(i) <- point_y.(i) -. point_x.(i);
	done;
	let r=sqrt(euclidean_len_sq buffer_dist) in
	let denominator = if r=0.0 then 0.0 else r**f_dim in
	let numerator=scalar_product buffer_dist direction in
	  -.numerator/.(surface*.denominator)
      end
;;
	  

(* This function is a bit strange insofar as that it checks two
   arrays, uses EQ comparison, and returns a list. 
 *)

let array_intersection_eq arr1 arr2 =
  let len1 = Array.length arr1 
  and len2 = Array.length arr2 in
  let rec seek_in_2 x pos =
    if pos=len2 then false
    else if arr2.(pos) == x then true
    else seek_in_2 x (1+pos)
  in
  let rec walk pos have =
    if pos=len1 then have
    else
      let entry = arr1.(pos) in
      walk (1+pos)
      (if seek_in_2 entry 0 then (entry::have) else have)
  in
  walk 0 []
;;


(* The following function has been derived by Lindholm as an
   analytic expression for the scalar potential BEM matrix
   contribution of a surface triangle, for first order elements.

   This is based on the diagram and formula in:

   http://magnet.atp.tuwien.ac.at/scholz/projects/diss/html/node19.html

   Note 1: when interpreting the diagram, please take great care:
   gamma is a symmetric matrix - it is just that the (i+1) indices mean
   in all the places where they show up: 
   "we would like to have indices 1,2,3; but actually, L has indices 0,1,2".

   Note 2: simple considerations show that for some special geometrical arrangements,
   the Lindholm formula is of the type

    [effect ~O(1/r^2)] = [Something O(1/r)] - [Something else O(1/r)]

   This has numerical implications on accuracy.


   Internal function only - not exported!
*)


let lindholm_triangle_contributions =
  let rec v_minus target v1 v2 =
    for i=0 to 3-1 do
      target.(i) <- v1.(i) -. v2.(i);
    done
  and xprod target v1 v2 =
    begin
      target.(0) <- v1.(1)*.v2.(2) -. v2.(1)*.v1.(2);
      target.(1) <- v1.(2)*.v2.(0) -. v2.(2)*.v1.(0);
      target.(2) <- v1.(0)*.v2.(1) -. v2.(0)*.v1.(1);
    end
  and sprod x y = x.(0)*.y.(0)+.x.(1)*.y.(1)+.x.(2)*.y.(2)
  and v_len x = sqrt(sprod x x)
  and sign x = if x<0.0 then -1.0 else if x>0.0 then 1.0 else 0.0
  and v_normalized target z =
    let len=sqrt(sprod z z) in
    let inv_len=(if len=0.0 then 0.0 else 1.0/.len) in
      for i=0 to 3-1 do
	target.(i) <- z.(i)*.inv_len;
      done
  in
  let pi=4.0*.atan 1.0 
  and r0 = Array.make 3 0.0
  and r1 = Array.make 3 0.0
  and r2 = Array.make 3 0.0
  and s0 = Array.make 3 0.0
  and s1 = Array.make 3 0.0
  and s2 = Array.make 3 0.0
  and xi0 = Array.make 3 0.0
  and xi1 = Array.make 3 0.0
  and xi2 = Array.make 3 0.0
  and eta0 = Array.make 3 0.0
  and eta1 = Array.make 3 0.0
  and eta2 = Array.make 3 0.0
  and v_zeta = Array.make 3 0.0
  and v_log = Array.make 3 0.0
  and gamma0 = Array.make 3 0.0
  and gamma1 = Array.make 3 0.0
  and gamma2 = Array.make 3 0.0
  and buffer0 = Array.make 3 0.0
  and buffer1 = Array.make 3 0.0
  and buffer2 = Array.make 3 0.0
  and area = ref 0.0 
  in
    fun ?store_zeta observer p0 p1 p2 ->
      begin
	let angle = triangle_space_angle_3d observer p0 p1 p2 in
	  begin
	    v_minus r0 p0 observer;
	    v_minus r1 p1 observer;
	    v_minus r2 p2 observer;
	    v_minus s0 r2 r1;
	    v_minus s1 r0 r2;
	    v_minus s2 r1 r0;
	    v_normalized xi0 s0;
	    v_normalized xi1 s1;
	    v_normalized xi2 s2;
	    v_minus buffer0 p1 p0;
	    v_minus buffer1 p2 p0;
	    xprod buffer2 buffer0 buffer1;
	    area:=0.5*.v_len(buffer2);
	    v_normalized v_zeta buffer2;
	    let r0l = v_len r0
	    and r1l = v_len r1
	    and r2l = v_len r2
	    and s0l = v_len s0
	    and s1l = v_len s1
	    and s2l = v_len s2
	    and c01 = sprod xi0 xi1
	    and c12 = sprod xi1 xi2
	    and c20 = sprod xi2 xi0
	    and zeta = sprod v_zeta r0
	    in
	      begin
		xprod eta0 v_zeta xi0;
		xprod eta1 v_zeta xi1;
		xprod eta2 v_zeta xi2;
		(* XXX should not need the pervasives qualifier here! 
		   OTOH if I remove it, I get conflicts with a (no longer
		   existing) Nlog.log function. Oh dear, this may be a subtle
		   linking issue waiting to be sorted out...
		*)
		v_log.(0) <- Pervasives.log ((r1l+.r2l+.s0l)/.(r1l+.r2l-.s0l));
		v_log.(1) <- Pervasives.log ((r2l+.r0l+.s1l)/.(r2l+.r0l-.s1l));
		v_log.(2) <- Pervasives.log ((r0l+.r1l+.s2l)/.(r0l+.r1l-.s2l));
		gamma0.(0) <- 1.0;
		gamma0.(1) <- c01;
		gamma0.(2) <- c20;
		gamma1.(0) <- c01;
		gamma1.(1) <- 1.0;
		gamma1.(2) <- c12;
		gamma2.(0) <- c20;
		gamma2.(1) <- c12;
		gamma2.(2) <- 1.0;
		let sign_zeta = sign zeta in
		let angle_pm = angle*.sign_zeta
		and denom_factor=1.0/.(8.0*.pi*. !area)
		in
		let lh0 = s0l*.denom_factor*.
		  (angle_pm*.(sprod eta0 r1)
		   -.zeta*.(sprod gamma0 v_log))
		and lh1 = s1l*.denom_factor*.
		  (angle_pm*.(sprod eta1 r2)
		   -.zeta*.(sprod gamma1 v_log))
		and lh2 = s2l*.denom_factor*.
		  (angle_pm*.(sprod eta2 r0)
		   -.zeta*.(sprod gamma2 v_log))
		in
		  begin
		    (match store_zeta with 
		       | None -> ()
		       | Some z ->
			   begin
			     z.(0) <- v_zeta.(0);
			     z.(1) <- v_zeta.(1);
			     z.(2) <- v_zeta.(2);
			   end);
		    [|lh0;lh1;lh2|]
		  end
	      end
	  end
      end
;;

(* This is a wrapper for the function above that deals with simplices,
   not just triangles. Also, internal.
*)
let lindholm_simplex3d_face_contributions =
  let zeta = Array.make 3 0.0 in
    fun 
      ?(lindholm_triangle_contributions=lindholm_triangle_contributions)
      simplex nr_face observer ->
      let p0 = simplex.ms_points.(if nr_face<=0 then 1 else 0).mp_coords
      and p1 = simplex.ms_points.(if nr_face<=1 then 2 else 1).mp_coords
      and p2 = simplex.ms_points.(if nr_face<=2 then 3 else 2).mp_coords
      and s_face_eqn = 
	match simplex.ms_inv_ext_point_coords with
	  | None -> failwith "Need triangle face equations"
	  | Some x -> x.(nr_face)
      in
	if observer=p0 || observer=p1 || observer=p2
	then [|0.0;0.0;0.0|]
	else
	  let lh = lindholm_triangle_contributions ~store_zeta:zeta observer p0 p1 p2 in
	    (* Now, we are almost done.
	       We only have to find out which way the triangle is facing.
	       That is what we have zeta for. We just insert that into the
	       directional part of the face equation.
	    *)
	    begin
	      (if zeta.(0)*.s_face_eqn.(0)+.zeta.(1)*.s_face_eqn.(1)+.zeta.(2)*.s_face_eqn.(2)>0.0
	       then 
		 begin
		   lh.(0) <- -.lh.(0);
		   lh.(1) <- -.lh.(1);
		   lh.(2) <- -.lh.(2);
		 end);
	      lh
	    end
;;

    
(* XXX TODO

let solid_angle_3d_at_mesh_edge 
    ?(fun_is_inside=(fun sx -> sx.ms_in_body <> Body_Nr 0)) mesh site =
  let p1 = mesh.ms_points.(site.(0))
  and p2 = mesh.ms_points.(site.(Array.length site-1))
  in
  let () = (if p1 == p2 then failwith "Site does not describe an edge, but a vertex!" else ()) in
  (*
     First, we have to identify those surfaces which meet in p1,p2.

     Strategy: find all faces that contain p1, p2, and a further surface point.
     Check that there are only two. Determine the angle between their
     outward-pointing surface normals. This directly tells us the reduction
     of the solid angle from the hemispherical value 2pi.
   *)
  let p1_simplices = array_filter fun_is_inside p1.mp_simplices
  and p2_simplices = array_filter fun_is_inside p2.mp_simplices
  in
  let p12_simplices = Array.of_list (array_intersection_eq p1_simplices p2_simplices) in
  let get_p12_surface_normal simplex =
    let nr_vertices = Array.length simplex.ms_points in
    let rec walk n =
      if n = nr_vertices
      then [||]
	  (* We use the empty array to satisfy the type system and express "no surface normal" *)
      else
	let p = simples.ms_points.(n) in

*)


(* Note: we directly produce a petsc matrix, rather than trying to
   abstract matrix creation issues out. Maybe we should change this later
   on...

   Note 2: it is somewhat subtle to understand why it makes sense
   trying to implement the boundary element matrix method in the
   2.5-dimensional thin-film case in such a way that only the film's
   edges and not the top and bottom layer give any contribution.
   (...but it should work!)
*)

let bem_matrix_lindholm
    ?lindholm_triangle_contributions
    ?(inside_regions=[Mesh.Body_Nr 1])
    ?(dof_name=("phi",[||]))
    ?(petsc_name=(gensym "Matrix-BEM-"))
    mwe =

  let use_stored_matrix_option = false in (* gb *)
  let bem_MX_filename = "bem.petscm" in
  let cached_mat =
    try 
      let cached = open_in bem_MX_filename in
      let () = loginfo(Printf.sprintf "A matrix will be loaded\n%!") in
      let () = close_in cached in
	Some bem_MX_filename 
    with | _ -> None
  in


  let inv_4pi = 1.0/.(4.0*.pi) in
  let mesh = mwe.mwe_mesh in
  let points = mesh.mm_points in
  let dofs = mwe.mwe_dofs in
  let nr_dofs = Array.length dofs in
  let rec list_has elem li =
    match li with
      | [] -> false
      | (head::tail) -> if head = elem then true else list_has elem tail
  in
  let region_is_inside r = list_has r inside_regions in
  let boundary_dofs = 
    array_mapfilter
      (fun dof -> 
	 let this_dof_name = the_dof_name mwe dof in
	   if this_dof_name <> dof_name then None
	   else
	     let inside_regions = List.filter (fun r -> region_is_inside r) dof.dof_in_body in
	     let outside_regions = List.filter (fun r -> not(region_is_inside r)) dof.dof_in_body in
	       (* ^ not as slow as it may seem! *)
	       if inside_regions <> [] && outside_regions <> []
	       then Some dof
	       else None)
      mwe.mwe_dofs 
  in

  let create_bem_matrix () = 
    
    let nr_boundary_dofs = Array.length boundary_dofs in
    let boundary_dof_nr_by_site =
      let ht = Hashtbl.create 100 in
      let () = Array.iteri (fun n dof -> Hashtbl.replace ht dof.dof_site n) boundary_dofs
      in ht
    in
    let mx_bem = Mpi_petsc.matrix_create ~matrix_type:"seqdense" ~auto_assembly:false
      nr_boundary_dofs nr_boundary_dofs petsc_name
    in
      
    let mesh_solid_angle =
      let dim = mwe.mwe_mesh.mm_dim in
	solid_angle_3d_at_mesh_vertex
	  ~fun_is_inside:(fun sx -> region_is_inside sx.ms_in_body)
    in
    let all_surfaces =
      array_join
        (Array.map
  	   (fun sx ->
  	      let sx_is_inside = region_is_inside sx.ms_in_body in
  		if not sx_is_inside then [||]
  		else
  		  array_mapfilteri
  		    (fun nr_face opt_sx_n ->
  		       match opt_sx_n
  		       with
  			 | None -> Some (nr_face,sx) (* outer nonmeshed space *)
  			 | Some sx_n -> 
  			     if not (region_is_inside(sx_n.ms_in_body))
  			     then Some (nr_face,sx)
  			     else None)
  		    sx.ms_neighbours
  	   )
  	   mwe.mwe_mesh.mm_simplices)
    in
    let site_buffer=Array.make 1 0 in
      begin
        for bi = 0 to nr_boundary_dofs-1 do
  	let dof_r = boundary_dofs.(bi) in
  	let solid_angle =
  	  if Array.length dof_r.dof_site <> 1 then
  	    failwith "Analytic expressions are only available for first-order meshes! (Boundary Element function (bem_matrix_lindholm))"
  	  else
  	    mesh_solid_angle points.(dof_r.dof_site.(0))
  	in
  	let contrib_diagonal = (solid_angle*.inv_4pi)-.1.0 in
  	let () =
  	  Mpi_petsc.matrix_inc mx_bem bi bi contrib_diagonal
  	in
  	  Array.iter
  	    (if mwe.mwe_mesh.mm_dim = 3
  	     then
  	       (fun (nr_face,sx) -> (* use this function for the 3d case *)
  		  let bdof_nr_0 =
  		    let () = site_buffer.(0)<-sx.ms_points.(if nr_face<=0 then 1 else 0).mp_id
  		    in Hashtbl.find boundary_dof_nr_by_site site_buffer
  		  in
  		  let bdof_nr_1 =
  		    let () = site_buffer.(0)<-sx.ms_points.(if nr_face<=1 then 2 else 1).mp_id
  		    in Hashtbl.find boundary_dof_nr_by_site site_buffer
  		  in
  		  let bdof_nr_2 =
  		    let () = site_buffer.(0)<-sx.ms_points.(if nr_face<=2 then 3 else 2).mp_id
  		    in Hashtbl.find boundary_dof_nr_by_site site_buffer
  		  in
  		  let triangle_contribs = lindholm_simplex3d_face_contributions ?lindholm_triangle_contributions sx nr_face dof_r.dof_pos
  		  in
  		    begin
  		      Mpi_petsc.matrix_inc mx_bem bi bdof_nr_0 triangle_contribs.(0);
  		      Mpi_petsc.matrix_inc mx_bem bi bdof_nr_1 triangle_contribs.(1);
  		      Mpi_petsc.matrix_inc mx_bem bi bdof_nr_2 triangle_contribs.(2);
  		    end
  	       )
  	     else (* dim = 2 *)
  	       failwith "Failure: BEM method only works for D=3!\n%!")
  	    all_surfaces
        done;
        Mpi_petsc.matrix_assemble mx_bem true;
	mx_bem;
      end
  in

    if use_stored_matrix_option then
      match cached_mat  with
	| Some bem_MX_filename -> 
	    let () = loginfo(Printf.sprintf "- Read bem matrix from file -%!") in
	    let mx_bem = Mpi_petsc.matrix_read_from_file bem_MX_filename in
	      (boundary_dofs, mx_bem) 
	| None -> 
	    let () = loginfo(Printf.sprintf "- No cached bem matrix - create one -%!") in	  
	    let mx_bem = create_bem_matrix () in
            let () = loginfo(Printf.sprintf "- Save bem matrix on file -%!") in
	    let () = Mpi_petsc.matrix_write_on_file mx_bem bem_MX_filename in
	      (boundary_dofs, mx_bem)
    else
      let mx_bem = create_bem_matrix () in
	(boundary_dofs, mx_bem)
;;

(* In our old mumag module, there still is code for a function
   bem_matrix_numerically_extrapolated, which has not been migrated
   yet. Actually, it is far too slow to be of any practical use despite
   for cross-check purposes, even though it would be able (in principle)
   to handle higher-order elements in BEM...
*)

(* Ad BEM computation of the demag field: we split the procedure in
   such a way that we do everything in a modular way and just provide a
   "core part", which uses BEM to compute the potential
   (whose gradient is H) from the sources (div M).

   Rationale: we may want to use the same computation as well e.g. for
   electrical systems.

   NOTE: the mwes we use here MUST be single-scalar-dof-mwes!
*)

let laplace_solver_bem
    ?lindholm_triangle_contributions
    ?(inside_regions=[Mesh.Body_Nr 1])
    (* ?field_mid - I am not at all sure whether we could allow
       field_mid here and this would still work... Presumably not,
       as the Green's function should come out different...
    *)
    ?(petsc_name=gensym "laplace-solver-bem")
    prematrix =
  let pname s = Printf.sprintf "%s-%s" petsc_name s in
  let mwe_le = prematrix.pmx_mwe_le
  and mwe_ri = prematrix.pmx_mwe_ri
  in
  let dirichlet_bcs =
    (* We can derive the Dirichlet boundaries from the mesh and
       knowledge of the inside regions by just looking at all DOFs and
       finding out which ones belong to inside and non-inside regions,
       and recording all combinations...
    *)
    let ht = Hashtbl.create 10 in
    let () = Array.iter
      (fun dof ->
	 let this_dof_inside_regions = list_intersection inside_regions dof.dof_in_body in
	 let this_dof_outside_regions =
	   List.filter
	     (fun r ->
		try let _ = List.find (fun x -> x=r) inside_regions in false
		with | Not_found -> true)
	     dof.dof_in_body
	 in
	   List.iter 
	     (fun r_in ->
		List.iter
		  (fun r_out -> Hashtbl.replace ht (r_in,r_out) 1)
		  this_dof_outside_regions)
	     this_dof_inside_regions)
      mwe_ri.mwe_dofs
    in Array.to_list (map_hashtbl_to_array (fun (r1,r2) v -> (r1,r2,fun _ -> true)) ht)
  in
  let () = List.iter (fun (Body_Nr r1,Body_Nr r2,_) -> Printf.printf "DBC regions=(%d,%d)\n%!" r1 r2) dirichlet_bcs in
  let (_,_,_,solver_phi1) = laplace_solver ~petsc_name:(pname "phi1") prematrix in
  let (_,_,_,solver_phi2) = laplace_solver ~dirichlet_bcs ~petsc_name:(pname "phi2") prematrix in
  let dof_name = the_dof_name mwe_ri (mwe_ri.mwe_dofs.(0)) in
  let nr_dofs = Array.length mwe_ri.mwe_dofs in
  let (boundary_dofs,mx_bem) =
    bem_matrix_lindholm
      ?lindholm_triangle_contributions
      ~inside_regions
      ~petsc_name:(pname "MX-BEM") mwe_ri
  in
  let nr_boundary_dofs = Array.length boundary_dofs in
  let dof_nr_to_bdof_index =
    let a = Array.make nr_dofs (-1) in
    let () = Array.iteri (fun n dof -> a.(dof.dof_nr) <- n) boundary_dofs in
      a
  in
  let make_petsc_vector s n = 
    let v = Mpi_petsc.vector_create s (pname n) in let () = Mpi_petsc.vector_assemble v in v
  in
  let v_phi1 = make_petsc_vector nr_dofs "phi1" in
  let v_zero = make_petsc_vector nr_dofs "zero" in
  let field_phi1 = FEM_field(mwe_ri,None,v_phi1)
  and cofield_zero = FEM_cofield(mwe_le,None,v_zero)
  in
  let phi1_boundary_vals = make_petsc_vector nr_boundary_dofs "bdofs-phi1" in
  let phi2_boundary_vals = make_petsc_vector nr_boundary_dofs "bdofs-phi2" in
    (* -- *)
  let solver ?target (cofield_div:(float fem_cofield)) =
    let v_target =
      match target with
	| None -> make_petsc_vector nr_dofs (gensym "result-")
	| Some (FEM_field (mwe,_,v) as field) ->
	    let () = ensure_field_is_unrestricted field in
	    if mwe != mwe_ri
	    then failwith (Printf.sprintf "laplace_solver_bem solver: target field MWE mismatch! (wanted: %s, got: %s)" mwe_ri.mwe_name mwe.mwe_name)
	    else v
    in
    let (FEM_cofield (mwe_co,_,v_co)) = cofield_div in
    let () = ensure_cofield_is_unrestricted cofield_div in
    let () = (if mwe_co != mwe_le
	      then failwith "laplace_solver_bem solver: co-field MWE mismatch!"
	      else ())
    in
    let field_phi1=solver_phi1 ~target:field_phi1 cofield_div in
    let () =
      Mpi_petsc.with_petsc_vector_as_bigarray v_phi1
	(fun ba_phi1 ->
	   begin
	     (* Transfer boundary DOF data to short vector
		XXX Can be improved (use vector_set variant that manipulates
		multiple entries at once!)
	     *)
	     for i=0 to nr_boundary_dofs-1 do
	       Mpi_petsc.vector_set phi1_boundary_vals i ba_phi1.{boundary_dofs.(i).dof_nr};
	     done;
	     (* Apply BEM matrix *)
	     let () = Mpi_petsc.matrix_times_vector mx_bem phi1_boundary_vals phi2_boundary_vals in
	       (* We now can extract the values for the Diriclet BCs for phi2 from this vector. *)
	     let ml_phi2_bv = Mpi_petsc.vector_extract phi2_boundary_vals in
	       (* XXX vector_extract should accept a ~target argument! *)
	       (* For phi2, we need a "zero sources" vector which we feed into our laplace solver. *)
	     let () =
	       Mpi_petsc.vector_scale v_zero 0.0 (* XXX should be: field_scale (for clarity)! *)
	     in
	     let field_phi2 =
	       solver_phi2
		    ~target:(FEM_field (mwe_ri,None,v_target))
		 ~bc_fun:(fun _ dof ->
			    (*let () = Printf.printf "solver_phi2 dof_nr=%d bdof_nr=%d\n%!" dof.dof_nr dof_nr_to_bdof_index.(dof.dof_nr) in*)
			    ml_phi2_bv.(dof_nr_to_bdof_index.(dof.dof_nr)))
		 cofield_zero (* We just cleared this! *)
	     in 
	       (* Now, phi2 is in the target vector. Still have to add phi1.
		  Plus, we may have to divide by thickness
	       *)
	       (* XXX Note: this presumably is wrong:
		  let thickness_factor = match thickness with | None -> 1.0 | Some th -> 1.0/.th in
		  Mpi_petsc.vector_AXPBY thickness_factor thickness_factor v_phi1 v_target
	       *)
	       Mpi_petsc.vector_AXPBY 1.0 1.0 v_phi1 v_target
	   end
	)
    in FEM_field(mwe_ri,None,v_target)
  in solver
;;


(* Notes:
   (1) Computing H_demag for thin film systems is tricky and requires
   entirely different techniques; we do not try to support this here.

   (2) Note that we will want to use the same techniques for
   the computation of ferroelectric depolarization fields; hence,
   it is important to make names parametric
*)

let demag_fun_diffop_div_m_string scalar_name prefix_m materials =
  Printf.sprintf "%s, j:3"
    (String.concat " "
       (Array.to_list
	  (Array.map (fun mat ->
			let coeff = mat.llg_Ms
			and name = mat.llg_name
			in
			  Printf.sprintf "%c%f*<%s|| d/dxj %s%s(j)>"
			    (if coeff > 0.0 then '+' else '-') (abs_float coeff)
			    scalar_name prefix_m name)
	     materials)))
;;

let make_demag_fun_3d
    ?(petsc_name=gensym "demag_fun_3d") (* XXX petsc_name is unused now,
					   but we will have to change
					   this for the future! *)
    ?lindholm_triangle_contributions
    ?(fun_make_and_register_mwe_field=
      (fun mwe -> (mwe,make_field ~constant_value:0.0 mwe))) (* XXX this unfortunately is a little bit hackish! *)
    ?(sloppy_cofield_to_field=true)
    ?(name_demag="H_demag")
    ?(name_rho="rho")
    ?(name_phi="phi")
    ?(diffop_div_m_string="")
    ?(order=1)
    ?(inside_regions=[Mesh.Body_Nr 1])
    ?(fem_only=false)
    all_mats (* XXX actually, unused, but will be used in the linalg_machine replacement of this function,
		hence this must be part of the signature!
	     *)
    mwe_h mwe_m
    =
  let mesh = mwe_m.mwe_mesh in
  let () = 
    (if mwe_h.mwe_mesh != mwe_m.mwe_mesh
     then failwith "make_demag_fun_3d: mwe meshes mismatch!"
     else ())
  in
  let dim_space = mesh.mm_dim in
  let () = 
    (if dim_space <> 3
     then failwith "make_demag_fun_3d: mesh is not 3-dimensional!"
     else ())
  in
  let elem_scalar = make_element dim_space ("S",[||]) order in
    (* used for both rho and phi *)
  let mwe_scalar =
    make_mwe "mwe_S" (* was: (gensym "demag-scalar") *)
      (fun sx -> elem_scalar) mesh
  in
  let mwe_rho = mwe_sibling "mwe_rho" "rho_from_S_" [|("S",name_rho)|] mwe_scalar in
  let mwe_phi = mwe_sibling "mwe_phi" "phi_from_S_" [|("S",name_phi)|] mwe_scalar in
    (* Note that we allocated scalar degrees of freedom to every node!
       So, we can use this for both "Dirichlet-at-infinity" FEM/BEM as well as for
       "Dirichlet-at-outer-boundary-of-meshed-exterior" FEM-only.
    *)
  let diffop_laplace_string = Printf.sprintf "-<d/dxj %s || d/dxj %s>, j:3" name_rho name_phi in
  let diffop_grad_string = Printf.sprintf "-<%s(j) ||d/dxj %s>, j:3" name_demag name_phi in
  let prematrix_div =
    diffop_prematrix
      ~ignore_jumps:false
      (diffop_from_string diffop_div_m_string)
      mwe_rho mwe_m
  in
  let prematrix_laplace =
    diffop_prematrix_compatibility_newmethod
      (diffop_from_string diffop_laplace_string)
      mwe_rho mwe_phi
  in
  let prematrix_grad =
    diffop_prematrix_compatibility_newmethod
      (diffop_from_string diffop_grad_string)
      mwe_h mwe_phi
  in
  let app_div =
    prematrix_to_applicator
      ~interface_coeffs:(Array.map (fun region -> (Some region, None,1.0))
			   (Array.of_list inside_regions))
      prematrix_div
  in
  let laplace_solver = 
    if fem_only then
      let max_region = Array.fold_left (fun m sx -> let Body_Nr r_sx = sx.ms_in_body in max m r_sx) (-1) mesh.mm_simplices in
      let dirichlet_bcs =
	let vac = Body_Nr (-1)
	and fun_true = fun _ -> true in
	  Array.to_list (Array.init (1+max_region)
			   (fun n -> (Body_Nr (-1),Body_Nr n, fun_true)))
      in
      let (_,_,_,solver) = laplace_solver ~dirichlet_bcs prematrix_laplace in 
	fun ?target cofield ->
	  solver ?target ~bc_fun:(fun _ _ -> 0.0) cofield
    else
      laplace_solver_bem ?lindholm_triangle_contributions
	~inside_regions prematrix_laplace
  in
  (* let () = Printf.printf "=== DDD prematrix laplace ===\n" in let () = debugprint_prematrix prematrix_laplace in (* DDD *) *)
  let buffer_rho_c = make_cofield ~constant_value:0.0 mwe_rho in
  let (_,buffer_rho) = fun_make_and_register_mwe_field mwe_rho in
  let (_,buffer_phi) = fun_make_and_register_mwe_field mwe_phi in
  if sloppy_cofield_to_field
  then
    let app_grad = prematrix_to_applicator_ff prematrix_grad in
    let demag_fun
	?(compute_field_rho=false)
	?(debug_charge_imbalance=false)
	?target field_m =
      let cofield_rho = app_div ~target:buffer_rho_c field_m in
      let field_phi = laplace_solver ~target:buffer_phi cofield_rho in
      let result = app_grad ?target field_phi in
      let () = 
	(if compute_field_rho
	 then
	     ignore(cofield_to_field ~target:buffer_rho buffer_rho_c)
	 else ())
      in
      let _ =
	(if not(debug_charge_imbalance) then ()
	 else
	     let FEM_cofield (_,_,data_rho) = cofield_rho in
	     let () = ensure_cofield_is_unrestricted cofield_rho in
	     let m_charges = Mpi_petsc.vector_extract data_rho in
	     let m_total = Array.fold_left (+.) 0.0 m_charges in
	     let () = logdebug (Printf.sprintf "DDD Magnetic charge imbalance: %f%!" m_total)
	     in
	       ())
      in
	result
    in
      demag_fun
  else
    let () = failwith "XXX non-sloppy_cofield_to_field should not be used!" in
    (* ...at least while we are debugging. Conceptual issues remain to
       be resolved with this approach as this would lead to non-sparse
       Jacobi matrices! *)
    let app_grad = prematrix_to_applicator prematrix_grad in
    let buffer_h = make_cofield ~constant_value:0.0 mwe_h in
    let demag_fun
	?(compute_field_rho=false)
	?(debug_charge_imbalance=false)
	?target field_m =
      let cofield_rho = app_div ~target:buffer_rho_c field_m in
      let field_phi = laplace_solver ~target:buffer_phi cofield_rho in
      let cofield_h = app_grad ~target:buffer_h field_phi in
      let result = cofield_to_field ?target cofield_h in
      result
    in
    demag_fun
;;

(* Note: this is for spatial field variations;
   we may want to include inter-field exchange
   contributions later on.
*)

let make_exchange_fun
    ?(petsc_name=gensym "exchange_fun") (* XXX unused now, but we will have
					   to change this for the future! *)
    ?(sloppy_cofield_to_field=true)
    ?(name_exchange="H_exch")
    all_materials
    mwe_h mwe_m =
  (* let () = failwith "XXXXXX -- mumag2 make_exchange_fun -- dead code, no longer in use - we are now using a linalg_script for this! XXXXXX" in *)
  let mesh = mwe_m.mwe_mesh in
  let dim = mesh.mm_dim in
  let diffop_str =
    let h_summands =
      String.concat " "
	(Array.to_list
	   (Array.map
	      (fun mat ->
		 let coeff = -.mat.llg_J in 
		   if coeff=0.0 then ""
		   else
		     Printf.sprintf "%c%f*<d/dxj %s_%s(k) || d/dxj m_%s(k)>"
		       (if coeff > 0.0 then '+' else '-')
		       (abs_float coeff) (* mf: shouldn't be easier to use "%+f"? *)
		       name_exchange
		       mat.llg_name
		       mat.llg_name)
	      all_materials))
    in
      if h_summands = "" then ""
      else
	Printf.sprintf "%s, j:%d,k:3" h_summands dim
  in
  let diffop_exch = diffop_from_string diffop_str in
  let pmx = diffop_prematrix_compatibility_newmethod diffop_exch mwe_h mwe_m in
  if sloppy_cofield_to_field
  then
    let app = prematrix_to_applicator_ff pmx in
    (pmx.pmx_ht_bulk,app)
  else
    let app = prematrix_to_applicator pmx in
    let buffer_cofield = make_cofield mwe_h in
    let fun_H_exch ?target m =
      let cofield = app ~target:buffer_cofield m in
      cofield_to_field ?target cofield
    in
    (pmx.pmx_ht_bulk,fun_H_exch)				
      (* We also do export the prematrix as we will need it
	 for the computation of the Jacobian! *)
;;

(* Parts of a new interface to the Landau-Lifshitz-Gilbert equation
   which will eventually allow us to use the "sundials" library for
   time-stepping:
*)

let make_fun_to_compute_H_total
    ?(strict_mwe_check=false)
    ?(basic_equation=("MAG","j","H_total_MAG(j)=H_demag(j)+H_exch_MAG(j)+H_anis_MAG(j)+H_ext(j);"))
    ?(extra_contribs_by_magnetization_name=[||]) (* Array of pairs of (magnetization name,extra_code) *)
    ?(intensive_parameter_names=[||])
    magnetization_names
    v_mwes
    =
  let (mag_metavariable,loop_variable,mag_template)=basic_equation in
  let rx_metavar = Str.regexp (Str.quote mag_metavariable) in
  let instantiate_h_line mname = Str.global_replace rx_metavar mname mag_template in
  let mag_blocks = Array.map
    (fun mname ->
       let extra_block = 
	 try
	   let (_,code)=array_find (fun (a,_) -> a=mname) extra_contribs_by_magnetization_name in
	     code
	 with | Not_found -> ""
       in
	 Printf.sprintf
"if(have_H_total_%s)
 {
  for(%s=0;%s<3;%s++){
     %s
  }
  %s
 }
"
	   mname loop_variable loop_variable loop_variable
	   (instantiate_h_line mname)
	   extra_block
    )
    magnetization_names
  in
  let ccode=String.concat "\n" (" int j;\n\n"::(Array.to_list mag_blocks)) in
  let fun_exec =
    site_wise_execute ~strict_mwe_check v_mwes [||] intensive_parameter_names ccode
  in
    fun_exec
;;

(* We use a modified Landau-Lifshitz-Gilbert equation in the form

   (NOTE: alpha, beta, gamma have been renamed c1, c2, c3)

   dm/dt(j) =   alpha * eps_jkl * m(k) * h_total(l)
              + beta  * eps_jkl * eps_lnp *  m(k) * m(n) * h_total(p)
              + gamma * m(j)*(1-m^2)

   where m is (a) reduced magnetization: |m|=1.

   There are some advantages of using 1-m^2 instead of 1-|m| that come
   from not having to deal with square roots.

   Note that this is general enough to capture all the usual LLG effects,
   but we may have to map the usual parameters to obtain our form.

   The gamma term does not actually change the physics, as dm/dt _|_ m
   as long as m is unit-normalized, so if it starts out with |m|=1, it
   will physically stay so and the gamma term does not contribute.
   If, however, there is some deviation due to numerical drift, this
   term can combat it. large gamma means that |m| will be quite
   "stiff" and not drift much, but |m| correction can reduce the
   timestepping step size considerably, while small gamma may bring a
   speedup at the expense of |m| deviating a bit more from unity. Is
   this a problem? Presumably not, since the interpolated value of m
   inside a FEM cell does deviate quite noticeably from unity
   anyway. Note that there are many advantages to resolve the
   fixed-length issue at the level of the equations of motion, in
   particular for multistep time integration methods where we just
   want to plug this into...

   Note:

    beta  * eps_jkl * eps_lnp * m(k) * m(n) * h_total(p)
  = beta* 2 delta(jk,np) m(k)m(n)h(p)
  = beta*(m(j)m(k)h(k) - m(k)m(k)h(j))



*)

let make_fun_to_compute_llg_rhs
    ?(strict_mwe_check=true)
    llg_materials
    v_mwes
    =
  (* Note: v_mwes must provide all the magnetizations,
     plus every dmdt_MAG, plus every H_total_mag.
     We will then fill the dmdt_MAG.
  *)
  let llg_blocks = Array.map
      (fun llg_mat ->
	String.concat ""
	  [
	   "
if(have_m_";llg_mat.llg_name;") {
  static double m[3], len_m_sq, mb, len_m_deviation, a[3], b[3];
  static double alpha, beta, gamma, stt_c1, stt_c2;
  int i, k;

#if defined (have_grad_m_";llg_mat.llg_name;") && defined (have_J)
  double dm_dJ[3];

  /* Compute J*grad_m */
  for(i=0; i<3; i++) {
    dm_dJ[i] = 0.0;
    for(k=0; k<3; k++)
      dm_dJ[i] += grad_m_";llg_mat.llg_name;"(i, k)*J(k);
  }

#  define add_y_dm_dJ(x, y, i) ((x) + (y)*dm_dJ[i])
#else
#  define add_y_dm_dJ(x, y, i) (x)
#endif

  alpha = ";(string_of_float llg_mat.llg_c1);";
  beta = ";(string_of_float llg_mat.llg_c2);";
  gamma = ";(string_of_float llg_mat.llg_c3);";
  stt_c1 = ";(string_of_float llg_mat.llg_stt_c1);";
  stt_c2 = ";(string_of_float llg_mat.llg_stt_c2);";

  for(i=0; i<3; i++) {
     double hi = H_total_";llg_mat.llg_name;"(i);
     m[i] = m_";llg_mat.llg_name;"(i);
     a[i] = add_y_dm_dJ(alpha*hi, stt_c1, i);
     b[i] = add_y_dm_dJ(beta*hi, stt_c2, i);
  }

#if 0
  printf(\"grad_m_";llg_mat.llg_name;" =\\n\");
  for(i=0; i<3; i++) {
    for(k=0; k<3; k++)
      printf(\"%g \", grad_m_";llg_mat.llg_name;"(i, k));
    printf(\"\\n\");
  }
  printf(\"m_";llg_mat.llg_name;" = (%g, %g, %g)\\n\", m[0], m[1], m[2]);
  printf(\"dm_dJ = (%g, %g, %g)\\n\", dm_dJ[0], dm_dJ[1], dm_dJ[2]);
  printf(\"dm_dJ_a = (%g, %g, %g)\\n\", add_y_dm_dJ(0, stt_c1, 0), add_y_dm_dJ(0, stt_c1, 1), add_y_dm_dJ(0, stt_c1, 2));
  printf(\"dm_dJ_b = (%g, %g, %g)\\n\", add_y_dm_dJ(0, stt_c2, 0), add_y_dm_dJ(0, stt_c2, 1), add_y_dm_dJ(0, stt_c2, 2));
#endif

  len_m_sq = m[0]*m[0] + m[1]*m[1] + m[2]*m[2];
  mb = m[0]*b[0] + m[1]*b[1] + m[2]*b[2];
  len_m_deviation = 1.0 - len_m_sq;

  dm_dt_";llg_mat.llg_name;"(0) =
     gamma * m[0]*len_m_deviation
   + m[1]*a[2] - m[2]*a[1]
   + m[0]*mb - b[0]*len_m_sq;

  dm_dt_";llg_mat.llg_name;"(1) =
     gamma * m[1]*len_m_deviation
   + m[2]*a[0] - m[0]*a[2]
   + m[1]*mb - b[1]*len_m_sq;

  dm_dt_";llg_mat.llg_name;"(2) =
     gamma * m[2]*len_m_deviation
   + m[0]*a[1] - m[1]*a[0]
   + m[2]*mb - b[2]*len_m_sq;
}
"
	 ])
      llg_materials
  in
  let ccode=String.concat "\n" (Array.to_list llg_blocks) in
  let fun_exec = site_wise_execute ~strict_mwe_check v_mwes [||] [||] ccode
  in
  fun_exec
;;

(* Unfortunately, we need more than this for sundials: we also have to
   get a rough idea of the system's Jacobian...

   Actually, this is not an unfeasible task, provided we do know some
   approximation to dh_total/dm (we will take the H_exch matrix here, 
   i.e. dh_exch/dm), as well as the current magnetization. 

   Back to the LLG, which now reads:

   dm/dt(j) =  
     alpha * eps_jkl * m(k) * h(l)
   + beta*(m(j)m(k)h(k) - m(k)m(k)h(j))
   + gamma * m(j)*(1-m^2)

   For determining the Jacobian, we drop the gamma term as this is a
   rather harmless correction anyway that "usually is zero". It may
   turn out that this is over-simplifying things and we really have to
   try harder, but for now this presumably is fine.

   So, what do we expect our Jacobian to look like? We get site-local
   contributions from the d[rhs]/dm(k), as well as cross-site
   contributions from dh/dm. Analytically, it reads - with H being the
   dh/dm matrix:

   J_Pp,Qq = (* P,Q denote the site and magnetization type, p,q vector indices *)
    delta_PQ* (* site-local contributions *)
   (   alpha * eps_pqs * h(s)
     + beta  * (delta(pq)*m(s)h(s) + m(p)*h(q) - 2*m(q)*h(p))
   )
   + (* potentially cross-site contributions *)
     alpha * eps(pks) * m(Pk)* H[Ps,Qq]
   + beta * (m(Pp) * m(Pk) * H[Pk,Qq] - m(Pk)*m(Pk) * H[Pp,Qq])

   Evidently, we should build this matrix by walking through all
   matrix entries of H, with the hind index first...

   Can/should we make the Jacobian from the Caml side? Presumably not,
   because this would involve an awful lot of calls from Caml into C
   for setting every single matrix element... Actually, we are much
   better off by passing on a "Jacobian plan" matrix to C which
   contains all the information on what matrix element to set
   to what value...

   How does such a plan work? We actually can combine the dh/dm matrix
   entries with the alpha/beta and combinatorical +/-1 coefficients into
   an overall numerical coefficient. Then, every J-entry is of order
   0,1, or 2 in m.

   So, a J-plan is an aggregation of (float * int array), where
   the float is a coefficient, and the entries in the int array are:

   [|target_index_left;
     target_index_right;
     h_total_index; (* or (-1) if we should not include a h_total contribution *)
     (the rest are m-indices to be multiplied)
   |]

   Ad parallelization: for the sake of building this matrix, it might
   make sense to ensure every machine has a copy of the full M-vector...

   Note that if we just encoded this as a (float * int array) array,
   then we may (on 32-bit systems) run into the
   max-2-mio-array-entries limit of OCaml. We do this for now, to get
   things going, but this will have to be changed later on!
 *)

let llg_jacobi_plan
    ?(prefix_dofs_m="m_")
    ?(prefix_dofs_h="H_total_")
    ht_dh_by_dm (* [|row;col|] => coeff *)
    mwe_m
    mwe_h_total
    llg_materials
    =
  let () =
    (if not(mwes_are_compatible mwe_m mwe_h_total)
     then failwith "PROBLEM: incompatible MWEs given for M and H_total!"
     else ())
  in
  let () = mwe_ensure_has_inv_volumes mwe_m in
  (* let () = mwe_ensure_has_g_solver mwe_m in *)
  let m_inv_vols =
    (* XXX actually, this "almost is a bug": we rather would need h_inv_vols,
       but this does not matter here, as H_exch is derived from m.
       XXX Fix it nevertheless!
     *)
    Array.map
      (fun d -> let x = d.dof_volume in if x = 0.0 then 0.0 else 1.0/.x)
      mwe_m.mwe_dofs 
  in
  let mat_by_stem =
    let ht = Hashtbl.create 10 in
    let () =
      Array.iter
	(fun m -> Hashtbl.add ht m.llg_name m)
	llg_materials
    in
      fun name -> Hashtbl.find ht name
  in
    (* Note: Translating all the Jacobi formula mash into code manually is
       - boring
       - tedious
       - error-prone
       - hard to debug
       Hence, it is WAY better to do this in a machine-assisted manner.
       How? Ideally, auto-generation from the LLG would be great.
       But if we want to invest somewhat less effort, then a simplified 
       common language to express contributions would be nice...
       
       Note again that this actually would boil down to having database
       capabilities in code, as we want to combine sparse matrices...
       
     Actually, the amount of sparse-ness is not that large. It really
       is sufficient to know for every site all the "neighbours" in the
       sense that the H-matrix does have corresponding entries. (Here,
       we implicitly assume that the H-matrix is quite sparse, usually
       only covering nearest-neighbour stuff...
    *)
  let with_dof_name_stripped prefix (dof_stem,indices) f =
    let len_prefix = String.length prefix in
      if not(string_compare_n len_prefix prefix dof_stem)
      then ()
      else f (String.sub dof_stem len_prefix (String.length dof_stem-len_prefix))
  in
  let do_delta p q f = if p = q then f () else () in
  let do_eps f =
    begin
      f 1.0 0 1 2;
      f 1.0 1 2 0;
      f 1.0 2 0 1;
      f (-1.0) 2 1 0;
      f (-1.0) 1 0 2;
      f (-1.0) 0 2 1;
    end
  in
  let ht_plan_contribs = Hashtbl.create 1000 in
    (* -- *)
  let register_contrib
      ?(dh_dm_left=(-1)) ?(dh_dm_right=(-1))
      j_index_left j_index_right h_index m_indices coeff =
    try
      let coeff_with_dh_dm_factor =
	match (dh_dm_left,dh_dm_right) with
	  | (-1,-1) -> coeff
	  | (ix_le, ix_ri) ->
	      let c = Hashtbl.find ht_dh_by_dm [|ix_le;ix_ri|]
		(* NOTE: this MAY raise NOT_FOUND. No Problem, we want it that way! *)
	      in c*.coeff*.m_inv_vols.(ix_le)
		(* Note that we have to include the inverse-dof-volume contribution here! *)
	  | _ -> impossible()
      in
	hashtbl_push ht_plan_contribs
	  (j_index_left,j_index_right)
	  (coeff_with_dh_dm_factor,
	   Array.init (3+(Array.length m_indices))
	     (fun n ->
		match n with
		  | 0 -> j_index_left
		  | 1 -> j_index_right
		  | 2 -> h_index
		  | _ -> m_indices.(n-3)))
    with | Not_found -> ()
  in
    (* -- *)
  let dof_sites_m = hashtbl_keys mwe_m.mwe_dofs_by_site in
  let ht_site_neighbours_m_of_h =
    let h = Hashtbl.create 100 in
    let () = Hashtbl.iter
      (fun indices coeff ->
	let nr_indices = Array.length indices in
	let ix_le=indices.(0)
	and ix_ri=indices.(1)
	in
	let site_h = mwe_h_total.mwe_dofs.(ix_le).dof_site
	and site_m = mwe_m.mwe_dofs.(ix_ri).dof_site
	in 
	let old_neighbours =
	  try Hashtbl.find h site_h with
	  | Not_found ->
	      let z = [site_h] in
	      let () = Hashtbl.add h site_h z in
	      z
		(* We explicitly do make sure we do contain the diagonal! *)
	in
	try 
	  let _ = List.find (fun x -> x=site_m) old_neighbours in ()
	    (* do nothing if we already had that neighbour... *)
   	with
	| Not_found -> 
	    Hashtbl.replace h site_h (site_m::old_neighbours))
	ht_dh_by_dm 
    in 
    h
  in
  let ix0 = [|0|] in
  let do_for_corresponding_hm_dofs site_h site_m f =
    let h_dofs_here = try Hashtbl.find mwe_h_total.mwe_dofs_by_site site_h with | Not_found -> [||]
    and m_dofs_here = try Hashtbl.find mwe_m.mwe_dofs_by_site site_h with | Not_found -> [||]
    and m_dofs_there = try Hashtbl.find mwe_m.mwe_dofs_by_site site_m with | Not_found -> [||]
      (* Why do we carry along m_dofs_here as well as m_dofs_there?
	 Answer: We need it for indexing the target matrix J(Pp,Qq) which carries
	 M-indices left and right.
      *)
    in
      (* We need to group them by their names. But here we can use a
	 trick: we just sort M and H_total dofs lecxicographically by their
	 full (dof_name,indices); this will bring them into proper order,
	 which is already half of the job.
	 
	 The remaining problem is that with neighbouring sites, 
	 h_dofs_here may not correspond so nicely to h_dofs_there,
	 so we have to be slightly inventive...
      *)
    let nr_h_dofs = Array.length h_dofs_here 
    and nr_m_dofs = Array.length m_dofs_there
    in
    let rec walk_dofs offset_h offset_mL offset_mR =
      if offset_h >= Array.length h_dofs_here
         || offset_mL >= Array.length m_dofs_here
         || offset_mR >= Array.length m_dofs_there
      then ()
      else
	let (ddd_name_h,ix_h) as dof_name_h = the_dof_name mwe_h_total h_dofs_here.(offset_h) in
	let (ddd_name_mL,ix_mL) as dof_name_mL = the_dof_name mwe_m m_dofs_here.(offset_mL) in
	let (ddd_name_mR,ix_mR) as dof_name_mR = the_dof_name mwe_m m_dofs_there.(offset_mR) in
	if ix_h <> ix0 then walk_dofs (1+offset_h) offset_mL offset_mR
	else if ix_mL <> ix0 then walk_dofs offset_h (1+offset_mL) offset_mR
	else if ix_mR <> ix0 then walk_dofs offset_h offset_mL (1+offset_mR)
	    (* This is quite conservative and ensures we are on the safe side should we
	       intermediately encounter dofs that have other indices...
	     *)
	else
	  with_dof_name_stripped prefix_dofs_h dof_name_h
	    (fun h_stem ->
	      with_dof_name_stripped prefix_dofs_m dof_name_mL
		(fun mL_stem ->
		  let stem_compare = compare h_stem mL_stem in
		  if stem_compare = -1 then walk_dofs (1+offset_h) offset_mL offset_mR
		  else if stem_compare = 1 then walk_dofs offset_h (1+offset_mL) offset_mR
		  else
		    (* h_stem = mL_stem *)
		    with_dof_name_stripped prefix_dofs_m dof_name_mR
		      (fun mR_stem ->
			let stem_compare = compare mL_stem mR_stem in
			if stem_compare = -1 then walk_dofs offset_h (1+offset_mL) offset_mR
			else if stem_compare = 1 then walk_dofs offset_h offset_mL (1+offset_mR)
			else
			  let v_indices_mL_dofs = Array.init 3 (fun n -> m_dofs_here.(offset_mL+n).dof_nr)
			  and v_indices_mR_dofs = Array.init 3 (fun n -> m_dofs_there.(offset_mR+n).dof_nr)
			  and v_indices_h_dofs = Array.init 3 (fun n -> h_dofs_here.(offset_h+n).dof_nr)
			  in
			  let () = f site_h site_m h_stem v_indices_h_dofs v_indices_mL_dofs v_indices_mR_dofs in
			  (* Note that we do pass the stem as well, so that f may choose field-dependent
			     LLG parameters like alpha...
			   *)
			  walk_dofs (3+offset_h) (3+offset_mL) (3+offset_mR))))
    in walk_dofs 0 0 0
  in
  let do_for_sites_pq f =
    Hashtbl.iter
      (fun site_h sites_m ->
	 List.iter
	   (fun site_m -> f site_h site_m)
	   sites_m)
      ht_site_neighbours_m_of_h
  in
  let do_for_sites_and_hm_dofs f =
    do_for_sites_pq
      (fun site_H site_M ->
	 do_for_corresponding_hm_dofs site_H site_M f)
  in
    (* Note: we do have a very slight problem here as our matrix 
       contribution representation does not allow us to save the work
       involved in repeated computation of h*m and m*m... For now,
       let us regard this as rather harmless...
    *)
  let () =
    do_for_sites_and_hm_dofs
      (fun site_H site_M dof_name_stem dof_indices_h dof_indices_mp dof_indices_mq ->
	 let mat = mat_by_stem dof_name_stem in 
	 let alpha = mat.llg_c1
	 and beta = mat.llg_c2
	 in
	   begin
	     do_delta site_H site_M (* site-diagonal contributions *)
	       (fun () ->
		  begin
		    do_eps
		      (fun c ei ej ek ->
			 register_contrib	(* alpha * eps_pqs * h(s) - WE ARE VERY SURE ABOUT THAT ONE. *)
			   dof_indices_mp.(ei) dof_indices_mq.(ej)
			   dof_indices_h.(ek) [||] (c*.alpha));
		    for p=0 to 3-1 do
		      for q=0 to 3-1 do
			register_contrib	(* beta * delta(pq) * m(s)h(s) *)
			  dof_indices_mp.(p) dof_indices_mq.(p)
			  dof_indices_h.(q) [|dof_indices_mp.(q)|] beta;
			register_contrib	(* beta * m(p)h(q) *)
			  dof_indices_mp.(p) dof_indices_mq.(q)
			  dof_indices_h.(q) [|dof_indices_mp.(p)|] beta;
			register_contrib	(* -2 beta * m(q)h(p) *)
			  dof_indices_mp.(p) dof_indices_mq.(q)
			  dof_indices_h.(p) [|dof_indices_mp.(q)|] (-2.0*.beta);
		      done; done;
		  end
	       );
	     (* potentially off-diagonal contributions *)
	     do_eps		     (* alpha * eps(pks) * m(Pk) H[Ps,Qq]  *)
	       (fun c ep ek es ->
		  for q=0 to 3-1 do
		    register_contrib
		      ~dh_dm_left:dof_indices_h.(es)
		      ~dh_dm_right:dof_indices_mq.(q)
		      dof_indices_mp.(ep) dof_indices_mq.(q)
		      (-1) [|dof_indices_mp.(ek)|] (c*.alpha);
		  done
	       );
	     for p=0 to 3-1 do
	       for q=0 to 3-1 do
		 for k=0 to 3-1 do
		   register_contrib		(* beta * m(Pp) * m(Pk) * H[Pk,Qq] *)
		     ~dh_dm_left:dof_indices_h.(k)
		     ~dh_dm_right:dof_indices_mq.(q)
		     dof_indices_mp.(p) dof_indices_mq.(q)
		     (-1) [|dof_indices_mp.(p);dof_indices_mp.(k);|] beta;
		   register_contrib		(* - beta * m(Pk) * m(Pk) * H[Pp,Qq] *)
		     ~dh_dm_left:dof_indices_h.(p)
		     ~dh_dm_right:dof_indices_mq.(q)
		     dof_indices_mp.(p) dof_indices_mq.(q)
		     (-1) [|dof_indices_mp.(k);dof_indices_mp.(k);|] (-.beta);
		 done;
		 done;
		 done
	   end
      )
  in
    (* Now we registered all the contributions.
       Finally, turn the plan into something
       that is more palatable to C code:
       an array of arrays of entries.
    *)
  let nr_entries_total =
    let r = ref 0 in
    let () = Hashtbl.iter (fun _ v -> r:= !r + List.length v) ht_plan_contribs in
      !r
  in
  let result = (* For now, we do things in a slightly dumb way by going through a list array *)
    let rcount = ref 0 in
    let a = Array.make (1+(nr_entries_total/1000000)) [] in
    let () = Hashtbl.iter
      (fun _ v ->
	 let ix_a = !rcount / 1000000 in
	 let extended_entry = List.append v a.(ix_a) in
	 let () = a.(ix_a) <- extended_entry in
	   rcount := !rcount + (List.length v))
      ht_plan_contribs
    in Array.map Array.of_list a
  in
  result
;;

external jacobi_plan_executor:
  (float * int array) array array ->
  Mpi_petsc.matrix ->
  Mpi_petsc.vector -> Mpi_petsc.vector -> unit
  = "caml_mumag_jacobi_plan_executor"
;;

(* Here, we do encounter an ugly issue: in order to execute the Jacobi
   plan, we need both M and H_total. But we (1) cannot easily compute
   H_total from M, and on the other hand, we also cannot just fuse M and
   H_total into a common state vector y (at the very least, this would be
   an extremely tasteless hack, as "the H part" of ydot then does not
   make too much sense...)

   We may, however, (1) store associations of M-fields computed at
   certain times to corresponding H-fields, and use that information,
   or (2) just accept our imperfection in that respect and use the
   last computed H_total whenever we need some H_total. (What does
   this effectively mean? We are assuming that H_total is not changing
   on the timescales we use to determine how M changes? Sounds quite
   silly, doesn't it?) The latter thing, by the way, basically is what
   magpar does.

   I presume the most reasonable strategy hence is to keep a backlog
   of the 10 (say) last computed H_total fields, as well as the
   simulation times they correspond to, and whenever we need a H for a
   given time, we search that history queue, taking the one for which
   time gives the closest match, and if there should be multiple of
   these, taking the last-computed.
*)

type ty_fun_advance_time = (float array -> float -> int -> float * float * ((string * float * float) array) * float Fem.fem_field * float Fem.fem_field)


let _ddd_ref_timings = ref [||];;	(* DDD for debugging only! *)

let mumag_cvode_time_integrator 
    ?(petsc_name=gensym "cvode")
    ?(tuning_params=(1e-6,1e-6,2,300)) 
    (* sundials rel_tolerance abs_tolerance max_order krylov_max
       Note: we do not provide any KSP tolerance fine-tuning parameters here yet,
       but if we did, this would be the place where they should go...
    *)
    ?(prefix_dofs_m="m_")
    (* XXX Does this make sense? I think we use strict conventions on the name dm_dt,
       so this may not be too reasonable after all...
    *)
    ?(prefix_dofs_h="H_total_")
    ?field_J_grad_m (* The spin polarised current density and grad_M,
                       whose presence enables the spin-transfer-torque
                       contributions to LLG *)
    llg_materials
    prematrix_h_exch
    intensive_params_names
    (fun_compute_H_total:(?target:float Fem.fem_field -> float array -> float Fem.fem_field -> float Fem.fem_field))
    field_m =
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #1 intensive_params_names=%s%!" (String.concat "," (Array.to_list intensive_params_names))) in
  let pname s = Printf.sprintf "%s-%s" petsc_name s in
  let () = ensure_field_is_unrestricted field_m in
  let FEM_field (mwe_m,_, data_m) = field_m in
  let fun_norm_m = fun_tensor_norms mwe_m in
  let len_prefix_m = String.length prefix_dofs_m in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #2%!") in
  let m_dof_names = 
    array_mapfilter
      (fun (name,ix) ->
	 if string_compare_n len_prefix_m prefix_dofs_m name
	 then Some (String.sub name len_prefix_m ((String.length name)-len_prefix_m))
	 else None)
      mwe_m.mwe_subfields in
  let dm_dt_renamings =
    Array.map
      (fun n -> (Printf.sprintf "%s%s" prefix_dofs_m n,Printf.sprintf "dm_dt_%s" n)) m_dof_names
  in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #3%!") in
  let mwe_dm_dt = mwe_sibling "dm_dt" "dm_dt_from_m_" dm_dt_renamings mwe_m in
  (* Create all what is needed to compute the spin-transfer-torque
   * contribution to the LLG equation
   *)
  let (opt_mwe_grad_m, opt_mwe_J, opt_field_grad_m, opt_field_J, opt_fun_grad_m) =
    (* In this scope level mwe, dop_str, dop, ... all refer to grad_m *)
    match field_J_grad_m with
      | None -> (None, None, None, None, None)
      | Some ((FEM_field(mwe_J, _, _) as field_J),
              (FEM_field(mwe, _, _) as field)) ->
        (* Create the string for the operator which calculates the grad m *)
        let dim = mwe.mwe_mesh.mm_dim in
        let dop_str =
          let summands =
            String.concat " + "
              (Array.to_list
                (Array.map
                    (fun m_dof_name ->
                      Printf.sprintf "<grad_m_%s(i, j)||d/dxj %s%s(i)>"
                        m_dof_name prefix_dofs_m m_dof_name)
                    m_dof_names))
          in
            if summands = ""
            then ""
            else
              Printf.sprintf "%s, i:3, j:%d" summands dim
        in
        let () = Printf.printf "grad_m: %s\n%!" dop_str in
        (* Create the diff.op. from the string and the prematrix *)
        let dop = diffop_from_string dop_str in
	let pmx = diffop_prematrix_compatibility_newmethod dop mwe mwe_m in
        let app = prematrix_to_applicator_ff pmx
        in
          (* Return the mwe and the applicator for grad_m and J *)
          (Some mwe, Some mwe_J, Some field, Some field_J, Some app)
  in
  let FEM_field (mwe_h_total,_,_) as field_h_total =
    fun_compute_H_total
      ?target:None
      (Array.make (Array.length intensive_params_names) 0.0)
      field_m
  in (* We start out by computing H once... *)
  let len_h_buffer=10 in
  let h_buffer =
    Array.init len_h_buffer
      (fun _ -> (0.0,copy_field field_h_total))
  in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #4%!") in
    (* The buffer of recently computed H-fields.
       Entries are (time,field). Note that we
       use a simple array and shift elements inside
       that array whenever we compute a new H-field,
       rather than using a circular update pointer.
       This simplifies coding and the extra computational
       effort is quite negligible for short queues like in
       our case.
    *)
  let r_intensive_parameters = ref [||] in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #5%!") in
  let compute_new_h time ((FEM_field (_,_,data_m_in)) as field_m) =
    let () = ensure_field_is_unrestricted field_m in
    let (_,h_to_be_replaced) = h_buffer.(len_h_buffer-1) in
    (* let () = Printf.printf "*** Before compute_H_total ***\n%s\n" (gc_info false) in *)
    let h_new = fun_compute_H_total ~target:h_to_be_replaced (!r_intensive_parameters) field_m in
    (* let () = Printf.printf "*** After compute_H_total ***\n%s\n\n\n" (gc_info false) in *)
      begin
	for i=0 to len_h_buffer-1-1 do
	  h_buffer.(i+1) <- h_buffer.(i)
	done;
	h_buffer.(0) <- (time,h_new);
	h_new
      end
  in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #6%!") in
  let get_h time =
    let rec walk best_time best_h ix =
      if ix = len_h_buffer
      then best_h
      else
	let (time_here,h_here) = h_buffer.(ix) in
	  if abs_float(time-.time_here) < abs_float(time-.best_time)
	  then
	    walk time_here h_here (1+ix)
	  else
	    walk best_time best_h (1+ix)
    in let (bt,bh)=h_buffer.(0) in walk bt bh 1
  in
  let v_cofields_none = [||] in
  let fun_rhs =
    let mwes = array_mapfilter
                 (fun item -> item)
                 [|Some mwe_dm_dt; Some mwe_m; Some mwe_h_total;
                   opt_mwe_grad_m; opt_mwe_J|]
    in
    let llg_fun = make_fun_to_compute_llg_rhs llg_materials mwes
    in 
      fun ~time ~y ~ydot ->
	(* let () = Printf.printf "*** Start fun_rhs ***\n%s\n" (gc_info false) in *)
	(* This is, admittedly, a bit insane: cvode gives us a NVector,
	   which we interface as an OCaml bigarray. However, the
	   right-hand-side function is just the Landau Lifshitz equation,
	   and we want to use our site_wise_execute function on that, so...
	   
	   ... we end up with a wrapping:
	   
	   petsc vector -> nvector -> bigarray -> petsc vector
	   
	   Not that this would be a problem. It is just a little bit silly.

	   (Actually, not that much, because later on, inspection has shown that
	   the data in the nvector are not located in the same place as
	   the original petsc data... Anyway, have to review that...)

	   Plus, this may complicate things when we consider parallelization.
	*)
	let () =
	  funcall_printing_cpu_instructions ~channel:stdout "DDD CPU fun_rhs total"
	    (fun () ->
	       let vec_m = y
	       and vec_dmdt = ydot in
	       let h_total = compute_new_h time (FEM_field (mwe_m,None,vec_m))
		 (* Note: for the R-H-S, we cannot use "get_h time", but have to bite the bullet
		    and do the computation. We can try to recycle H for the Jacobian though... *)
	       in
               let _ =
		 match (opt_fun_grad_m, opt_field_grad_m) with
                   | (Some fun_grad_m, Some field_grad_m) ->
                       let _ = fun_grad_m ~target:field_grad_m field_m in ()
                   | _ -> ()
               in
		 (* 		  let _ = app_grad_m *)
               let fields_for_llg_fun =
		 array_mapfilter
                   (fun item -> item)
                   [|Some (FEM_field (mwe_dm_dt, None, vec_dmdt));
                     Some (FEM_field (mwe_m, None, vec_m));
                     Some h_total;
                     opt_field_grad_m;
                     opt_field_J|]
               in
	       let _ =
		 llg_fun
		   [||]
		   fields_for_llg_fun
		   v_cofields_none
	       in
		 (* let () = Printf.printf "DDD dm/dt=%s\n" (float_array_to_string (Mpi_petsc.vector_extract vec_dmdt)) in *)
		 ()) ()
	in 0
  in
  let fun_jacobi =
    let plan =
      llg_jacobi_plan
	~prefix_dofs_m:"m_"
	~prefix_dofs_h:"H_total_"
	prematrix_h_exch
	mwe_m mwe_h_total
	llg_materials
    in
      fun ~jacobian ~time ~y ~ydot ->
	let () = ()
	  (* let (FEM_field (_,_,vec_h_total)) as field_h = get_h time *)
	  (* THE COMMENT BELOW PRESUMABLY IS WRONG!
	     
	     The problem is that fun_jacobi needs a H_total field corresponding to M,
	     but cannot really afford to to the lengthy computation. Therefore, we
	     re-use one of the H-fields which we computed recently. Our heuristics is to
	     just take the one which was computed for the time closest to the present time.
	     
	     This strategy should be reasonably robust even if time integration should decide
	     to go back and forth in time with its computations...
	  *)
	in
	let () =
	  funcall_printing_cpu_instructions ~channel:stdout "DDD CPU fun_jacobi total"
	    (fun () ->
	       let vec_m = y in
	       let (FEM_field (_,_,vec_h_total)) as field_h = compute_new_h time (FEM_field(mwe_m,None,vec_m)) in
	       let () = jacobi_plan_executor plan jacobian vec_m vec_h_total
	       in
		 ()) ()
	in
	  ()
  in
  let v_initial =
    let (FEM_field (_,_,v)) = field_m in v
  in
  let (rel_tolerance,abs_tolerance,max_order,krylov_max)=tuning_params in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #7%!") in
  let the_cvode = 
    Sundials.make_cvode_petscseq
      ~initial_time:0.0
      ~rel_tolerance
      ~abs_tolerance
      ~max_order
      ~max_num_steps:1000000000 (* 1e9 *)
      ~debug:false
      ~dense:false
      ~jacobi_prealloc:50 (* XXX DDD XXX DDD *)
      ~krylov_max
      ~precond_type:Sundials.PREC_LEFT
      (* ~same_nonzero_pattern:true XXX This may cause failure! Need Bugfix! *)
      ~nonzero_pattern_constant:true
      ~ddd_store_timings:_ddd_ref_timings
      ~fun_rhs ~fun_jacobi ~v_initial
      ()
  in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #8%!") in
  let v_sol = Mpi_petsc.vector_create (Array.length mwe_m.mwe_dofs) (pname "m_timestep_end") in
  let v_begin = Mpi_petsc.vector_create (Array.length mwe_m.mwe_dofs) (pname "m_timestep_start") in
  let v_delta = Mpi_petsc.vector_create (Array.length mwe_m.mwe_dofs) (pname "m_timestep_delta") in
  let () = Mpi_petsc.vector_assemble v_sol in
  let () = Mpi_petsc.vector_assemble v_begin in
  let () = Mpi_petsc.vector_assemble v_delta in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #9%!") in
  let field_begin = FEM_field(mwe_m,None,v_begin) in
  let field_delta = FEM_field(mwe_m,None,v_delta) in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #10%!") in
  let fun_advance_time intensive_parameters target_time nr_single_steps =
    let () = r_intensive_parameters := intensive_parameters in
      funcall_printing_cpu_instructions ~channel:stdout "DDD CPU cvode_advance total"
	(fun () ->
	   let () = Mpi_petsc.vector_copy v_initial v_begin in
	   let time_next = 
	     Sundials.cvode_advance the_cvode target_time v_sol nr_single_steps in
	   let () = Mpi_petsc.vector_copy v_sol data_m in
	     (* Ensure field_m gets the same values! *)
	   let () = Mpi_petsc.vector_copy v_sol v_delta in
	   let () = Mpi_petsc.vector_AXPBY (-1.0) 1.0 v_begin v_delta in
	   let dist_norm = fun_norm_m field_delta in
	   let nr_steps = Sundials.cvode_get_num_steps the_cvode in
	   let result = (time_next, nr_steps, dist_norm, field_begin, field_m) in
	   let () = Printf.printf "DDD sundials timings: %s\n%!" (float_array_to_string (!_ddd_ref_timings)) in
	     result) ()
  in
  let () = logdebug (Printf.sprintf "DDD mumag_cvode_time_integrator #11%!") in
    (the_cvode,fun_advance_time)
;;


(* === We only interface part of the following functions. The idea is
   to take an approximation order and a function defined on the sphere
   and map this to C code that computes the derivative, as used
   for H_anisotropy.
   === *)

let independent_so3_scalars order =
  let rec walk this_order have =
    if this_order > order then Array.concat have
    else 
      let all_contribs = all_distributions_to_buckets 3 this_order in
      let reduced_contribs =
	array_filter (fun v -> v.(2)<2) all_contribs
	  (* Not all the tensorial quantities are independent:
	     we also have to take into account the condition
	     M_x^2+M_y^2+M_z^2 = constant. So, whenever we encounter
	     a contribution carrying a factor of M_z^2, we may
	     regard this as dependent, as it can be re-written in terms
	     containing M_x^2 and M_y^2 instead. Hence, we remove
	     all such terms.
	   *)
      in
      walk (2+this_order) (reduced_contribs::have)
  in
  walk 0 []
;;

let vivify_multinomial_coefficient_vector v_powers =
  let n = Array.length v_powers in
  let dim = Array.length v_powers.(0) in
  let multinom powers v =
    let rec walk n have =
      if n=dim then have
      else walk (1+n) (have*.(int_power powers.(n) v.(n)))
    in
    walk 0 1.0
  in
  fun coeffs v ->
    if Array.length coeffs <> n then failwith "Bad coefficient vector!"
    else
      let rec walk j have =
	if j = n then have
	else
	  walk (1+j) (have+.coeffs.(j)*.(multinom v_powers.(j) v))
      in walk 0 0.0
;;

let rec random_point_on_d_sphere ?(fun_rng  = fun x -> Random.float x) dim =
  let p = Array.init dim (fun _ -> gauss_random ~fun_rng 0.0 1.0) in
  let len = sqrt(euclidean_len_sq p) in
    if len < 1e-8
    then random_point_on_d_sphere ~fun_rng dim
      (* redo if we had really exceptionally bad luck.
	 Not that this would ever happen... *)
    else
      let factor = 1.0/.len in
	begin
	  for i=0 to dim-1 do
	    p.(i) <- factor*.p.(i);
	  done;
	  p
	end
;;

let reasonably_independent_points_on_sphere_for_determination_of_tensor_coefficients
    ?(fun_rng  = fun x -> Random.float x)
    ?(iterations=10)
    terms =
  let nr_terms = Array.length terms in
  let dim = Array.length terms.(0) in
  let point_contribs point =
    Array.init nr_terms
      (fun n ->
	 vivify_multinomial_coefficient_vector
	   (Array.sub terms n 1) [|1.0|] point)
  in
  let weakest_point points_and_term_values =
    let nr_points = Array.length points_and_term_values in
    let lengths = Array.map (fun (_,x) -> sqrt(euclidean_len_sq x)) points_and_term_values in
    let strengths = 
      Array.mapi
	(fun n_here (pt_here,tv_here) -> 
	   let rel_strengths = 
	     Array.mapi
	       (fun m (pt,tv) ->
		  if m=n_here then infinity
		  else 
		    let cos_angle =
		      let x =
			(scalar_product tv_here tv)/.(lengths.(m)*.lengths.(n_here))
		      in if classify_float x = FP_normal then x else 1.0
		    in
		      1.0-.(abs_float cos_angle))
	       points_and_term_values
	   in
	   let (min_strength,_,min_index) =
	     Array.fold_left
	       (fun (sf_s,n,sf_ix) x ->
		  if x < sf_s then (x,n+1,n) else (sf_s,n+1,sf_ix))
	       (infinity,0,0) rel_strengths
	   in min_strength)
	points_and_term_values
    in
    let (min_strength,_,min_index) =
      Array.fold_left
	(fun (sf_s,n,sf_ix) x ->
	   if x < sf_s then (x,n+1,n) else (sf_s,n+1,sf_ix))
	(infinity,0,0) strengths
    in min_index
  in
  let weed_out_weakest_point a =
    let ix = weakest_point a in
      array_one_shorter a ix
  in
  let getpoint () =
    let p = random_point_on_d_sphere ~fun_rng dim in
      (p,point_contribs p)
  in
  let rec iterate_weed_out n have =
      if n = iterations 
      then have 
      else
	let extra_point = getpoint() in
	let have_extended =
	  Array.append [|extra_point|] have
	in iterate_weed_out (1+n) (weed_out_weakest_point have_extended)
  in
    iterate_weed_out 0 (Array.init nr_terms (fun _ -> getpoint()))
;;
(* Tests show this would easily allow us to go up to order 14 or so... *)

let taylor_coefficients_of_spherical_function
    ?(fun_rng  = fun x -> Random.float x)
    order f =
  let terms = independent_so3_scalars order in
  let det_inv = det_and_inv (Array.length terms) in
  let v_points_and_values = reasonably_independent_points_on_sphere_for_determination_of_tensor_coefficients terms in
  let (_,mx) = det_inv (Array.map (fun (_,v) -> v) v_points_and_values) in
  let f_values = Array.map (fun (p,_) -> f p) v_points_and_values in
  let all_coeffs = array_pointwise (fun x y -> (x,y)) (mx_x_vec mx f_values) terms in
    array_filter (fun (c,_) -> abs_float c > 1e-10) all_coeffs
;;

let spherical_function_and_derivative_ccode_from_coeffs
    ?(scalar_name="e")
    ?(deriv_name="h")
    ?(field_name="mag")
    ?(derivative_coeff=1.0) (* use -1.0 if the negative derivative of the energy is being needed... *)
    coeffs_and_powers =
  let nr_coeffs = Array.length coeffs_and_powers in
    if nr_coeffs = 0 then ("{/* no anisotropy energy */}","{/* no anisotropy field */}")
    else
      let format_term powers coeff =
	let expanded_powers =
	  array_join (Array.mapi (fun n p -> Array.make p n) powers)
	in
	let formatted_powers =
	  String.concat "*"
	    (Array.to_list
	       (Array.map
		  (fun n -> Printf.sprintf "%s(%d)" field_name n) expanded_powers))
	in Printf.sprintf "(%12.8f)*%s" coeff (if formatted_powers="" then "1.0" else formatted_powers)
      in
      let (_,powers0) = coeffs_and_powers.(0) in
      let dim = Array.length powers0 in
      let ccode_spherical_fun =
	Printf.sprintf "    %s=%s;" scalar_name
	  (String.concat "+"
	     ("0.0"::(Array.to_list
			(Array.map (fun (c,p) -> format_term p c) coeffs_and_powers))))
      in
      let ccode_derivative = Array.init dim
	(fun dir ->
	   let contribs = Hashtbl.create 10 in
	   let () =
	     Array.iter 
	       (fun (coeff,powers) ->
		  if powers.(dir)=0 then ()
		  else
		    let new_powers =
		      Array.init dim
			(fun n -> if n = dir then powers.(n)-1 else powers.(n))
		    in
		    let new_coeff = derivative_coeff*.coeff*.(float_of_int powers.(dir)) in
		      hashtbl_increase (+.) contribs new_powers new_coeff)
	       coeffs_and_powers
	   in
	     Printf.sprintf "    %s(%d)=%s;" deriv_name dir
	       (String.concat "+"
		  ("0.0"::(Array.to_list
			     (map_hashtbl_to_array format_term contribs)))))
      in 
      (ccode_spherical_fun,
       String.concat "\n" (Array.to_list ccode_derivative))
;;

let anisotropy_ccode_from_function material_name order f =
  let tcoeffs = 
    taylor_coefficients_of_spherical_function order f
  in
  let (ccode_e,ccode_h) =
    spherical_function_and_derivative_ccode_from_coeffs
      ~scalar_name:(Printf.sprintf "E_anis_%s" material_name)
      ~deriv_name:(Printf.sprintf "H_anis_%s" material_name)
      ~field_name:(Printf.sprintf "m_%s" material_name)
      ~derivative_coeff:(-1.0)
      tcoeffs
  in
  (Printf.sprintf
     "if(have_m_%s) {
%s
 }
" material_name ccode_e,
Printf.sprintf
     "if(have_m_%s) {
    double sprod;int i;
%s
    /* Ensure H_anis _|_ m */
    for(sprod=0.0,i=0;i<3;i++)sprod+=m_%s(i)*H_anis_%s(i);
    for(i=0;i<3;i++)H_anis_%s(i)-=sprod*m_%s(i);

 }
" material_name ccode_h material_name material_name material_name material_name)
;;

let make_anisotropy_funs mats mwe_m mwe_E_anis mwe_H_anis =
  let v_ccode_e_h =
    Array.map
      (fun mat ->
	match mat.llg_anisotropy with
	| None -> ("","")
	| Some f_anis ->
	    anisotropy_ccode_from_function
	      mat.llg_name mat.llg_anisotropy_order f_anis
      ) mats
  in
  let ccode_e = String.concat "\n" (Array.to_list (Array.map (fun (x,_) -> x) v_ccode_e_h))
  and ccode_h = String.concat "\n" (Array.to_list (Array.map (fun (_,x) -> x) v_ccode_e_h))
  in
  let () = loginfo (Printf.sprintf "DDD *** ANISOTROPY ***\n*** ccode_e:\n%s\n\n*** ccode_h:\n%s" ccode_e ccode_h) in
  let vivify_code mwes code =
    let f_core = site_wise_execute mwes [||] [||] code
    in
    fun ?target field_m ->
      let the_target = 
	match target with
	| None -> make_field mwes.(1)
	| Some field_target -> field_target
      in
      let _ = f_core [||] [|field_m;the_target|] [||]
      in the_target
  in
  (vivify_code [|mwe_m;mwe_E_anis|] ccode_e,
   vivify_code [|mwe_m;mwe_H_anis|] ccode_h)
;;


(* Create all the relevant MWEs for a micromagnetics simulation
   NOTE: mwe_H_demag will be 1st order no matter what other orders
   we ask for.

   NOTE: we also provide functions which compute the derived quantities
   from m, wrapped up as a very special kind of non-python-callable pills.

   Rationale: these will in the end be passed on to compute_H_total()
   to do the partial sub-problems, and by avoiding the
   ocaml_callable -> py_callable -> ocaml_callable packaging, we will be
   in a much better strategic position when we think about parallelization.

   ...on the other hand, maybe we should not give python a handle at
   the underlying mumag functions to compute H at all. Or maybe
   postpone this... It is just implementation effort we could do
   without, after all. Plus, it is still possible in principle to
   implement all that functionality in Python.

   So... two functions; one computing all the H as well as H_total,
   one computing all the energy densities...
*)


(* XXX actually, this little idea evolved into the brain of the whole
   thingy...
   
   Note: this version has been obsoleted, as we are not yet using the
   "sloppy cofield to field mapping" here. It is, however,
   quite essential for the Jacobian.

 *)

let magsim_brain
    ?(petsc_name=gensym "magsim")
    ?lindholm_triangle_contributions
    ?(do_demag=true)
    ?(fem_only=false)
    ?(make_exchange_fun=make_exchange_fun)
    ?(make_demag_fun_3d=make_demag_fun_3d)
    (* ^ evil hack to make this externally parametrizable! *)
    ?(extra_contribs_by_magnetization_name=[||]) (* Array of pairs of (magnetization name,extra_code) *)
    ?(energy_factor=1.0)
    ?(enable_spin_transfer_torque=true)
    ?tuning_params
    mesh
    materials_and_orders_by_region
    intensive_parameter_names
    =
  let () = logdebug (Printf.sprintf "DDD magsim_brain #1 ipn=%s%!" (String.concat "," (Array.to_list intensive_parameter_names))) in
  let all_mats =
    let ht = Hashtbl.create 10 in
    let () =
      Array.iter
	(fun mats_and_orders ->
	   Array.iter
	     (fun (mat,_) -> Hashtbl.replace ht mat.llg_name mat)
	     mats_and_orders) materials_and_orders_by_region
    in
      map_hashtbl_to_array
	~sorter:(fun a b -> compare a.llg_name b.llg_name)
	(fun k v -> v)
	ht
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #2%!") in
  let vec_elems_by_region =
    Array.map
      (fun mats_and_orders ->
	 let atomic_elements =
	   Array.map
	     (fun (mat,ord) ->
		make_element
(* 		  ~element_name:(Printf.sprintf "elem_m_%s" mat.llg_name) *)
		  mesh.mm_dim
		  ((Printf.sprintf "m_%s" mat.llg_name),[|3|]) ord)
	     mats_and_orders
	 in
	   Array.fold_left fuse_elements empty_element atomic_elements)
      materials_and_orders_by_region
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #3%!") in
  let scalar_elems_by_region =
    Array.map
      (fun mats_and_orders ->
	 let atomic_elements =
	   Array.map
	     (fun (mat,ord) ->
		make_element
		  ~element_name:(Printf.sprintf "elem_e_%s" mat.llg_name)
		  mesh.mm_dim
		  ((Printf.sprintf "E_%s" mat.llg_name),[||]) ord)
	     mats_and_orders
	 in
	   Array.fold_left fuse_elements empty_element atomic_elements)
      materials_and_orders_by_region
  in
  let mwes_and_fields_by_name = Hashtbl.create 10 in
  let get_mwe_field name =
    Hashtbl.find mwes_and_fields_by_name name
  in
  let get_mwe name =
    let (mwe,_) = Hashtbl.find mwes_and_fields_by_name name
    in mwe
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #4%!") in
  let get_field name =
    let (_,field) = Hashtbl.find mwes_and_fields_by_name name
    in field
  in
  let fun_make_and_register_mwe_field mwe =
    let () = logdebug (Printf.sprintf "DDD fun_make_and_register_mwe_field mwe=%s%!" mwe.mwe_name) in (* DDD *)
    let (FEM_field (mwe,_,v_data)) as field = 
      make_field ~constant_value:0.0 mwe
    in
    let () =
      Hashtbl.add mwes_and_fields_by_name
	mwe.mwe_name (mwe,field)
    in (mwe,field)
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #5%!") in
  let (mwe_m,_) =
    fun_make_and_register_mwe_field 
      (make_mwe "mwe_m"
	 (fun sx -> let Mesh.Body_Nr r = sx.ms_in_body in 
	 vec_elems_by_region.(r))
	 mesh)
  in
  let (mwe_e,_) =
    fun_make_and_register_mwe_field 
      (make_mwe "mwe_e"
	 (fun sx -> let Mesh.Body_Nr r = sx.ms_in_body in 
	 scalar_elems_by_region.(r))
	 mesh)
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #6%!") in
  let mwe_m_sibling stem =
    let (mwe,_) =
      fun_make_and_register_mwe_field
	(mwe_sibling (Printf.sprintf "mwe_%s" stem) (Printf.sprintf "[%s/m]_" stem)
	   (Array.map
	      (fun mat -> (Printf.sprintf "m_%s" mat.llg_name,Printf.sprintf "%s_%s" stem mat.llg_name))
	      all_mats)
	   mwe_m)
    in mwe
  in
  let mwe_e_sibling stem =
    let (mwe,_) =
      fun_make_and_register_mwe_field
	(mwe_sibling (Printf.sprintf "mwe_%s" stem) (Printf.sprintf "[%s/m]_" stem)
	   (Array.map
	      (fun mat -> (Printf.sprintf "E_%s" mat.llg_name,Printf.sprintf "%s_%s" stem mat.llg_name))
	      all_mats)
	   mwe_e)
    in mwe
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #7%!") in
  let further_m_type_mwes_and_names =
    Array.map (fun (f,n) -> (n,f n))
      [|(mwe_m_sibling,"M");
	(mwe_m_sibling,"H_exch");
	(mwe_m_sibling,"H_anis");
	(mwe_m_sibling,"H_total");
	(mwe_e_sibling,"E_exch");
	(mwe_e_sibling,"E_anis");
	(mwe_e_sibling,"E_demag");
	(mwe_e_sibling,"E_ext");
      |]
      (* Should we also introduce "H_localexch" for same-site magnetism interaction, e.g.
	 Dy/Fe - or should we rather absorb this into H_exch? Presumably, we have to treat
	 it as separate fields, or we will get into trouble with computing the energy
	 properly...
      *)
  in
    (* besides these, we need H_demag and E_demag, which is of a different
       kind... Moreover if spin-transfer-torque is enabled we need
       the spin-polarized current density vector J
     *)
  let () = logdebug (Printf.sprintf "DDD magsim_brain #7.2%!") in
  let mwe_H = 
    let elem_H =
      make_element
        ~element_name:"elem_H"
        mesh.mm_dim ("H",[|3|])
        1 (* As the FEM/BEM method can only handle 1st order for now,
              we just hard-code this... *)
    in
      make_mwe "mwe_H" (fun sx -> elem_H) mesh
  in
  let field_J_grad_m =
    (*  Create the tensor where to put grad_m for stt computation *)
    if enable_spin_transfer_torque = false
    then
      None
    else
      let matrix_elems_by_region =
        Array.map
          (fun mats_and_orders ->
            let atomic_elements =
              Array.map
                (fun (mat,ord) ->
                    make_element
                      mesh.mm_dim
                      ((Printf.sprintf "grad_m_%s" mat.llg_name),[|3;3|]) ord)
                mats_and_orders
            in
              Array.fold_left fuse_elements empty_element atomic_elements)
          materials_and_orders_by_region
      in
      let (_, field_grad_m) =
        fun_make_and_register_mwe_field
          (make_mwe "mwe_grad_m"
            (fun sx -> let Mesh.Body_Nr r = sx.ms_in_body in
              matrix_elems_by_region.(r))
            mesh)
      in
      let (_, field_J) =
        fun_make_and_register_mwe_field
          (mwe_sibling "mwe_J" "[J/H]" [|("H", "J")|] mwe_H)
      in
        Some (field_J, field_grad_m)
  in
  let (mwe_H_demag, mwe_H_ext) =
      (
	let (mwe,_) =
	  fun_make_and_register_mwe_field
	    (mwe_sibling "mwe_H_demag" "[H_demag/H]" [|("H","H_demag")|] mwe_H)
	in mwe,
	let (mwe,_) =
	  fun_make_and_register_mwe_field
	    (mwe_sibling "mwe_H_ext" "[H_ext/H]" [|("H","H_ext")|] mwe_H)
	in mwe
      )
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #7.3%!") in
  let (prematrix_H_exch,fun_H_exch) = 
    (* We will always do H_exch - there simply will not be any reasonable
       simulation where we want to turn this off - even if, we should just
       set all the J_exch to zero, which has the same effect.
    *)
    make_exchange_fun all_mats (get_mwe "mwe_H_exch") (get_mwe "mwe_m")
  in
    (* DDD print H_exch so that we can do external checks. We may have some trouble here...
  let fun_H_exch ?target field_m = (* DDD *)
    let h_exch = fun_H_exch ?target field_m in
    let () = Printf.printf "=== H_exch ===\n" in
    let () = debugprint_field h_exch in
      h_exch
  in
    *)
  let () = logdebug (Printf.sprintf "DDD magsim_brain #8%!") in
  let (fun_E_anis,fun_H_anis) =
    make_anisotropy_funs all_mats
      (get_mwe "mwe_m") (get_mwe "mwe_E_anis") (get_mwe "mwe_H_anis")
  in
  let fun_H_demag =
    let field_H_demag0 = get_field "mwe_H_demag" in
      if not do_demag then
	fun ?compute_field_rho ?debug_charge_imbalance ?target field ->
	  let () = ensure_field_is_unrestricted field in
	  match target with
	    | None -> field_H_demag0
	    | Some ((FEM_field (_,_,x)) as field) ->
		let () = ensure_field_is_unrestricted field in
		let () = Mpi_petsc.vector_scale x 0.0 in
		  field
      else
	make_demag_fun_3d
	  ?lindholm_triangle_contributions
	  ~fun_make_and_register_mwe_field
	  ~diffop_div_m_string:(demag_fun_diffop_div_m_string "rho" "m_" all_mats)
	  ~inside_regions:(Array.to_list
			     (array_mapfilteri
				(fun ix_old mats_orders ->
				   if false && (Array.length mats_orders = 0) (* XXX JUST FOR NOW, EVERY REGION IS AN INSIDE REGION! *)
				   then None
				   else Some (Body_Nr ix_old))
				materials_and_orders_by_region))
	  ~fem_only
	  all_mats
	  (get_mwe "mwe_H_demag") (get_mwe "mwe_m")
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #9%!") in
  let fun_compute_H_total =
    let field_H_demag = get_field "mwe_H_demag"
    and field_H_exch = get_field "mwe_H_exch"
    and field_H_anis = get_field "mwe_H_anis"
    and field_H_total = get_field "mwe_H_total"
    and field_H_ext = get_field "mwe_H_ext"
    and field_m = get_field "mwe_m"
    in
    let v_fields = [|field_H_total;field_H_demag;field_H_exch;field_H_anis;field_H_ext;field_m|] in
    let v_cofields = [||] in
    let fun_combine =
      make_fun_to_compute_H_total
        ~intensive_parameter_names
        ~extra_contribs_by_magnetization_name
	(Array.map (fun mat -> mat.llg_name) all_mats)
	(Array.map get_mwe [|"mwe_H_total";"mwe_H_demag";"mwe_H_exch";"mwe_H_anis";"mwe_H_ext";"mwe_m"|])
    in
    let () = logdebug (Printf.sprintf "DDD magsim_brain #10%!") in
      fun 
	?(compute_field_rho=false)
	?(debug_charge_imbalance=false)
	?target
	v_intensive_params field_m_input ->
(* 	  let () = logdebug (Printf.sprintf "DDD compute_H_total #1\n%!") in *)
	let _ = copy_field_into field_m_input field_m in 
	(* NOTE: might want to mini-fork-parallelize this...? *)
(* 	  let () = logdebug (Printf.sprintf "DDD compute_H_total #2\n%!") in *)
	let _ =
	  fun_H_demag ~compute_field_rho ~debug_charge_imbalance
	    ~target:field_H_demag field_m
	in
(* 	let () = logdebug (Printf.sprintf "DDD compute_H_total #3\n%!") in *)
	let _ = fun_H_exch ~target:field_H_exch field_m in
	let _ = fun_H_anis ~target:field_H_anis field_m in
(* 	  let () = logdebug (Printf.sprintf "DDD compute_H_total #4\n%!") in *)
	let _ = fun_combine v_intensive_params v_fields v_cofields
	in 
	  match target with
	    | None -> field_H_total
	    | Some x ->
		copy_field_into field_H_total x
  in
  let fun_update_energies_and_M =
    let (mwe_M,field_M) =
      get_mwe_field "mwe_M"
    and (mwe_E_demag,field_E_demag) =
      get_mwe_field "mwe_E_demag"
    and field_H_demag = get_field "mwe_H_demag"
    and (mwe_E_exch,field_E_exch) =
      get_mwe_field "mwe_E_exch"
    and (mwe_H_exch,field_H_exch) = get_mwe_field "mwe_H_exch"
    and field_E_anis = get_field "mwe_E_anis"
    and (mwe_E_ext,field_E_ext) = get_mwe_field "mwe_E_ext"
    and (mwe_H_ext,field_H_ext) = get_mwe_field "mwe_H_ext"
    and (mwe_m,field_m) = get_mwe_field "mwe_m"
    in
    let material_pieces f_mat =
      String.concat "\n"
	(Array.to_list
	   (Array.map f_mat all_mats))
    in
    let ccode_M =
      material_pieces
	(fun mat ->
	   Printf.sprintf "if(have_m_%s) {int i; for(i=0;i<3;i++) {M_%s(i) = (%f)*m_%s(i);} }\n"
	     mat.llg_name mat.llg_name mat.llg_Ms mat.llg_name)
    in
    let () = Printf.printf "*** DDD ccode_M ***\n%s\n******************\n%!" ccode_M in
    let ccode_E_ext =
      material_pieces
	(fun mat ->
	   Printf.sprintf 
"
 if(have_m_%s) {
   E_ext_%s  = -(%f)*(H_ext(0)*m_%s(0) + H_ext(1)*m_%s(1) + H_ext(2)*m_%s(2));
}

"
	     mat.llg_name
	     mat.llg_name
	     (mat.llg_Ms*.energy_factor)
	     mat.llg_name
	     mat.llg_name
	     mat.llg_name
	)
	
    in
    let fun_update_M_core =
      site_wise_execute [|mwe_M;mwe_m|] [||] [||] ccode_M
    in
    let fun_E_ext_core =
      site_wise_execute
	[|mwe_E_ext;mwe_m;mwe_H_ext|]
	[||]
	intensive_parameter_names
	ccode_E_ext
    in
    let ccode_E_demag_exch =
      material_pieces
	(fun mat ->
	   Printf.sprintf
"
 if(have_m_%s) {
   int i;

   E_exch_%s=0.0;
   E_demag_%s=0.0;

   for(i=0;i<3;i++)
     {
      E_exch_%s += -(%f)*H_exch_%s(i)*m_%s(i);
      E_demag_%s += -(%f)*H_demag(i)*m_%s(i);
     }
}
"
	       mat.llg_name
	       mat.llg_name
	       mat.llg_name
	       mat.llg_name
	       (mat.llg_Ms*.energy_factor)
	       mat.llg_name
	       mat.llg_name
	       mat.llg_name
	       (mat.llg_Ms*.energy_factor)
	       mat.llg_name)
    in
    let fun_update_M () =
      let () = logdebug (Printf.sprintf "DDD fun_update_M called!%!") in
      let _ =
	fun_update_M_core [||] [|field_M;field_m|] [||]
      in ()
    in
    let fun_E_demag_exch_core =
      site_wise_execute
	[|mwe_E_demag;mwe_E_exch;
	  mwe_H_demag;mwe_H_exch;mwe_m|]
	[||]
	[||]
	ccode_E_demag_exch
    in
    let fun_update_E_ext v_intensive_params =
      let _ =
	fun_E_ext_core
	  v_intensive_params
	  [|field_E_ext;field_m;field_H_ext|]
	  [||]
      in ()
    in
    let fun_update_E_anis () =
      let _ = fun_E_anis ~target:field_E_anis field_m
      in ()
    in
    let fun_update_E_demag_exch () =
      let _ =
	fun_E_demag_exch_core
	  [||]
	  [|field_E_demag;field_E_exch;
	    field_H_demag;field_H_exch;field_m|]
	  [||] 
      in ()
    in
      fun v_intensive_params ->
	begin
	  logdebug (Printf.sprintf "DDD update_energies_and_M!%!");
	  fun_update_M ();
	  logdebug (Printf.sprintf "DDD updated_M!%!");
	  fun_update_E_ext v_intensive_params;
	  fun_update_E_anis ();
	  fun_update_E_demag_exch ();
	end
  in
  let create_timestepper sampling_fun_m sampling_fun_H_ext =
    let () = logdebug (Printf.sprintf "DDD create_timestepper #1%!") in
    let mwe_H_ext = get_mwe "mwe_H_ext" in
    let sampled_initial_field_m = sample_field_type2 mwe_m sampling_fun_m in
    let sampled_initial_field_H_ext = sample_field_type2 mwe_H_ext sampling_fun_H_ext in
    let field_m = get_field "mwe_m" in
    let field_H_ext = get_field "mwe_H_ext" in
    let field_m = copy_field_into sampled_initial_field_m field_m in
    let field_H_ext = copy_field_into sampled_initial_field_H_ext field_H_ext in
    let reduced_fun_compute_H_total ?target intensive_params field =
      fun_compute_H_total ?target intensive_params field
    in
    let () = logdebug (Printf.sprintf "DDD create_timestepper #2%!") in
      mumag_cvode_time_integrator
        ?tuning_params
        ?field_J_grad_m
        all_mats prematrix_H_exch intensive_parameter_names
        reduced_fun_compute_H_total field_m
  in
  let () = logdebug (Printf.sprintf "DDD magsim_brain #11%!") in
  (mwes_and_fields_by_name,
   fun_compute_H_total,
   fun_update_energies_and_M,
   create_timestepper)
;;
(* That should do it for now...
   XXX also return functions to compute energies!
   XXX TODO: properly handle intensive parameters!
*)


(* Function to work out the maximum angle between the vector data on
neighbouring sites. This (as far as we know) is only useful
information for magnetisation vectors described by first order basis
functions. *)

let vector_field_max_angle field =
  let () = ensure_field_is_unrestricted field in
  let (FEM_field (mwe,_, data)) = field in 
  let subfields = mwe.mwe_subfields in 
  let dofnames = Array.map fst subfields in
  let nr_dofnames = Array.length dofnames in

  let subfields_components = Array.map snd subfields in
  let subfields_vector_components =   (* array to store the components of the vectors *)
    Array.init nr_dofnames            (* on neighbour sites associated to each subfield *)   
      (fun i -> 
	let len = subfields_components.(i).(0) in 
	[|(Array.make len 0.0);(Array.make len 0.0)|]  
      ) 
  in
  let max_angle_by_dofname =
    Array.init nr_dofnames (fun i -> (dofnames.(i),0.0)) in  (* array to store the field_name+max_angle *)
  let links = mesh_plotinfo_links mwe.mwe_mesh in
  let compute_angle v1 v2 =
    let len_vec1 = sqrt (Snippets.euclidean_len_sq v1) in
    let len_vec2 = sqrt (Snippets.euclidean_len_sq v2) in
    let angle = 
      try 
	acos (min 1.0 ((scalar_product v1 v2)/.(len_vec1 *. len_vec2)))      (* angle in radiants *)
      with 
      | _ -> failwith "fem.ml - vector_field_max_angle - : division by zero"
    in
      angle
  in
  let () =
      Array.iter
	 ( fun (node_a,node_b) ->      (* the angle is computed only between fields living on the mesh nodes *)
	     try (* we get rid of the cases where the current field is not defined in node_a or node_b *)

	       let dofs_on_site_a = Hashtbl.find mwe.mwe_dofs_by_site [|node_a|] in
	       let dofs_on_site_b = Hashtbl.find mwe.mwe_dofs_by_site [|node_b|] in
	       
	       for name_ix = 0 to nr_dofnames-1 do
		 let pos_a = array_position_if (fun dof_a -> dofnames.(name_ix) = fst (the_dof_name mwe dof_a)) dofs_on_site_a 0 in
		 let pos_b = array_position_if (fun dof_b -> dofnames.(name_ix) = fst (the_dof_name mwe dof_b)) dofs_on_site_b 0 in
		 if pos_a > (-1) && pos_b > (-1) 
		 then
		   let v1 = subfields_vector_components.(name_ix).(0) in
		   let v2 = subfields_vector_components.(name_ix).(1) in
		   let vec_len = Array.length v1 in
		   let () = 
		     for comp_ix=0 to vec_len-1 do
		       let dof_a = dofs_on_site_a.(pos_a+comp_ix) in
		       let dof_b = dofs_on_site_b.(pos_b+comp_ix) in
		       Mpi_petsc.with_petsc_vector_as_bigarray data
			 (fun ba_field ->
			   v1.(comp_ix) <- (ba_field.{dof_a.dof_nr});
			   v2.(comp_ix) <- (ba_field.{dof_b.dof_nr})
			 )
		     done
		   in
		   let new_angle = compute_angle v1 v2 in
		   let (field_name,max_angle_sf) = max_angle_by_dofname.(name_ix) in
		   if new_angle <= max_angle_sf 
		   then ()
		   else 
		     max_angle_by_dofname.(name_ix) <- (field_name,new_angle)
	       done
		 
	     with | _  -> ()
	  )
      links
  in max_angle_by_dofname;;

