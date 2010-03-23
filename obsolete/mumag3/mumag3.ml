(* (C) 2007 Dr. Thomas Fischbacher *)

(* XXX NOTE: this throws a Parsing.Parse_Error! *)

(* XXX PART OF MUMAG3 IS TO BECOME THE "NSIM" MODULE! XXX

   XXX DESIGN ISSUE: presumably we do not need .get_field and
   .get_cofield "methods" at all. The .execute_on method
   can handle that as well! We just have to register 
   additional scripts for the machine.

ocamlc -I ../mumag2 -I ../snippets -I ../ccpla -I ../mesh -I ../fem -I ../mpi_petsc -I ../nsim_grammars -I ../nsim -i mumag3.ml

   We need a new data type, la_script, encoding sequences of linear
   algebra operations.

   The la_script for the computation of H_demag using the FEM method
   would then look as follows:

===>
mwes: (just the names of mwes, must all be registered;
       plus: for every mwe, there must be a vector
       distribution scheme!)
 
buffers:
 .data_rho_c: mwe_rho
 .data_phi_f: mwe_phi
matrices:
 mx_div_M: "mwe_rho", "mwe_phi", [(None,None,1.0)] "Ms[mat]*<rho || d_dxj m[mat](j)>, j:3"

ksps:
 ksp_laplace: "-<d/dxj phi || d/dxj rho>", [boundary DOF indices]

mx_x_vector   mx_div_M    data_m_f   .data_rho_c
solve_laplace ksp_laplace .data_rho_c .data_phi_f
mx_x_vector   mx_grad_phi .data_phi_f data_H_demag
<===

For FEM-BEM, this would roughly look as follows:

===>
distribution_schemes: 
 scalar_field: [|40;50;48;42;43|]
 short_scalar_field: [|5;5;6;5;5|]
buffers:
 .data_rho_c: scalar_field
 .data_phi_f1: scalar_field
 .data_phi_f2: scalar_field
 .data_bdy_phi_f1: short_scalar_field


mx_x_vector   mx_div_M    data_m_f   .data_rho_c
solve         ksp_laplace_f1 .data_rho_c .data_phi_f1
vec_pull      pull_boundary .data_phi_f1 .data_bdy_phi_f1
mx_x_vector   mx_bem .data_bdy_phi_f1 .data_bdy_phi_f2
vec_zero      .data_rho_c2
vec_push    push_boundary .data_bdy_phi_f2 .data_rho_c2
solve         ksp_laplace_f2 .data_rho_c2 .data_phi_f2
vec_add       1.0 .data_phi_f1 1.0 .data_phi_f2
mx_x_vector   mx_grad_phi .data_phi_f2 data_H_demag
<===

Actually, this is an over-simplification, as "solving the Laplace
equation with boundary conditions" will be decomposed into multiple
individual steps as well.

Later on, we may think about providing some specialized means
to "box" linalg-scripts, so that we can treat "solving a
laplace equation with special LHS" as a building block of its own.

Right now, I do not really want (for now) to go down the road of
building a full-fledged parallel programming language (because next,
I would perhaps discover I needed some form of "lambda"...).
So, our scripts will just be a little bit longer...

~~~~~~~~~~~~~~~~~~~~

Computing H_demag is pretty complicated. A much more easily achievable
task that should get us on the right track would be to compute H_exch
in parallel. So, we start out with this.

~~~~~~~~~~~~~~~~~~~~

There is one awkward issue: right now, time integration is
not parallelized, so this will take place on the master node only. 

Hence, we have to build the Jacobian on the master node, for which 
we need the H_exch matrix (rhs rescaled with inverse volume factors).

How do we properly hook that information transfer (computation of
the H_exch prematrix) into make_linalg_machine? For now, I think
I will just try an ad-hoc solution and allow a function optarg 
"pass_on_prematrices"...

*)

open Snippets;;
open Mesh;;
open Fem;;
open Ccpla;;
open Nsim;;


(* Map a mwe to a pair of int arrays
   (local_index_by_global_index,global_index_by_local_index)
   which implement mappings between the indices of all MWE
   degrees of freedom and boundary degrees of freedom.
   (local_index_by_global_index will have -1 entries for non-surface dofs)

   The question of what actually constitutes a boundary is being
   resolved via the list of regions to be considered "inside" and the
   dof_name...
*)

let mwe_surface_dof_mappings inside_regions dof_name mwe =
  let (dof_name_stem,_) = dof_name in
  let region_is_inside r = List.mem r inside_regions in
  let boundary_dofs = 
    array_mapfilter
      (fun dof -> 
	 let (this_dof_name_stem,_) = the_dof_name mwe dof in
	   if this_dof_name_stem <> dof_name_stem then None
	   else
	     let inside_regions =
	       List.filter (fun r -> region_is_inside r)
		 dof.dof_in_body
	     in
	     let outside_regions =
	       List.filter (fun r -> not(region_is_inside r))
		 dof.dof_in_body
	     in
	       (* ^ not as slow as it may seem! Short lists! *)
	       if inside_regions <> [] && outside_regions <> []
	       then Some dof
	       else None)
      mwe.mwe_dofs 
  in
  let nr_dofs = Array.length mwe.mwe_dofs
  and nr_bdofs = Array.length boundary_dofs
  in
  let global_index_by_local_index = Array.map (fun dof -> dof.dof_nr) boundary_dofs in
  let local_index_by_global_index = Array.make nr_dofs (-1) in
  let () =
    Array.iteri
      (fun nr_bdof nr_dof -> local_index_by_global_index.(nr_dof) <- nr_bdof)
      global_index_by_local_index
  in
    (local_index_by_global_index,global_index_by_local_index)
;;

let mwe_dof_3d_surface_triangles_and_space_angles_and_coords
    inside_regions mwe (ix_local_by_ix_global,ix_global_by_ix_local)
    =
  let () =
    (if mwe.mwe_mesh.mm_dim <> 3
     then failwith "mwe_dof_3d_surface_triangles: Need 3d mesh to extract surface triangles"
     else ())
  in
  let region_is_inside r = List.mem r inside_regions in
  let solid_angle =
    solid_angle_3d_at_mesh_vertex
      ~fun_is_inside:(fun sx -> region_is_inside sx.ms_in_body)
  in
  let nr_bdofs = Array.length ix_global_by_ix_local in
  let point_coords_by_local_index =
    Array.map
      (fun ix_global -> mwe.mwe_dofs.(ix_global).dof_pos)
      ix_global_by_ix_local
  in
  let mesh_index_by_local_index =
    Array.map
      (fun ix_global ->
	 let site = mwe.mwe_dofs.(ix_global).dof_site in
	   if Array.length site >1
	   then failwith "mwe_dof_3d_surface_triangles: expected first order elements!"
	   else site.(0))
      ix_global_by_ix_local
  in
  let local_index_by_mesh_index = 
    let a = Array.make (Array.length mwe.mwe_mesh.mm_points) (-1) in
    let () =
      Array.iteri (fun ix_local ix_mesh -> a.(ix_mesh) <- ix_local)
	mesh_index_by_local_index
    in a
  in
  let bdof_solid_angles =
    Array.init nr_bdofs
      (fun nr_bdof -> solid_angle mwe.mwe_mesh.mm_points.(mesh_index_by_local_index.(nr_bdof)))
  in
  let rec collect_surfaces nr_bdof have_surfaces =
    if nr_bdof = nr_bdofs
    then Array.of_list have_surfaces
    else
      let bdof = mwe.mwe_dofs.(ix_global_by_ix_local.(nr_bdof)) in
      let mesh_ix = mesh_index_by_local_index.(nr_bdof) in
      let rec walk_sx_nel_ix rest_sx_nel_ix have_surfaces =
	match rest_sx_nel_ix with
	  | [] ->
		collect_surfaces (1+nr_bdof) have_surfaces
	  | sx_nel_ix::rest_sx_nel_ix ->
	      let (sx,_,_) = sx_nel_ix in
	      let bdof_indices =
		Array.map (fun p -> local_index_by_mesh_index.(p.mp_id)) sx.ms_points
	      in
	      let nr_this_bdof_in_simplex = array_position nr_bdof bdof_indices 0 in
	      let all_simplex_vertices_except_this_are_larger_and_also_on_surface n =
		let rec walk k =
		  if k = 4 then true
		  else if k = n then walk (1+k)
		  else if bdof_indices.(k) < bdof_indices.(nr_this_bdof_in_simplex) then false
		  else if bdof_indices.(k) = (-1) then false
		  else walk (1+k)
		in walk 0
	      in
	      let rec collect_all_surfaces_from_this_simplex
		  nr_opposing_point have_surfaces
		  =
		if nr_opposing_point = 4
		then walk_sx_nel_ix rest_sx_nel_ix have_surfaces
		else if nr_opposing_point = nr_this_bdof_in_simplex
		then collect_all_surfaces_from_this_simplex (1+nr_opposing_point) have_surfaces
		else
		  if not(all_simplex_vertices_except_this_are_larger_and_also_on_surface
			   nr_opposing_point)
		  then collect_all_surfaces_from_this_simplex (1+nr_opposing_point) have_surfaces
		  else
		    let this_surface_indices =
		      array_one_shorter bdof_indices nr_opposing_point
		    in
		    let this_surface_outward_normal =
		      match sx.ms_inv_ext_point_coords with
			| None -> failwith "mwe_dof_3d_surface_triangles: need triangle face equations"
			| Some s_face_eqns ->
			    let s_face_eqn = s_face_eqns.(nr_opposing_point) in
			    let s_normal = Array.sub s_face_eqn 0 3 in
			    let rescaling_factor =
			      (if (hyperplane_eval
				     s_face_eqn
				     sx.ms_points.(nr_opposing_point).mp_coords > 0.0)
			       then (-1.0) else 1.0) (* ensure outward-pointing normal XXX Check that this really is outward-pointing. Actually should be! *)
			      /. (sqrt (euclidean_len_sq s_normal))
			    in
			      begin
				for i=0 to 3-1 do
				  s_normal.(i) <- rescaling_factor*.s_normal.(i);
				done;
				s_normal
			      end
		    in
		      collect_all_surfaces_from_this_simplex
			(1+nr_opposing_point)
			((this_surface_indices,this_surface_outward_normal)::have_surfaces)
	      in
		collect_all_surfaces_from_this_simplex 0 have_surfaces
      in
	walk_sx_nel_ix bdof.dof_sx_nel_ix have_surfaces
  in
  let the_surfaces_and_normals = collect_surfaces 0 [] in
    (the_surfaces_and_normals,bdof_solid_angles,point_coords_by_local_index)
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

   NOTE: we both provide the old and new variant of that function.
   The calling interface has changed!

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



let lindholm_triangle_contributions_ng =
  let rec v_minus target v1 v2 =
    begin
      target.(0) <- v1.(0) -. v2.(0);
      target.(1) <- v1.(1) -. v2.(1);
      target.(2) <- v1.(2) -. v2.(2);
    end
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
    fun ~outward_surface_normal ~store_lh012 ~observer p0 p1 p2 ->
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
		  (* Quick "hack" to exclude nonsensical self-interaction terms.
		     XXX Actually, we should put more effort into doing this in a
		     clean fashion, but for now, this is good enough for us!
		  *)
		let lh0 = if classify_float lh0 <> FP_normal then 0.0 else lh0
		and lh1 = if classify_float lh1 <> FP_normal then 0.0 else lh1
		and lh2 = if classify_float lh2 <> FP_normal then 0.0 else lh2
		in
		let must_change_orientation = (* XXX this actually seems to be the right sign convention, but why? XXX is the outward-pointing surface normal convention right? *)
		  sprod v_zeta outward_surface_normal < 0.0
		in
		  begin
		    store_lh012.(0) <- if must_change_orientation then -.lh0 else lh0;
		    store_lh012.(1) <- if must_change_orientation then -.lh1 else lh1;
		    store_lh012.(2) <- if must_change_orientation then -.lh2 else lh2;
		  end
	      end
	  end
      end
;;

(* Note that the function below only takes arguments which are
   somewhat easily network-transmissible! This will become relevant
   later on!
*)

let lindholm_make_bdof_row
    ?(fun_add_contrib=(fun (index:int) (contrib:float) ->
			 failwith "lindholm_make_bdof_row: Need fun_add_contrib!"))
    ?observer_vertex_nr
    (surfaces_and_normals,bdof_solid_angles,point_coords_by_local_index) pos_observer
    =
  (*
  let fun_add_contrib index contrib =	(* DDD *)
    let () = Printf.printf " BEM contrib ix=%4d %10.7f\n%!" index contrib in
      fun_add_contrib index contrib
  in
  *)
  let buffer_lh012 = Array.make 3 0.0 in
  let nr_bdof_points = Array.length point_coords_by_local_index in
  let nr_surfaces = Array.length surfaces_and_normals in
    begin
      for i=0 to nr_surfaces-1 do
	let (surface_point_indices,outward_surface_normal) = surfaces_and_normals.(i) in
	let ix0 = surface_point_indices.(0) in
	let ix1 = surface_point_indices.(1) in
	let ix2 = surface_point_indices.(2) in
	let () = lindholm_triangle_contributions_ng
	  ~outward_surface_normal
	  ~store_lh012:buffer_lh012
	  ~observer:pos_observer
	  point_coords_by_local_index.(ix0)
	  point_coords_by_local_index.(ix1)
	  point_coords_by_local_index.(ix2)
	in
	  begin
	    fun_add_contrib ix0 buffer_lh012.(0);
	    fun_add_contrib ix1 buffer_lh012.(1);
	    fun_add_contrib ix2 buffer_lh012.(2);
	  end
      done;
      match observer_vertex_nr with
	| None -> ()
	| Some ix_observer ->
	    fun_add_contrib ix_observer (bdof_solid_angles.(ix_observer)/.(4.0*.pi)-.1.0)
    end
;;

let bem_fill_block_of_rows bdof_row_info ~start_row ~end_row mx_bem =
  let (surfaces_and_normals,bdof_solid_angles,point_coords_by_local_index) =
    bdof_row_info
  in
  let nr_rows = Array.length point_coords_by_local_index in
    for nr_row=start_row to end_row-1 do
      let fun_add_contrib index contrib =
	Mpi_petsc.matrix_inc mx_bem nr_row index contrib
      in
	lindholm_make_bdof_row
	  ~fun_add_contrib
	  ~observer_vertex_nr:nr_row (* Needed for modification of the diagonal! *)
	  bdof_row_info
	  point_coords_by_local_index.(nr_row)
    done
;;

(* NOTE: all entries are created on master! We may want to split this
   into parallel generation and parallel population. On the other hand,
   in the long run we will use Hlib!
*)

let bem_matrix
    ~fun_create_matrix
    ~fun_assemble_matrix
    ~fun_extract_petsc_matrix
    inside_regions ((dof_stem,_) as dof_name) mwe =
  let () = Printf.printf "Making BEM Matrix!\n%!" in
  let mesh = mwe.mwe_mesh in
  let restr = Some (Array.map (fun (Body_Nr r) -> (dof_stem,false,[|Some r;None|])) inside_regions) in
  let sdof_mappings = mwe_surface_dof_mappings (Array.to_list inside_regions) dof_name mwe in
  let (lts,stl,distrib) = mwe_shortvec_info mwe restr in
  let mxsize = Array.length stl in
  let mx = fun_create_matrix mxsize mxsize in
  let pmx = fun_extract_petsc_matrix mx in
  let bdof_row_info =
    mwe_dof_3d_surface_triangles_and_space_angles_and_coords (Array.to_list inside_regions)
      mwe (lts,stl)
  in
    (* See that we flush often... *)
    begin
      for row=0 to mxsize-1 do
	bem_fill_block_of_rows bdof_row_info ~start_row:row ~end_row:(1+row) pmx;
	(if row land 16 = 15 then fun_assemble_matrix mx false else ());
      done;
      fun_assemble_matrix mx true;
      mx
    end
;;

let make_H_exch_linalg_machine
    ?ccpla ~prefix
    ?pass_on_prematrix
    ~mwe_m ~mwe_H_exch material_names_and_Ms_and_J =
  let () = Printf.printf "DDD make_H_exch_linalg_machine #1\n%!" in
    (*
      let () = mwe_ensure_has_inv_volumes mwe_H_exch in
    *)
  let () = mwe_ensure_has_inv_volumes mwe_H_exch in
  let prefix = Printf.sprintf "%s.H_exch" prefix in
  let ddiffop_str = 
    Printf.sprintf "%s, j:3,k:3"
      (String.concat"+"
	 (Array.to_list
	    (Array.map
	       (fun (mname, m_s,j) ->
		  Printf.sprintf
		    "(%f)*<d/dxj H_exch_%s(k) || d/dxj m_%s(k)>"
		    (-.j) mname mname
	       ) material_names_and_Ms_and_J)))
  in
  let () = Printf.printf "DDD make_H_exch_linalg_machine ddiffop_str=%s\n%!" ddiffop_str in
  let () = Printf.printf "DDD make_H_exch_linalg_machine #2\n%!" in
  let script=
    {
     las_intensive_params=[||];
     las_mwes=[|mwe_m.mwe_name;mwe_H_exch.mwe_name|];
     las_internal_buffers=
     [|("inv_volumes",mwe_H_exch.mwe_name,None,true,0.0);
       (* XXX can I actually declare this a field? It certainly is not
	  a co-field, because then I would have a sensible integral! *)
       ("m",mwe_m.mwe_name,None,true,0.0);
       ("H_exch",mwe_H_exch.mwe_name,None,true,0.0);
     |];
     las_op_matrices=
     [|
       {loms_name="laplace_m";
	loms_mwe_name_le=mwe_H_exch.mwe_name;
	loms_mwe_name_ri=mwe_m.mwe_name;
	loms_symbolic_operator=ddiffop_str;
	loms_matoptions=[||];
	loms_ddy_jacobian=false;
       };
     |];
     las_dense_matrices=[||];
     las_ksps=[||];
     las_swexs=[||];
     las_jplans=[||];
     las_command_sequences=
     [|
       ("init_inv_volume_factors",[|("arg_inv_volumes",mwe_H_exch.mwe_name)|],[||],
	[|
	  LC_vec_distribute("arg_inv_volumes","inv_volumes");
	|]);
       ("compute_H_exch",
	[|("arg_m",mwe_m.mwe_name);
	  ("arg_H_exch",mwe_H_exch.mwe_name);
	|],
	[||],
	[|
	  LC_vec_distribute("arg_m","m");
	  LC_smx_x_pvec("laplace_m","m","H_exch");
	  (* LC_debug_printvec_1cpu("DDD LAM H_exch cofield","H_exch",3); *)
	  LC_vec_pointwise_mult("H_exch","inv_volumes","H_exch");
	  (* LC_debug_printvec_1cpu("DDD LAM H_exch field","H_exch",300); *)
	  LC_vec_collect("arg_H_exch","H_exch");
	|]);
     |]
   }
  in
  let () = Printf.printf "DDD make_H_exch_linalg_machine #3\n%!" in
  let pass_on_prematrices_by_name = 
    let h = Hashtbl.create 1 in
    let () =
      match pass_on_prematrix with
	| None -> ()
	| Some f ->
	    Hashtbl.add h "laplace_m" f
    in h
  in
  let the_machine = 
    make_linalg_machine ?ccpla ~prefix ~pass_on_prematrices_by_name ~relevant_mwes:[|mwe_m;mwe_H_exch|]
      script
  in
  let () = Printf.printf "DDD make_H_exch_linalg_machine #4\n%!" in
  let v_inv_vols =
    match !(mwe_H_exch.mwe_dof_funs_inv_volumes) with
    | None -> failwith "make_H_exch_linalg_machine: mwe_H_exch needs dof volume information!"
    | Some v ->
	v
  in
  let () = Printf.printf "DDD make_H_exch_linalg_machine #5\n%!" in
  let () =
    the_machine.execute_on
      "init_inv_volume_factors"
      [|FEM_field(mwe_H_exch,None,v_inv_vols)|] [||]
  in
  let () = Printf.printf "DDD make_H_exch_linalg_machine #6\n%!" in
  the_machine
;;



(*
   NOTE: The linalg_machines should be combined, so that we end up with
   make_mumag_linalg_machine, because operations like the distribution 
   of m across machines need not be duplicated.

   NOTE: We could make great use of even further design changes here:
   While our differential operator strings now can express many vital
   concepts such as modifications necessary to express DBC/NBC,
   we may want to include additional information, such as matrix symmetry.

   In the longer run, we will need a parsed specification language for
   linalg scripts...
*)

let make_H_demag_linalg_machine
    ?ccpla ~prefix
    ~mwe_m ~mwe_H_demag ~inside_regions
    ?(fun_make_and_register_mwe_field=fun (mwe:(float mesh_with_elements)) -> impossible())
    material_names_and_Ms_and_J =
  let mesh = mwe_m.mwe_mesh in
  let boundary_restriction_phi = Some [|("phi",false,[|Some (-1);None|])|] in
  let () = Printf.printf "DDD make_H_demag_linalg_machine #1\n%!" in
  let () =
    (if mwe_H_demag.mwe_mesh != mesh
     then failwith "make_H_demag_linalg_machine: mwe mesh mismatch!"
     else ())
  in
  let () = Printf.printf "DDD make_H_demag_linalg_machine #1\n%!" in
  let () = mwe_ensure_has_inv_volumes mwe_H_demag in
  let elem_scalar = make_element ~element_name:"H_demag_scalar" 3 ("S",[||]) 1 in
  let mwe_scalar = make_mwe "mwe_scalar" (fun _ -> elem_scalar) mesh in
  let mwe_rho = mwe_sibling "mwe_rho" "rho/S" [|("S","rho")|] mwe_scalar in
  let mwe_phi = mwe_sibling "mwe_phi" "phi/S" [|("S","phi")|] mwe_scalar in
  let ddiffop_div_m_str = 
    Printf.sprintf "%s, j:3"
      (String.concat"+"
	 (Array.to_list
	    (Array.map
	       (fun (mname, m_s,_) ->
		  Printf.sprintf
		    "(%f)*<rho || d/dxj m_%s(j)>+(%f)*<rho || D/Dxj m_%s(j)>"
		    m_s mname m_s mname)
	       material_names_and_Ms_and_J)))
  in
  let () = Printf.printf "DDD make_H_demag_linalg_machine #2\n%!" in
  let indices_bdy_phi = 
    array_mapfilter
      (fun dof -> 
	 if (List.tl dof.dof_in_body <> [])
	   &&
	   List.exists (fun bn -> -1 <> array_position bn inside_regions 0) dof.dof_in_body
	 then Some dof.dof_nr else None)
      mwe_phi.mwe_dofs
  in
  let () = Printf.printf "DDD make_H_demag_linalg_machine #3\n%!" in
  let script=
    {
     las_intensive_params=[||];
     las_mwes=[|mwe_m.mwe_name;
		mwe_H_demag.mwe_name;
		mwe_rho.mwe_name;
		mwe_phi.mwe_name;
	      |];
      las_internal_buffers=
	[|("inv_volumes_H_demag",mwe_H_demag.mwe_name,None,true,0.0);
	  (* XXX can I actually declare this a field? It certainly is not
	     a co-field, because then I would have a sensible integral! *)
	  ("m",mwe_m.mwe_name,None,true,0.0);
	  ("H_demag",mwe_H_demag.mwe_name,None,true,0.0);
	  ("rho",mwe_rho.mwe_name,None,false,0.0);
	  ("rho_s",mwe_rho.mwe_name,None,false,0.0); (* Effective surface charges for rho2 from D.B.C. *)
	  ("phi",mwe_phi.mwe_name,None,true,0.0);
	  ("phi1",mwe_phi.mwe_name,None,true,0.0);
	  ("phi2",mwe_phi.mwe_name,None,true,0.0);
	  ("phi1b",mwe_phi.mwe_name,boundary_restriction_phi,true,0.0);
	  ("phi2b",mwe_phi.mwe_name,boundary_restriction_phi,true,0.0);
     |];
     las_op_matrices=
     [|
       {loms_name              = "div_m";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_m.mwe_name;
	loms_symbolic_operator = ddiffop_div_m_str;
	loms_matoptions=[||];
	loms_ddy_jacobian=false;
       };
       {loms_name              = "neg_laplace_phi";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "<d/dxj rho || d/dxj phi>;gauge_fix:phi, j:3";
	loms_matoptions=[|Mpi_petsc.MAT_SYMMETRIC;Mpi_petsc.MAT_SYMMETRY_ETERNAL|];
	loms_ddy_jacobian=false;
       };
       (*
       {loms_name              = "laplace_phi";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "-<d/dxj rho || d/dxj phi>;gauge_fix:phi, j:3";
	loms_matoptions=[||];
       };
       *)
       {loms_name              = "grad_phi";
	loms_mwe_name_le       = mwe_H_demag.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "-<H_demag(j) || d/dxj phi>, j:3";
	loms_matoptions=[||];
	loms_ddy_jacobian=false;
       };
       {loms_name              = "laplace_DBC";
	loms_mwe_name_le       = mwe_phi.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "-<d/dxj phi[vol] || d/dxj phi[vol]>;phi[boundary=-1/*]=phi[boundary=-1/*], j:3";
	(* NOTE: vol is important here! *)
	loms_matoptions=[|Mpi_petsc.MAT_SYMMETRIC;Mpi_petsc.MAT_SYMMETRY_ETERNAL|];
	loms_ddy_jacobian=false;
       };
       {loms_name              = "load_DBC";
	loms_mwe_name_le       = mwe_phi.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "<d/dxj phi[vol] || d/dxj phi[boundary=-1/*]>;(L||R)=(*||phi[boundary=-1/*]), j:3";
	loms_matoptions=[||];
	loms_ddy_jacobian=false;
       };
     |];
     las_dense_matrices=
	[|
	  {ldms_name = "BEM";
	   ldms_hlib = false;
	   ldms_mwe_name = mwe_phi.mwe_name;
	   ldms_dof_name = ("phi",[||]);
	   ldms_inside_regions = inside_regions; (* <- XXX *)
	   ldms_lattice_info=None; (* extension, not supported by mumag3! *)
	   ldms_matoptions=[||];
	  };
	|];
     las_ksps=[|
       (*
       {lks_name = "solve_laplace_phi";
	lks_matrix_name = "laplace_phi";
	lks_precond_name = "laplace_phi";
	lks_ksp_type=if 0<>1 then None else Some "gmres" (*"cg"*);
	lks_pc_type=if 0<>1 then None else Some "ilu";
	lks_initial_guess_nonzero=None;
	lks_rtol=None;
	lks_atol=None;
	lks_dtol=None;
	lks_maxits=None;
       };
       *)
       {lks_name = "solve_neg_laplace_phi";
	lks_matrix_name = "neg_laplace_phi";
	lks_precond_name = "neg_laplace_phi";
	lks_ksp_type=if 1 =0 then None else Some "gmres" (*"cg"*);
	lks_pc_type=if 1  =0 then None else Some "ilu";
	lks_initial_guess_nonzero=Some true;
	lks_rtol=None;
	lks_atol=None;
	lks_dtol=None;
	lks_maxits=None;
       };
       {lks_name = "solve_laplace_DBC";
	lks_matrix_name = "laplace_DBC";
	lks_precond_name = "laplace_DBC";
	lks_ksp_type=None (* Some "cg" *);
	lks_pc_type=None (* Some "ilu" *);
	lks_initial_guess_nonzero=None;
	lks_rtol=None;
	lks_atol=None;
	lks_dtol=None;
	lks_maxits=None;
       };
     |];
     las_swexs=[||];
     las_jplans=[||];
     las_command_sequences=
	[|
	  ("init_inv_volume_factors",
	   [|("arg_inv_volumes_H_demag",mwe_H_demag.mwe_name)|],
	   [||],
	   [|
	     LC_vec_distribute("arg_inv_volumes_H_demag","inv_volumes_H_demag");
	   |]);
	  (* ------ *)
	  ("compute_H_demag",
	   [|("arg_m",mwe_m.mwe_name);
	     ("arg_H_demag",mwe_H_demag.mwe_name);
	   |],
	   [||],
	   [|
	     LC_vec_distribute("arg_m","m"); 
	     LC_smx_x_pvec("div_m","m","rho");
	     (* LC_psolve("solve_laplace_phi","rho","phi1"); *)
	     LC_pvec_scale("rho",-1.0);
	     LC_psolve("solve_neg_laplace_phi","rho","phi1");
	     LC_pvec_pull(indices_bdy_phi,"phi1","phi1b");
	     (* LC_debug_printvec_1cpu("phi1-bdofs","phi1b"); *)
	     LC_dmx_x_pvec("BEM","phi1b","phi2b");
	     (* LC_debug_printvec_1cpu("phi2-bdofs","phi2b"); *)
	     LC_smx_x_pvec("load_DBC","phi2b","rho_s");
	     (* LC_debug_printvec_1cpu("phi2-effective-rho","rho_s"); *)
	     LC_psolve("solve_laplace_DBC","rho_s","phi2");
	     LC_pvec_push(indices_bdy_phi,"phi2b","phi2");
	     (* LC_debug_printvec_1cpu("phi2-final","phi2"); *)
	     LC_pvec_axpby(NCOEFF_float 1.0,"phi1",NCOEFF_float 0.0,"phi");
	     LC_pvec_axpby(NCOEFF_float 1.0,"phi2",NCOEFF_float 1.0,"phi");
	     (* LC_debug_printvec_1cpu("phi-final","phi"); *)
	     LC_smx_x_pvec("grad_phi","phi","H_demag");
	     (* LC_debug_printvec_1cpu("H_demag-unrescaled","H_demag"); *)
	     (* LC_debug_printvec_1cpu("H_demag-volumes","inv_volumes_H_demag"); *)
	     LC_vec_pointwise_mult("H_demag","inv_volumes_H_demag","H_demag");
	     (* LC_debug_printvec_1cpu("H_demag","H_demag"); *)
	     LC_vec_collect("arg_H_demag","H_demag");
	   |]);
	|]
    }
  in
  let () = Printf.printf "DDD make_H_demag_linalg_machine #4\n%!" in
  let relevant_mwes = [|mwe_m;mwe_H_demag;mwe_rho;mwe_phi|] in
  let the_machine = 
    make_linalg_machine
      ?ccpla ~prefix
      ~relevant_mwes
      script
  in
  let () = Array.iter (fun mwe -> let _ = fun_make_and_register_mwe_field mwe in ()) relevant_mwes in
    (* ^ XXX This is a bad hack to placify the upper levels: we still have to make and register some fields,
       which can be accessed externally through the magsim_brain. However, with this approach,
       they will be empty. XXX ADJUST!
    *)
  let v_inv_vols =
    match !(mwe_H_demag.mwe_dof_funs_inv_volumes) with
      | None -> failwith "make_H_demag_linalg_machine: mwe_H_demag needs dof volume information!"
      | Some v ->
	  v
  in
  let () = Printf.printf "DDD make_H_demag_linalg_machine #5\n%!" in
  let () =
    the_machine.execute_on
      "init_inv_volume_factors"
      [|FEM_field(mwe_H_demag,None,v_inv_vols)|] [||]
  in
  let () = Printf.printf "DDD make_H_demag_linalg_machine #6\n%!" in
    the_machine
;;



(* TODO: linalg_machine should eventually be made visible at the python level as well.
   Q: How to then hook this up with sundials?
 *)

(* Hooking a parallel H_exch machine into existing mumag2 machinery... *)

let make_ccpla_exchange_fun
    ?ccpla
    ?(petsc_name=gensym "exchange_fun")
    ?(sloppy_cofield_to_field=true)
    ?(name_exchange="H_exch")
    all_materials
    mwe_H_exch mwe_m =
  let () = Printf.printf "DDD make_ccpla_exchange_fun #1\n%!" in
  let material_names_and_Ms_and_J = Array.map (fun m -> (m.Mumag2.llg_name,m.Mumag2.llg_Ms,m.Mumag2.llg_J)) all_materials in
  let prefix=petsc_name in
  let r_H_exch = ref None in
  let fun_process_pmx ht =
    r_H_exch := Some ht
  in
  let () = Printf.printf "DDD make_ccpla_exchange_fun #2\n%!" in
  let machine =
    make_H_exch_linalg_machine
      ?ccpla ~prefix ~pass_on_prematrix:fun_process_pmx
      ~mwe_m ~mwe_H_exch
      material_names_and_Ms_and_J
  in
  let () = Printf.printf "DDD make_ccpla_exchange_fun #3\n%!" in
  let v_dummy = Mpi_petsc.vector_dummy() in
  let fun_H_exch ?target field_m =
    let field_target =
      match target with
	| Some ((FEM_field (_,_,_)) as field) -> 
	    let () = ensure_field_is_unrestricted field in (* XXX *)
	      field
	| None ->
	    make_field mwe_H_exch
    in
    let () = machine.execute_on "compute_H_exch" [|field_m;field_target|] [||] in
      field_target
  in
  let () = Printf.printf "DDD make_ccpla_exchange_fun #4\n%!" in
    match !r_H_exch with
      | None -> failwith "make_ccpla_based_prematrix_and_fun_H_exch: could not find H_exch!"
      | Some ht ->
	  (ht,fun_H_exch)
;;


let make_ccpla_demag_fun_3d
    ?ccpla
    (* === XXX NOTE: THIS LOT IS DUMMY PARAMETERS WHICH WILL EVEN BE IGNORED! XXX ===
       (We just ensure our signature is compatible to mumag2.ml. We will soon get rid
       of mumag2.ml, and hence will not need this heart-lung-machine wrapping stuff
       much longer!)
    *)
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
    all_mats
    mwe_h mwe_m
    =
  let () = Printf.printf "DDD make_ccpla_demag_fun_3d #1\n%!" in
  let material_names_and_Ms_and_J =
    Array.map (fun mat -> (mat.Mumag2.llg_name,mat.Mumag2.llg_Ms,mat.Mumag2.llg_J)) all_mats
  in
  let machine =
    make_H_demag_linalg_machine ?ccpla ~prefix:"lam_H_demag"
      ~mwe_m ~mwe_H_demag:mwe_h
      ~inside_regions:(Array.of_list inside_regions)
      ~fun_make_and_register_mwe_field
      material_names_and_Ms_and_J
  in
  let () = Printf.printf "DDD make_ccpla_demag_fun_3d #2\n%!" in
  let fun_H_demag
      ?(compute_field_rho=false)
      ?(debug_charge_imbalance=false)
      ?target field_m
      =
    let field_target =
      match target with
	| Some ((FEM_field (_,_,_)) as field) -> 
	    let () = ensure_field_is_unrestricted field in (* XXX *)
	      field
	| None ->
	    make_field mwe_h
    in
    let () = machine.execute_on "compute_H_demag" [|field_m;field_target|] [||] in
      field_target
  in
  let () = Printf.printf "DDD make_ccpla_demag_fun_3d #3\n%!" in
    fun_H_demag
;;
    

(* ======

TODO:

Right now, we always use a linalg_machine to compute H_exch. Depending 
on whether our script is started standalone or under mpirun control, 
this linalg_machine will employ either single-process PETSc commands,
or distributed linear algebra to compute H_exch.

At present, my timings on alpha for barmini_par.py are:

25.776 sec -- old method
27.268 sec -- linalg_machine, single-process
28.394 sec -- linalg_machine, MPI, 2 processes

We are a little bit slower than with the old method. I think the reasons
for this are:

(1) The linalg_machine will start off by copying the data vectors in 
question to its own workspace - and in the end, it will copy the result
data vector out of its own workspace to the user's workspace. This is 
necessary for parallel operation (because we have to collect vector 
data from individual nodes), but actually not necessary in 
single-process mode. So, we have a bit of unnecessary overhead here.

(2) I use a manifestly symmetric H_exch matrix, but I did not tell petsc
about this symmetry. This maps the m field to the H_exch cofield. I then
use the box method - by point-wise multiplying vector components with 
the corresponding inverse volumes of the shape functions - in a separate
step. The old code folded this rescaling into the coefficients of the 
H_exch matrix (which then no longer is symmetric). I suppose this may be
repaired by adding configuration options to provide information about 
the symmetry of some differential operator. Then, petsc should be able 
to store the H_exch matrix in half as much space, thereby greatly 
reducing memory bandwidth requirements.

   ====== *)

(* === PERIODIC BOUNDARY CONDITIONS, PERIODIC-LINDHOLM === *)

(* How to do shell-wise addition with cells that are
   to be taken into account partially?

   We need:

   * a "global shape", which we define to be a function S that:

   - has no zeroes outside a unit sphere
   - is positive in a neighbourhood of the origin(!)
   - is positive in a region which has the large-scale 
     shape of our sample (e.g. a sphere, a disc, a box,
     etc.), and negative in the "outside" regions.

   * Lattice periodicity data, in the form of 3 optional 
     lattice vectors (BEM will only work in 3d anyway due
     to the Lindholm formula!) - so we may have periodicity
     in just one or one or two directions.

   * a "cell rescaling factor" telling us by how much to
     shrink the cell to fit it into the global shape.

   * One oversampling factor for every direction - we use this to
     estimate what fraction of the cell actually lies inside the
     global shape. (Determining these numbers may take some time,
     but we only have to do that once, or twice if we want an
     independent check with increased resolution...)

   How do we take care of "cutting off too early"? Basically, we would
   have to validate against contributions obtained by doing (part 
   of?) the next layer. However, this presumably then should be up 
   to the user. So, we should at least make sure we design this in such
   a way that we allow the computation of individual entries...

   IMPORTANT NOTE: we demand from the user that he should choose his
   unit cell in such a way that [|0.0;0.0;0.0|] is the midpoint. If he
   does otherwise, he may get quite strange artefacts!
*)

let global_shape_approximating_lattice_points_and_greyfactors
    (* "greyfactors" are what we get from oversampling: if our
       measurements show that only 30% of a faraway cell is
       inside the shape boundary, this is 0.3. *)
    ~oversampling_steps
    ~fun_global_shape
    ~v_lattice 
    (* If we want periodicity in only one or two directions,
       we can get this by just using a null vector as the 
       corresponding lattice yslvector.
    *)
    ~cell_rescaling (* >1, we use the inverse. *)
    =
  let dim = Array.length v_lattice in
  let taxi_norm v =
    Array.fold_left (fun sf x -> max sf (abs_float x)) 0.0 v
  in
  (* First, we have to find all the lattice vectors inside the unit sphere. *)
  let inv_cell_rescaling=1.0/.cell_rescaling in
  let v_lattice_len2 = Array.map euclidean_len_sq v_lattice in
  let v_rescaled_lattice =
    Array.map (Array.map (fun x -> x*.inv_cell_rescaling)) v_lattice
  in
  let v_sampling_steps =
    array_pointwise
      (fun nr_steps v ->
	 let z = 1.0/.(float_of_int nr_steps) in
	   Array.map (fun x -> x*.z) v)
      oversampling_steps
      v_rescaled_lattice
  in
  let oversampling_inv_nr_cells =
    1.0/.(float_of_int (Array.fold_left ( * ) 1 oversampling_steps))
  in
  let intcoords_to_pos ~rescaled i_xyz =
    let v_lattice_eff = if rescaled then v_rescaled_lattice else v_lattice in
    let result = Array.make dim 0.0 in
    let () =
      for dir=0 to dim-1 do
	for j=0 to dim-1 do
	  result.(j) <- result.(j) +.
	    (float_of_int i_xyz.(dir))*.v_lattice_eff.(dir).(j)
	done
      done
    in
      result
  in
  let greyfactor i_xyz =
    let pos = intcoords_to_pos ~rescaled:true i_xyz in
    let r_nr_cells_inside = ref 0 in
    let delta = Array.make dim 0.0 in
    let real_pos = Array.make dim 0.0 in
    let () = multifor oversampling_steps
      (fun _ v_subpos ->
	 begin
	   for i=0 to dim-1 do
	     delta.(i) <- 
	       (float_of_int v_subpos.(i))-.
	       (float_of_int (oversampling_steps.(i)-1))*.0.5;
	     real_pos.(i) <- pos.(i);
	   done;
	   for n=0 to dim-1 do
	     for i=0 to dim-1 do
	       real_pos.(i) <- real_pos.(i)
	       +. delta.(n) *. v_sampling_steps.(n).(i)
	     done;
	   done;
	   let f_shape = fun_global_shape real_pos in
	   if f_shape > 0.0
	   then
	     r_nr_cells_inside := 1+ !r_nr_cells_inside
	   else ()
	 end)
    in
      (float_of_int (!r_nr_cells_inside)) *. oversampling_inv_nr_cells
  in
  let lattice_points_in_unit_box_and_greyfactors =
    let ht_seen = Hashtbl.create 17 in
    let neighbours_added i_xyz list =
      let rec add_dir nr_dir have =
	if nr_dir = dim then have
	else if v_lattice_len2.(nr_dir) = 0.0
	then add_dir (1+nr_dir) have
	else
	  let have_next =
	    (Array.mapi (fun n c -> if n=nr_dir then c+1 else c) i_xyz)
	    ::(Array.mapi (fun n c -> if n=nr_dir then c-1 else c) i_xyz)
	    ::have
	  in add_dir (1+nr_dir) have_next
      in add_dir 0 list
    in
    let rec walk_nodes intcoords_hot_points =
      match intcoords_hot_points with
	| [] -> ()
	| (i_xyz::rest_intcoords_hot_points) ->
	    if Hashtbl.mem ht_seen i_xyz (* We already encountered this point *)
	    then walk_nodes rest_intcoords_hot_points
	    else if taxi_norm (intcoords_to_pos ~rescaled:true i_xyz) > 1.0
	    then
	      (* We did not yet encounter this point, but it "just"
		 crossed the sphere surface! Part of this cell presumably
		 may still be inside the sphere, so we add it to the to-be-analyzed
		 list, but we do not walk its neighbours! *)
	      let () = Hashtbl.add ht_seen i_xyz true in
		walk_nodes rest_intcoords_hot_points
	    else
	      (* inner point - add it, and also investigate its neighbours. *)
	      let () = Hashtbl.add ht_seen i_xyz true in
		walk_nodes (neighbours_added i_xyz rest_intcoords_hot_points)
    in
    let () = walk_nodes [Array.make dim 0] in
      array_filter (fun (_,x) -> x<>0.0)
	(map_hashtbl_to_array
	   (fun i_xyz _ ->
	      ((intcoords_to_pos ~rescaled:false i_xyz),
	       greyfactor i_xyz))
	   ht_seen)
  in
  let () = Printf.printf "\n%!" in
  let ddd = array_foreach_do lattice_points_in_unit_box_and_greyfactors
      (fun (pos,gf) -> Printf.printf "GF POS: %s val: %6.4f\n%!" (float_array_to_string pos) gf)
  in
  lattice_points_in_unit_box_and_greyfactors
;;

(*
let gsalpag_sphere_test =
  global_shape_approximating_lattice_points_and_greyfactors
    ~oversampling_steps:[|8;8;8|]
    ~fun_global_shape:(fun v -> 1.0-.(euclidean_len_sq v))
    ~v_lattice:[|[|1.0;0.0;0.0|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|]
    ~cell_rescaling:5.0
;;

Array.iter (fun (xyz,s) -> Printf.printf "%5.3f: %4.2f  %4.2f  %4.2f\n" s xyz.(0) xyz.(1) xyz.(2)) gsalpag_sphere_test;;
*)

(* NOTE: we will use this in future when we finished changing over to
   the new LH calling conventions. We also provide a re-implementation
   that uses the old calling conventions! *)

let pbc_lindholm_triangle_contributions_ng
    ~oversampling_steps
    ~fun_global_shape
    ~v_lattice 
    ~cell_rescaling (* >1, we use the inverse. *)
    =
  let points_and_greyfactors =
    global_shape_approximating_lattice_points_and_greyfactors
      ~oversampling_steps
      ~fun_global_shape
      ~v_lattice 
      ~cell_rescaling
  in
  let v_buffer = Array.make 3 0.0 in
  let displaced_observer = Array.make 3 0.0 in
    fun
      ~outward_surface_normal
      ~store_lh012
      ~observer
      p0 p1 p2 ->
	begin
	  for i=0 to 3-1 do 
	    v_buffer.(i) <- 0.0;
	    store_lh012.(i) <- 0.0;
	  done;
	  for i=0 to Array.length points_and_greyfactors-1 do
	    let (pos,greyfactor) = points_and_greyfactors.(i) in
	      begin
		for i=0 to 3-1 do
		  displaced_observer.(i) <- observer.(i) -. pos.(i);
		done;
		lindholm_triangle_contributions_ng ~outward_surface_normal
		  ~store_lh012:v_buffer
		  ~observer:displaced_observer p0 p1 p2;
		for i=0 to 3-1 do
		  store_lh012.(i) <- store_lh012.(i)+.v_buffer.(i)*.greyfactor
		done;
	      end
	  done
	end
;;

(* Compatibility variant - to go away soon... *)
let pbc_lindholm_triangle_contributions
    ~oversampling_steps
    ~fun_global_shape
    ~v_lattice 
    ~cell_rescaling (* >1, we use the inverse. *)
    =
  let points_and_greyfactors =
    global_shape_approximating_lattice_points_and_greyfactors
      ~oversampling_steps
      ~fun_global_shape
      ~v_lattice 
      ~cell_rescaling
  in
  let store_lh012 = Array.make 3 0.0 in
  let displaced_observer = Array.make 3 0.0 in
    fun
      ?store_zeta
      observer
      p0 p1 p2 ->
	begin
	  for i=0 to 3-1 do 
	    store_lh012.(i) <- 0.0;
	  done;
	  for i=0 to Array.length points_and_greyfactors-1 do
	    let (pos,greyfactor) = points_and_greyfactors.(i) in
	      begin
		for i=0 to 3-1 do
		  displaced_observer.(i) <- observer.(i) -. pos.(i);
		done;
		let lh_res =
		  lindholm_triangle_contributions ?store_zeta
		    displaced_observer p0 p1 p2 
		in
		  for i=0 to 3-1 do
		    (let fc = classify_float lh_res.(i) in
		       if fc = FP_nan || fc = FP_infinite 
		       then failwith (Printf.sprintf "BEM matrix contribution (observer=%s p0=%s p1=%s p2=%s): %f" (float_array_to_string displaced_observer) (float_array_to_string p0) (float_array_to_string p1) (float_array_to_string p2) lh_res.(i))
		       else ());
		    store_lh012.(i) <- store_lh012.(i)+.lh_res.(i)*.greyfactor
		  done;
	      end
	  done;
	  store_lh012
	end
;;
(* this function (slp=set lattice points) does the same 
thing as the pbc_lindholm_triangle_contributions one 
but in this case the points of the lattice are given,
and the greyfactors are taken as 1 (full contribution
of the given cells)
*)
let slp_lindholm_triangle_contributions
    ~pts_lattice
    =
  let points_and_greyfactors =
    Array.map ( fun v -> (v,1.0)) pts_lattice 
  in
  let () = Printf.printf "points and greyfactors: \n%!" in
  let () = Array.iter (fun (a,b) -> Printf.printf "%s %f\n%!" (float_array_to_string a) b) points_and_greyfactors in
  let store_lh012 = Array.make 3 0.0 in
  let displaced_observer = Array.make 3 0.0 in
    fun
      ?store_zeta
      observer
      p0 p1 p2 ->
	begin
	  for i=0 to 3-1 do 
	    store_lh012.(i) <- 0.0;
	  done;
	  for i=0 to Array.length points_and_greyfactors-1 do
	    let (pos,greyfactor) = points_and_greyfactors.(i) in
	      begin
		for i=0 to 3-1 do
		  displaced_observer.(i) <- observer.(i) -. pos.(i);
		done;
		let lh_res =
		  lindholm_triangle_contributions ?store_zeta
		    displaced_observer p0 p1 p2 
		in
		  for i=0 to 3-1 do
		    (let fc = classify_float lh_res.(i) in
		       if fc = FP_nan || fc = FP_infinite 
		       then failwith (Printf.sprintf "BEM matrix contribution (observer=%s p0=%s p1=%s p2=%s): %f" (float_array_to_string displaced_observer) (float_array_to_string p0) (float_array_to_string p1) (float_array_to_string p2) lh_res.(i))
		       else ());
		    store_lh012.(i) <- store_lh012.(i)+.lh_res.(i)*.greyfactor
		  done;
	      end
	  done;
	  store_lh012
	end
;;

