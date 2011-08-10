(* (C) 2007 Dr. Thomas Fischbacher *)

(*
  ocamlc -I ../mumag2 -I ../snippets -I ../ccpla -I ../mesh -I ../fem -I ../mpi_petsc -I ../nsim_grammars -I ../hlib -i bem3d.ml
*)

open Snippets;;
open Mesh;;
open Fem;;

external lindholm_triangle_contributions_C: float -> float
array array -> unit =
    "caml_bem3d_raw_lindholm"
;;

external lindholm_triangle_contributions_C_ld: float -> float array array -> unit =
    "caml_bem3d_raw_lindholm_longdouble"
;;

external raw_lindholm_block_pbc: float ->
  (float array * float array * float array (* p0, p1, p2 *)
   * float array (* outward surface normal *)
   * float array array	(* store_lh023: array[3] of float array[n] *)
   * float array array (* orthogonal matrix *)
   * float array (* greyfactors *)
   * float array array (* displacements *)
   * float array array) (* observer points *)
   -> unit =
    "caml_bem3d_raw_lindholm_block_pbc"
;;

let mwe_dof_3d_surface_triangles_and_space_angles_and_coords
    ~inside_property mwe (ix_local_by_ix_global,ix_global_by_ix_local)
    =
  let mesh = mwe.mwe_mesh in
  let simplices = mesh.mm_simplices in
  let () =
    (if mesh.mm_dim <> 3
     then failwith "mwe_dof_3d_surface_triangles: Need 3d mesh to extract surface triangles"
     else ())
  in
  let face_eqn =
    let inv_point_matrices =
      Simplex.get_inv_point_matrices mesh.mm_simplex_data
    in
      (fun sx_nr face_nr ->
         Array.init 4 (fun i -> inv_point_matrices.{sx_nr, face_nr, i}))
  in
  let region_is_inside r =
    let props = try Hashtbl.find mwe.mwe_properties_by_region r with | Not_found -> [||] in
      (array_position inside_property props 0) <> -1
  in
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
      let fillcount = bdof.dof_sx_nel_ix_fillcount in
      let rec walk_sx_nel_ix n have_surfaces =
	if n = fillcount
	then collect_surfaces (1+nr_bdof) have_surfaces
	else
	  let sx = simplices.(bdof.dof_sx_nel_ix.(2*n)) in
	  let nel_ix = bdof.dof_sx_nel_ix.(2*n+1) in
	  let ix = nel_ix land 0xffff in
	  let nel = nel_ix lsr 16 in
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
	    then walk_sx_nel_ix (1+n) have_surfaces
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
                  let s_face_eqn = face_eqn sx.ms_id nr_opposing_point in
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
	walk_sx_nel_ix 0 have_surfaces
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
		(* let () = Printf.printf "OK %10.8f %10.8f %10.8f\n" lh0 lh1 lh2 in *)
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
    ?(high_precision=false)
    ?(min_dist=0.0)
    ?(fun_add_contrib=(fun (index:int) (contrib:float) ->
			 failwith "lindholm_make_bdof_row: Need fun_add_contrib!"))
    ?observer_vertex_nr
    (surfaces_and_normals,bdof_solid_angles,point_coords_by_local_index) pos_observer
    =
  let buffer_lh012 = Array.make 3 0.0 in
  let nr_bdof_points = Array.length point_coords_by_local_index in
  let nr_surfaces = Array.length surfaces_and_normals in
  let args = Array.make 6 [||] in
  let () = args.(1) <- buffer_lh012 in
  let () = args.(2) <- pos_observer in
    begin
      for i=0 to nr_surfaces-1 do
	let (surface_point_indices,outward_surface_normal) = surfaces_and_normals.(i) in
	let ix0 = surface_point_indices.(0) in
	let ix1 = surface_point_indices.(1) in
	let ix2 = surface_point_indices.(2) in
	let () =
	  begin
	    args.(0) <- outward_surface_normal;
	    args.(3) <- point_coords_by_local_index.(ix0);
	    args.(4) <- point_coords_by_local_index.(ix1);
	    args.(5) <- point_coords_by_local_index.(ix2);
	  end
	in
	let () =
	  (if high_precision then lindholm_triangle_contributions_C_ld
	   else lindholm_triangle_contributions_C)
	    min_dist args
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


let bem_fill_block_of_rows ?(min_dist=0.0) bdof_row_info ~start_row ~end_row mx_bem =
  let (surfaces_and_normals,bdof_solid_angles,point_coords_by_local_index) =
    bdof_row_info
  in
  let nr_rows = Array.length point_coords_by_local_index in
    for nr_row=start_row to end_row-1 do
      let fun_add_contrib index contrib =
	Mpi_petsc.matrix_inc mx_bem nr_row index contrib
      in
	lindholm_make_bdof_row
	  ~min_dist
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
    ?(min_dist=0.0)
    ?geom_info
    ~fun_create_matrix
    ~fun_assemble_matrix
    ~fun_extract_petsc_matrix
    ?(field_name="phi")
    ?(inside_property="material")
    ?(outside_property="outer")
    ((dof_stem,_) as dof_name) mwe =
  let () = Printf.printf "Making BEM Matrix!\n%!" in
  let mesh = mwe.mwe_mesh in
  let restr =
    Some [|(field_name,
	    Ddiffop_parser.DLOG_and
	      [Ddiffop_parser.DLOG_some inside_property;Ddiffop_parser.DLOG_some outside_property;])|]
  in
  let (lts,stl,distrib) = mwe_shortvec_info mwe restr in
  let mxsize = Array.length stl in
  let mx = fun_create_matrix mxsize mxsize in
  let pmx = fun_extract_petsc_matrix mx in
  let bdof_row_info =
    match geom_info with
      | None ->
	  mwe_dof_3d_surface_triangles_and_space_angles_and_coords inside_property
	    mwe (lts,stl)
      | Some x -> x
  in
    (* See that we flush often... *)
    begin
      for row=0 to mxsize-1 do
	bem_fill_block_of_rows ~min_dist bdof_row_info ~start_row:row ~end_row:(1+row) pmx;
	(if (row land 31) = 31 then fun_assemble_matrix mx false else ());
      done;
      fun_assemble_matrix mx true;
      mx
    end
;;

let bem_hmatrix
    ?(cluster_strategy=2) ?(algorithm=4) ?(nfdeg=2) ?(nmin=50) ?(eta=2.0)
    ?(eps_aca=0.00001) ?(eps=0.00001) ?(p=3) ?(kmax=50)
    ?geom_info
    ?lattice_info
    ?(field_name="phi")
    ?(inside_property="material")
    ?(outside_property="outer")
    ((dof_stem,_) as dof_name)
    mwe =
  (* building an HLib h-matrix BEM *)
  let restr =
    Some [|(field_name,
	    Ddiffop_parser.DLOG_and
	      [Ddiffop_parser.DLOG_some inside_property;Ddiffop_parser.DLOG_some outside_property;])|]
  in
  let (lts,stl,distrib) = mwe_shortvec_info mwe restr in
  let (surfaces_and_normals,solid_angles,vertex_coords) =
    match geom_info with
      | None ->
	  mwe_dof_3d_surface_triangles_and_space_angles_and_coords inside_property
	    mwe (lts,stl)
      | Some x -> x
  in
  let triangles = Array.map (fun (x,_) -> x) surfaces_and_normals in
  let normals = Array.map (fun (_,x) -> x) surfaces_and_normals in
  (*let () =
    let faas aa = string_array_to_string (Array.map float_array_to_string aa) in
    let iaas aa = string_array_to_string (Array.map int_array_to_string aa) in
      Printf.printf "DDD H-MATRIX\n vertices %s\n triangles %s\n normals %s\n%!"
	(faas vertex_coords) (iaas triangles) (faas normals)
  in*)
  let hmx = Hlib.make_hmatrix ~cluster_strategy ~algorithm ~nfdeg ~nmin ~eta
                              ~eps_aca ~eps ~p ~kmax
                              ?lattice_info
                              vertex_coords triangles normals in
  let pi = 4.0*.atan 1.0 in
  let v_diagonal =
    Mpi_petsc.vector_pack
      ~name:(Printf.sprintf "HMX-diag-%s" mwe.mwe_name)
      (Array.mapi
	 (fun ix_short ix_long -> solid_angles.(ix_short)/.(4.0*.pi)-.1.0) stl)
  in
  let v_buffer =
    Mpi_petsc.vector_create
      (Array.length stl)
      (Printf.sprintf "HMX-diag-buffer-%s" mwe.mwe_name)
  in
  let () = Mpi_petsc.vector_assemble v_buffer  in
    (hmx,v_diagonal,v_buffer)
;;

let apply_bem_hmatrix (hmx,v_diagonal,v_buffer) v_target v_src =
  begin
    Mpi_petsc.with_petsc_vector_as_bigarray v_target
      (fun ba_target ->
	 Mpi_petsc.with_petsc_vector_as_bigarray v_src
	   (fun ba_src ->
	      Hlib.apply_hmatrix hmx ba_target ba_src
	   ));
    Mpi_petsc.vector_pointwise_mult v_src v_diagonal v_buffer;
    Mpi_petsc.vector_AXPBY 1.0 1.0 v_buffer v_target;
  end
;;

(* Return an array of tuples (val, desc) where val is a float in representing
   a size in units of megabytes, while desc is a string descibing what val is.
 *)
let get_bem_hmatrix_stats (hmx, _, _) = Hlib.get_hmatrix_stats_verbose hmx;;

(* Get a string with a description of the given bem hmatrix
   (used for logging details about created hmatrices). *)
let get_bem_hmatrix_stat_string bem_hmx =
  let lines =
    Array.map
     (fun (size, units, desc) ->
        Printf.sprintf "%50s: %10.2f %s" desc size units)
     (get_bem_hmatrix_stats bem_hmx)
  in
  let str = String.concat "\n" (Array.to_list lines)
  in "HLib matrix statistics:\n" ^ str
;;

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

(* === Eventually, it turned out that it is most appropriate to do the bulk of the boundary
   element matrix computation in C, as we then can make better use of the SIMD capabilities
   of modern processors. This has to be combined with (1) support for macrogeometry "faraway"
   boundaries, and (2) means to build only part of a boundary element matrix,
   for parallelization. Let us temporarily call this new interface "new_and_shiny" (short: _nas)
   to have a tag for it. Eventually, this will replace the present mess.
   === *)

let identity_3d = [|[|1.0;0.0;0.0;|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|];;

let nas_lindholm_all_contributions_by_triangle =
  let default_gf =[|1.0|]
  and default_displacements = [|[|0.0;0.0;0.0|]|]
  in
    (fun
       ?(min_dist=0.0)
       ?(ortho_transformation=identity_3d)
       ?(greyfactors=default_gf)
       ?(displacements=default_displacements)
       ~observer_points
       ~p012
       ~outward_surface_normal
       ~store_lh012
       () ->
	 raw_lindholm_block_pbc
	   min_dist
	   (p012.(0),p012.(1),p012.(2),outward_surface_normal,
	    store_lh012,ortho_transformation,
	    greyfactors,displacements,
	    observer_points)
    )
;;

let nas_partially_fill_bem_matrix_row_constrained
    ?(min_dist=0.0)
    ?(nr_flushes=10)
    ?geom_info
    ?lattice_info
    (* None or Some (ortho_transformations, [|(nr_orthogonal_transformation,greyfactor,displacement)|]) *)
    ~fun_add_contrib
    ~fun_assemble_matrix (* Note: we may have to go through the CCPLA for assembly, hence this is a parameter! *)
    ?rows_to_fill
    ?(field_name="phi")
    ?(inside_property="material")
    ?(outside_property="outer")
    ((dof_stem,_) as dof_name)
    mwe =
  let mesh = mwe.mwe_mesh in
  let restr =
    Some [|(field_name,
	    Ddiffop_parser.DLOG_and
	      [Ddiffop_parser.DLOG_some inside_property;Ddiffop_parser.DLOG_some outside_property;])|]
  in
  let (lts,stl,distrib) = mwe_shortvec_info mwe restr in
  let mxsize = Array.length stl in
  let rows_todo =
    match rows_to_fill with
      | None -> Array.init mxsize identity
      | Some x -> x
  in
  let nr_rows_todo = Array.length rows_todo in
  let lh012_buffer = Array.init 3 (fun _ -> Array.make nr_rows_todo 0.0) in
  let p012_buffer = Array.make 3 [||] in
  let (surfaces_and_normals,bdof_solid_angles,vertex_coords) =
    match geom_info with
      | None ->
	  mwe_dof_3d_surface_triangles_and_space_angles_and_coords inside_property
	    mwe (lts,stl)
      | Some x -> x
  in
  let row_observer_points = Array.map (fun n -> vertex_coords.(n)) rows_todo in
  let greyfactors_and_displacements_sorted_by_otrans =
    match lattice_info with
      | None -> [|(identity_3d,[|1.0|],[|[|0.0;0.0;0.0|]|])|]
      | Some (v_otrans,v_nr_ot_gf_disp) ->
	  let () =
	    Array.iter
	      (fun (otrans,greyfactor,displacement) ->
		 Printf.printf "GF POS: %s val: %6.4f\n%!" (float_array_to_string displacement) greyfactor
	      )
	      v_nr_ot_gf_disp
	  in
	    Array.mapi
	    (fun nr_otrans otrans ->
	       let relevant_entries = array_filter (fun (n,_,_) -> n=nr_otrans) v_nr_ot_gf_disp in
		 (otrans,
		  Array.map (fun (_,x,_) -> x) v_nr_ot_gf_disp, (* greyfactors *)
		  Array.map (fun (_,_,x) -> x) v_nr_ot_gf_disp  (* displacements *)
		 ))
	    v_otrans
  in
  let leading_triangle_index indices =
    min indices.(0) (min indices.(1) indices.(2))
  in
    (* We have to find out early what this node's workload will be
       so that we can determine when to flush...
    *)
  let flush_how_often =
    let nr_relevant = Array.length surfaces_and_normals in
      Array.init nr_relevant (fun n -> ((n+1)*nr_flushes)/nr_relevant-(n*nr_flushes)/nr_relevant)
  in
  let () = array_foreach_do_n surfaces_and_normals
    (fun nr_triangle (surface_point_indices, outward_surface_normal) ->
       begin
	 for i=0 to 3-1 do
	   for j=0 to nr_rows_todo-1 do
	     lh012_buffer.(i).(j) <- 0.0;
	   done
	 done;
	 Array.iter
	   (fun (otrans,greyfactors,displacements) ->
	      let () = p012_buffer.(0) <- vertex_coords.(surface_point_indices.(0)) in
	      let () = p012_buffer.(1) <- vertex_coords.(surface_point_indices.(1)) in
	      let () = p012_buffer.(2) <- vertex_coords.(surface_point_indices.(2)) in
		nas_lindholm_all_contributions_by_triangle
		  ~min_dist
		  ~ortho_transformation:otrans
		  ~greyfactors
		  ~displacements
		  ~observer_points:row_observer_points
		  ~p012:p012_buffer
		  ~outward_surface_normal
		  ~store_lh012:lh012_buffer ())
	   greyfactors_and_displacements_sorted_by_otrans;
	 for i=0 to 3-1 do
	   for j=0 to nr_rows_todo-1 do
	     fun_add_contrib rows_todo.(j) surface_point_indices.(i) lh012_buffer.(i).(j);
	   done
	 done;
	 for i=0 to flush_how_often.(nr_triangle)-1 do
	   fun_assemble_matrix false (* flush only *)
	 done;
       end
       )
  in
  let () = (* adjusting the diagonal *)
    array_foreach_do rows_todo
      (fun nr_row -> fun_add_contrib nr_row nr_row (bdof_solid_angles.(nr_row)/.(4.0*.pi)-.1.0))
  in
  let () = fun_assemble_matrix true (* final assembly *)
  in ()
;;


(* XXX Presumably, this is to become obsolete: *)
let nas_partially_fill_bem_matrix_col_constrained
    ?(min_dist=0.0)
    ?(nr_flushes=10)
    ?geom_info
    ?lattice_info
    (* None or Some (ortho_transformations, [|(nr_orthogonal_transformation,greyfactor,displacement)|]) *)
    ~fun_add_contrib
    ~fun_assemble_matrix (* Note: we may have to go through the CCPLA for assembly, hence this is a parameter! *)
    ?columns_to_fill
    ?(field_name="phi")
    ?(inside_property="material")
    ?(outside_property="outer")
    mwe =
  let mesh = mwe.mwe_mesh in
  let restr =
    Some [|(field_name,
	    Ddiffop_parser.DLOG_and
	      [Ddiffop_parser.DLOG_some inside_property;Ddiffop_parser.DLOG_some outside_property;])|]
  in
  let (lts,stl,distrib) = mwe_shortvec_info mwe restr in
  let mxsize = Array.length stl in
  let cols_todo =
    match columns_to_fill with
      | None -> Array.init mxsize identity
      | Some x -> x
  in
  let nr_cols_todo = Array.length cols_todo in
  let col_is_relevant =
    let a = Array.make mxsize false in
    let () = Array.iter (fun c -> a.(c) <- true) cols_todo in
      a
  in
  let lh012_buffer = Array.init 3 (fun _ -> Array.make mxsize 0.0) in
  let p012_buffer = Array.make 3 [||] in
  let (surfaces_and_normals,bdof_solid_angles,vertex_coords) =
    match geom_info with
      | None ->
	  mwe_dof_3d_surface_triangles_and_space_angles_and_coords inside_property
	    mwe (lts,stl)
      | Some x -> x
  in
  let greyfactors_and_displacements_sorted_by_otrans =
    match lattice_info with
      | None -> [|(identity_3d,[|1.0|],[|[|0.0;0.0;0.0|]|])|]
      | Some (v_otrans,v_nr_ot_gf_disp) ->
	  let () =
	    Array.iter
	      (fun (otrans,greyfactor,displacement) ->
		 Printf.printf "GF POS: %s val: %6.4f\n%!" (float_array_to_string displacement) greyfactor
	      )
	      v_nr_ot_gf_disp
	  in
	    Array.mapi
	    (fun nr_otrans otrans ->
	       let relevant_entries = array_filter (fun (n,_,_) -> n=nr_otrans) v_nr_ot_gf_disp in
		 (otrans,
		  Array.map (fun (_,x,_) -> x) v_nr_ot_gf_disp, (* greyfactors *)
		  Array.map (fun (_,_,x) -> x) v_nr_ot_gf_disp  (* displacements *)
		 ))
	    v_otrans
  in
  let leading_triangle_index indices =
    min indices.(0) (min indices.(1) indices.(2))
  in
    (* We have to find out early what this node's workload will be
       so that we can determine when to flush...
    *)
  let relevant_surfaces_and_normals =
    array_filter
      (fun (surface_point_indices,_) -> col_is_relevant.(leading_triangle_index surface_point_indices))
      surfaces_and_normals
  in
  let flush_how_often =
    let nr_relevant = Array.length relevant_surfaces_and_normals in
      Array.init nr_relevant (fun n -> ((n+1)*nr_flushes)/nr_relevant-(n*nr_flushes)/nr_relevant)
  in
  let () = array_foreach_do_n relevant_surfaces_and_normals
    (fun nr_triangle (surface_point_indices, outward_surface_normal) ->
       begin
	 for i=0 to 3-1 do
	   for j=0 to mxsize-1 do
	     lh012_buffer.(i).(j) <- 0.0;
	   done
	 done;
	 Array.iter
	   (fun (otrans,greyfactors,displacements) ->
	      let () = p012_buffer.(0) <- vertex_coords.(surface_point_indices.(0)) in
	      let () = p012_buffer.(1) <- vertex_coords.(surface_point_indices.(1)) in
	      let () = p012_buffer.(2) <- vertex_coords.(surface_point_indices.(2)) in
		nas_lindholm_all_contributions_by_triangle
		  ~min_dist
		  ~ortho_transformation:otrans
		  ~greyfactors
		  ~displacements
		  ~observer_points:vertex_coords
		  ~p012:p012_buffer
		  ~outward_surface_normal
		  ~store_lh012:lh012_buffer ())
	   greyfactors_and_displacements_sorted_by_otrans;
	 for i=0 to 3-1 do
	   for j=0 to mxsize-1 do
	     fun_add_contrib j surface_point_indices.(i) lh012_buffer.(i).(j);
	   done
	 done;
	 for i=0 to flush_how_often.(nr_triangle)-1 do
	   fun_assemble_matrix false (* flush only *)
	 done;
       end
       )
  in
  let () = (* adjusting the diagonal *)
    array_foreach_do cols_todo
      (fun nr_col -> fun_add_contrib nr_col nr_col (bdof_solid_angles.(nr_col)/.(4.0*.pi)-.1.0))
  in
  let () = fun_assemble_matrix true (* final assembly *)
  in ()
;;
