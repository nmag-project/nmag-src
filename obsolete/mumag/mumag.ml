(* (C) 2007 Dr. Thomas Fischbacher *)

(*
  ocamlc -I ../mumag2 -I ../snippets -I ../ccpla -I ../mesh -I ../fem -I ../mpi_petsc -I ../nsim_grammars -I ../nsim -I ../bem3d -i mumag.ml
*)

open Snippets;;
open Fem;;
open Mesh;;
open Nsim;;

type llg_material =
  {
    llg_name: string;
    llg_Ms: float; (* M_sat *)
    llg_J: float; (* J_exch *)
    llg_c1: float;
    llg_c2: float;
    llg_c3: float;
    llg_anisotropy: (float array -> float) option;
    llg_anisotropy_order: int;
  }
;;

(* XXX NOTES:

   (1) In the combined linalg_machine, we may fold the double
   distribution of the m vector for the computation of H_demag and
   H_exch into one single distribution!

   (2) The way how we pass all those mwes is *quite* awkward! Need a better strategy!

*)

let make_mumag_linalg_machine
    ?ccpla ~prefix
    ?(pass_on_prematrices_by_name=Hashtbl.create 1)
    ~mwe_m ~mwe_dm_dt
    ~mwe_H_total ~mwe_H_demag ~mwe_H_exch ~mwe_H_anis ~mwe_H_ext
    (* XXX NOTE: need a better solution for passing all these mwe parameters! *)
    ~inside_regions
    ?(fun_make_and_register_mwe_field=fun (mwe:(float mesh_with_elements)) -> impossible())
    llg_materials =
  let mesh = mwe_m.mwe_mesh in
    (* XXX TODO? check that all MWEs have same mesh?! XXX *)
  let boundary_restriction_phi = Some [|("phi",false,[|Some (-1);None|])|] in
  let () = Printf.printf "DDD make_mumag_linalg_machine #1\n%!" in
  let () = Printf.printf "DDD make_mumag_linalg_machine #1\n%!" in
  let () = mwe_ensure_has_inv_volumes mwe_H_demag in
  let () = mwe_ensure_has_inv_volumes mwe_H_exch in
  let elem_scalar = make_element ~element_name:"H_demag_scalar" 3 ("S",[||]) 1 in
  let mwe_scalar = make_mwe "mwe_scalar" (fun _ -> elem_scalar) mesh in
  let mwe_rho = mwe_sibling "mwe_rho" "rho/S" [|("S","rho")|] mwe_scalar in
  let mwe_phi = mwe_sibling "mwe_phi" "phi/S" [|("S","phi")|] mwe_scalar in
  let chunk_from_pattern ?(separator="") ?(postfix="") pattern  =
    Printf.sprintf "%s%s"
      (string_multifill_template_then_concat ~separator 
	 pattern
	 [|"%MAT%";"%ABS_MAG%";"%negJ%";"%LLG_C1%";"%LLG_C2%";"%LLG_C3%"|]
	 (Array.map (fun llg ->
		       [|llg.llg_name;
			 string_of_float llg.llg_Ms;
			 string_of_float (-.llg.llg_J);
			 string_of_float llg.llg_c1;
			 string_of_float llg.llg_c2;
			 string_of_float llg.llg_c3;
		       |])
	    llg_materials))
      postfix
  in
  let ddiffop_div_m_str = 
    chunk_from_pattern ~separator:"+" ~postfix:",j:3" "(%ABS_MAG%)*<rho || d/dxj m_%MAT%(j)>+(%ABS_MAG%)*<rho || D/Dxj m_%MAT%(j)>" 
  in
  let ddiffop_H_exch_str = 
    chunk_from_pattern ~separator:"+" ~postfix:",j:3,k:3" "(%negJ%)*<d/dxj H_exch_%MAT%(k) || d/dxj m_%MAT%(k)>"
  in
  let () = Printf.printf "DDD make_mumag_linalg_machine #2\n%!" in
  let indices_bdy_phi = 
    array_mapfilter
      (fun dof -> 
	 if (List.tl dof.dof_in_body <> [])
	   &&
	   List.exists (fun bn -> -1 <> array_position bn inside_regions 0) dof.dof_in_body
	 then Some dof.dof_nr else None)
      mwe_phi.mwe_dofs
  in
  let () = Printf.printf "DDD make_mumag_linalg_machine #3\n%!" in
  let script=
    {
     las_mwes=[|mwe_m.mwe_name;
		mwe_H_demag.mwe_name;
		mwe_H_exch.mwe_name;
		mwe_rho.mwe_name;
		mwe_phi.mwe_name;
	      |];
      las_internal_buffers=
	[|("inv_volumes_H_demag",mwe_H_demag.mwe_name,None,true);
	  ("inv_volumes_H_exch",mwe_H_exch.mwe_name,None,true);
	  (* XXX can I actually declare these as fields? They certainly are not
	     co-fields, because then I would have a sensible integral! *)
	  ("m",mwe_m.mwe_name,None,true);
	  ("H_exch",mwe_H_exch.mwe_name,None,true);
	  ("H_anis",mwe_H_anis.mwe_name,None,true);
	  ("H_ext",mwe_H_ext.mwe_name,None,true);
	  ("H_total",mwe_H_total.mwe_name,None,true);
	  (* --- H_demag fields --- *)
	  ("H_demag",mwe_H_demag.mwe_name,None,true);
	  ("rho",mwe_rho.mwe_name,None,false);
	  ("rho_s",mwe_rho.mwe_name,None,false); (* Effective surface charges for rho2 from D.B.C. *)
	  ("phi",mwe_phi.mwe_name,None,true);
	  ("phi1",mwe_phi.mwe_name,None,true);
	  ("phi2",mwe_phi.mwe_name,None,true);
	  ("phi1b",mwe_phi.mwe_name,boundary_restriction_phi,true);
	  ("phi2b",mwe_phi.mwe_name,boundary_restriction_phi,true);
     |];
     las_op_matrices=
     [|
       (* === H_exch === *)
       {loms_name="laplace_m";
	loms_mwe_name_le=mwe_H_exch.mwe_name;
	loms_mwe_name_ri=mwe_m.mwe_name;
	loms_symbolic_operator=ddiffop_H_exch_str;
	loms_matoptions=[||];
       };
       (* === H_demag === *)
       {loms_name              = "div_m";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_m.mwe_name;
	loms_symbolic_operator = ddiffop_div_m_str;
	loms_matoptions=[||];
       };
       {loms_name              = "neg_laplace_phi";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "<d/dxj rho || d/dxj phi>;gauge_fix:phi, j:3";
	loms_matoptions=[|Mpi_petsc.MAT_SYMMETRIC;Mpi_petsc.MAT_SYMMETRY_ETERNAL|];
       };
       {loms_name              = "grad_phi";
	loms_mwe_name_le       = mwe_H_demag.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "-<H_demag(j) || d/dxj phi>, j:3";
	loms_matoptions=[||];
       };
       {loms_name              = "laplace_DBC";
	loms_mwe_name_le       = mwe_phi.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "-<d/dxj phi[vol] || d/dxj phi[vol]>;phi[boundary=-1/*]=phi[boundary=-1/*], j:3";
	(* NOTE: vol is important here! *)
	loms_matoptions=[|Mpi_petsc.MAT_SYMMETRIC;Mpi_petsc.MAT_SYMMETRY_ETERNAL|];
       };
       {loms_name              = "load_DBC";
	loms_mwe_name_le       = mwe_phi.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "<d/dxj phi[vol] || d/dxj phi[boundary=-1/*]>;(L||R)=(*||phi[boundary=-1/*]), j:3";
	loms_matoptions=[||];
       };
     |];
     las_dense_matrices=
	[|
	  {ldms_name = "BEM";
	   ldms_mwe_name = mwe_phi.mwe_name;
	   ldms_dof_name = ("phi",[||]);
	   ldms_inside_regions = inside_regions; (* <- XXX *)
	   ldms_matoptions=[||];
	  };
	|];
     las_ksps=[|
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
     las_swexs=
	[|
	  {
	    lss_name="swex_compute_H_total";
	    lss_c_code=
	      chunk_from_pattern
"if(have_H_total_%MAT%)
   {
     int j;
     for(j=0;j<3;j++)H_total_%MAT%(j)=H_demag(j)+H_exch_%MAT%(j)+H_anis_%MAT%(j)+H_ext(j);
   }
";
	    lss_aux_args=[||];
	    lss_field_mwes=[|mwe_H_total.mwe_name;
			     mwe_H_exch.mwe_name;
			     mwe_H_demag.mwe_name;
			     mwe_H_anis.mwe_name;
			     mwe_H_ext.mwe_name;
			   |];
	    lss_cofield_mwes=[||];
	  };
	  {
	    lss_name="swex_compute_dm_dt";
	    lss_c_code=
	      chunk_from_pattern
"
if(have_m_%MAT%) {
  static double m[3], len_m_sq,mh,len_m_deviation, h[3];
  static double alpha,beta,gamma;
  int i;

  alpha=%LLG_C1%;
  beta=%LLG_C2%;
  gamma=%LLG_C3%;

  for(i=0;i<3;i++)
   {
    m[i]=m_%MAT%(i);
    h[i]=H_total_%MAT%(i);
   }

  len_m_sq=m[0]*m[0]+m[1]*m[1]+m[2]*m[2];
  mh=m[0]*h[0]+m[1]*h[1]+m[2]*h[2];

  len_m_deviation=1.0-len_m_sq;

  dm_dt_%MAT%(0) =
    gamma * m[0]*len_m_deviation
   +alpha * (m[1]*h[2] - m[2]*h[1])
   +beta  * ( m[0]*mh -h[0]*len_m_sq);

  dm_dt_%MAT%(1) =
    gamma * m[1]*len_m_deviation
   +alpha * (m[2]*h[0] - m[0]*h[2])
   +beta  * ( m[1]*mh -h[1]*len_m_sq);

  dm_dt_%MAT%(2) =
    gamma * m[2]*len_m_deviation
   +alpha * (m[0]*h[1] - m[1]*h[0])
   +beta  * ( m[2]*mh -h[2]*len_m_sq);
}
";
	    lss_aux_args=[||];
	    lss_field_mwes=[|mwe_dm_dt.mwe_name;
			     mwe_m.mwe_name;
			     mwe_H_total.mwe_name;
			   |];
	    lss_cofield_mwes=[||];
	  };
	|];
     las_command_sequences=
	[|
	  ("init_inv_volume_factors",
	   [|("arg_inv_volumes_H_demag",mwe_H_demag.mwe_name);
	     ("arg_inv_volumes_H_exch",mwe_H_exch.mwe_name)
	   |],
	   [||],
	   [|
	     LC_vec_distribute("arg_inv_volumes_H_demag","inv_volumes_H_demag");
	     LC_vec_distribute("arg_inv_volumes_H_exch","inv_volumes_H_exch");
	   |]);
	  (* ------ *)
	  ("compute_H_exch",
	   [|("arg_m",mwe_m.mwe_name);
	     ("arg_H_exch",mwe_H_exch.mwe_name);
	   |],
	   [||],
	   [|
	     LC_vec_distribute("arg_m","m");
	     LC_mx_x_pvec("laplace_m","m","H_exch");
	     LC_vec_pointwise_mult("H_exch","inv_volumes","H_exch");
	     LC_vec_collect("arg_H_exch","H_exch");
	   |]);
	  (* ------ *)
	  ("compute_H_demag",
	   [|("arg_m",mwe_m.mwe_name);
	     ("arg_H_demag",mwe_H_demag.mwe_name);
	   |],
	   [||],
	   [|
	     LC_vec_distribute("arg_m","m"); 
	     LC_mx_x_pvec("div_m","m","rho");
	     (* LC_psolve("solve_laplace_phi","rho","phi1"); *)
	     LC_pvec_scale("rho",-1.0);
	     LC_psolve("solve_neg_laplace_phi","rho","phi1");
	     LC_pvec_pull(indices_bdy_phi,"phi1","phi1b");
	     (* LC_debug_printvec_1cpu("phi1-bdofs","phi1b"); *)
	     LC_mx_x_pvec("BEM","phi1b","phi2b");
	     (* LC_debug_printvec_1cpu("phi2-bdofs","phi2b"); *)
	     LC_mx_x_pvec("load_DBC","phi2b","rho_s");
	     (* LC_debug_printvec_1cpu("phi2-effective-rho","rho_s"); *)
	     LC_psolve("solve_laplace_DBC","rho_s","phi2");
	     LC_pvec_push(indices_bdy_phi,"phi2b","phi2");
	     (* LC_debug_printvec_1cpu("phi2-final","phi2"); *)
	     LC_pvec_axpby(1.0,"phi1",0.0,"phi");
	     LC_pvec_axpby(1.0,"phi2",1.0,"phi");
	     (* LC_debug_printvec_1cpu("phi-final","phi"); *)
	     LC_mx_x_pvec("grad_phi","phi","H_demag");
	     (* LC_debug_printvec_1cpu("H_demag-unrescaled","H_demag"); *)
	     (* LC_debug_printvec_1cpu("H_demag-volumes","inv_volumes_H_demag"); *)
	     LC_vec_pointwise_mult("H_demag","inv_volumes_H_demag","H_demag");
	     (* LC_debug_printvec_1cpu("H_demag","H_demag"); *)
	     LC_vec_collect("arg_H_demag","H_demag");
	   |]);
	|]
    }
  in
  let () = Printf.printf "DDD make_mumag_linalg_machine #4\n%!" in
  let relevant_mwes = [|mwe_m;mwe_H_demag;mwe_H_exch;mwe_rho;mwe_phi|] in
  let the_machine = 
    make_linalg_machine
      ?ccpla ~prefix
      ~pass_on_prematrices_by_name
      ~relevant_mwes
      script
  in
  let () = Array.iter (fun mwe -> let _ = fun_make_and_register_mwe_field mwe in ()) relevant_mwes in
    (* ^ XXX This is a bad hack to placify the upper levels: we still have to make and register some fields,
       which can be accessed externally through the magsim_brain. However, with this approach,
       they will be empty. XXX ADJUST!
    *)
  let v_inv_vols_H_demag =
    match !(mwe_H_demag.mwe_dof_funs_inv_volumes) with
      | None -> failwith "make_mumag_linalg_machine: mwe_H_demag needs dof volume information!"
      | Some v ->
	  v
  in
  let v_inv_vols_H_exch =
    match !(mwe_H_exch.mwe_dof_funs_inv_volumes) with
      | None -> failwith "make_mumag_linalg_machine: mwe_H_exch needs dof volume information!"
      | Some v ->
	  v
  in
  let () = Printf.printf "DDD make_mumag_linalg_machine #5\n%!" in
  let () =
    the_machine.execute_on
      "init_inv_volume_factors"
      [|FEM_field(mwe_H_demag,None,v_inv_vols_H_demag);
	FEM_field(mwe_H_demag,None,v_inv_vols_H_exch)
      |]
      [||]
  in
  let () = Printf.printf "DDD make_mumag_linalg_machine #6\n%!" in
    the_machine
;;
