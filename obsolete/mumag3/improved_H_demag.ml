let make_H_demag_linalg_machine_improved
    (* The improvement will consist of a better handling of surface divergence charges.
       However, before we get there, we first have to make it work the old-fashioned
       way!
    *)
    ?ccpla ~prefix
    ~mwe_m ~mwe_H_demag ~inside_regions
    ?(fun_make_and_register_mwe_field=fun (mwe:(float mesh_with_elements)) -> impossible())
    material_names_and_Ms_and_J =
  let mesh = mwe_m.mwe_mesh in
  let boundary_restriction_phi = Some [|("phi",false,[|Some (-1);None|])|] in
  let () = Printf.printf "DDD make_demag_linalg_machine #1\n%!" in
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
		    "(%f)*<rho || d/dxj m_%s(j)>"
		    m_s mname)
	       material_names_and_Ms_and_J)))
  in
  let ddiffop_Div_m_str = 
    Printf.sprintf "%s, j:3"
      (String.concat"+"
	 (Array.to_list
	    (Array.map
	       (fun (mname, m_s,_) ->
		  Printf.sprintf
		    "(%f)*<rho || D/Dxj m_%s(j)>"
		    m_s mname)
	       material_names_and_Ms_and_J)))
  in
  let () = Printf.printf "DDD make_demag_linalg_machine #2\n%!" in
  let indices_bdy_phi = 
    array_mapfilter
      (fun dof -> 
	 if (List.tl dof.dof_in_body <> [])
	   &&
	   List.exists (fun bn -> -1 <> array_position bn inside_regions 0) dof.dof_in_body
	 then Some dof.dof_nr else None)
      mwe_phi.mwe_dofs
  in
  let () = Printf.printf "DDD make_demag_linalg_machine #3\n%!" in
  let script=
    {
     las_mwes=[|mwe_m.mwe_name;
		mwe_H_demag.mwe_name;
		mwe_rho.mwe_name;
		mwe_phi.mwe_name;
	      |];
      las_internal_buffers=
	[|("inv_volumes_H_demag",mwe_H_demag.mwe_name,None,true);
	  (* XXX can I actually declare this a field? It certainly is not
	     a co-field, because then I would have a sensible integral! *)
	  ("m",mwe_m.mwe_name,None,true);
	  ("H_demag",mwe_H_demag.mwe_name,None,true);
	  ("rho_v",mwe_rho.mwe_name,None,false);
	  ("rho_s",mwe_rho.mwe_name,None,false); (* Effective surface charges for rho2 from D.B.C. *)
	  ("phi",mwe_phi.mwe_name,None,true);
	  ("phi1",mwe_phi.mwe_name,None,true);
	  ("phi2",mwe_phi.mwe_name,None,true);
	  ("phi1b",mwe_phi.mwe_name,boundary_restriction_phi,true);
	  ("phi2b",mwe_phi.mwe_name,boundary_restriction_phi,true);
     |];
     las_op_matrices=
     [|
       {loms_name              = "div_m";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_m.mwe_name;
	loms_symbolic_operator = ddiffop_div_m_str;
	loms_matoptions=[||];
       };
       (* Surface charges: *)
       {loms_name              = "Div_m";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_m.mwe_name;
	loms_symbolic_operator = ddiffop_Div_m_str;
	loms_matoptions=[||];
       };
       {loms_name              = "laplace_vol";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "-<d/dxj rho || d/dxj phi>;gauge_fix:phi, j:3";
	loms_matoptions=[||];
       };
       {loms_name              = "laplace_surface";
	loms_mwe_name_le       = mwe_rho.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "-<d/dxj rho[vol] || d/dxj phi>-<D/Dxj rho[boundary] || d/dxj phi>;gauge_fix:phi, j:3";
	loms_matoptions=[||];
       };
       {loms_name              = "grad_phi";
	loms_mwe_name_le       = mwe_H_demag.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "<H(j) || d/dxj phi>, j:3";
	loms_matoptions=[||];
       };
       {loms_name              = "laplace_DBC";
	loms_mwe_name_le       = mwe_phi.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	loms_symbolic_operator = "-<d/dxj phi || d/dxj phi>;phi[boundary=-1/*]=phi[boundary=-1/*], j:3";
	loms_matoptions=[||];
       };
       {loms_name              = "load_DBC";
	loms_mwe_name_le       = mwe_phi.mwe_name;
	loms_mwe_name_ri       = mwe_phi.mwe_name;
	(* loms_symbolic_operator = "<d/dxj phi || d/dxj phi[boundary=-1/*]>;(L||R)=(*||phi[boundary=-1/*]), j:3"; *)
	(* loms_symbolic_operator = "<d/dxj phi[vol=1] || d/dxj phi[boundary=-1/*]>;(L||R)=(*||phi[boundary=-1/*]), j:3"; *)
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
       {lks_name = "solve_laplace_vol";
	lks_matrix_name = "laplace_vol";
	lks_precond_name = "laplace_vol";
	lks_ksp_type=None;
	lks_pc_type=None;
	lks_initial_guess_nonzero=None;
	lks_rtol=None;
	lks_atol=None;
	lks_dtol=None;
	lks_maxits=None;
       };
       {lks_name = "solve_laplace_surface";
	lks_matrix_name = "laplace_surface";
	lks_precond_name = "laplace_surface";
	lks_ksp_type=None;
	lks_pc_type=None;
	lks_initial_guess_nonzero=None;
	lks_rtol=None;
	lks_atol=None;
	lks_dtol=None;
	lks_maxits=None;
       };
       {lks_name = "solve_laplace_DBC";
	lks_matrix_name = "laplace_DBC";
	lks_precond_name = "laplace_DBC";
	lks_ksp_type=None;
	lks_pc_type=None;
	lks_initial_guess_nonzero=None;
	lks_rtol=None;
	lks_atol=None;
	lks_dtol=None;
	lks_maxits=None;
       };
     |];
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
	     LC_mx_x_pvec("div_m","m","rho_v");
	     LC_mx_x_pvec("Div_m","m","rho_s");
	     LC_debug_printvec_1cpu("rho[volume]","rho_v");
	     LC_debug_printvec_1cpu("rho[surface]","rho_s");
	     LC_psolve("solve_laplace_vol","rho_v","phi1");
	     LC_debug_printvec_1cpu("phi[vol-charges]","phi1");
	     LC_psolve("solve_laplace_surface","rho_s","phi2");
	     LC_debug_printvec_1cpu("phi[surf-charges]","phi2");
	     LC_pvec_axpby(1.0,"phi2",1.0,"phi1");
	     LC_debug_printvec_1cpu("phi[total]","phi1");
	     (* phi1 now contains the total surface potential *)
	     LC_pvec_pull(indices_bdy_phi,"phi1","phi1b");
	     LC_debug_printvec_1cpu("phi1-bdofs","phi1b");
	     LC_mx_x_pvec("BEM","phi1b","phi2b");
	     LC_debug_printvec_1cpu("phi2-bdofs","phi2b");
	     LC_mx_x_pvec("load_DBC","phi2b","rho_s");
	     LC_debug_printvec_1cpu("phi2-effective-rho","rho_s");
	     LC_psolve("solve_laplace_DBC","rho_s","phi2");
	     LC_pvec_push(indices_bdy_phi,"phi2b","phi2");
	     LC_debug_printvec_1cpu("phi2-final","phi2");
	     LC_pvec_axpby(1.0,"phi1",0.0,"phi");
	     LC_pvec_axpby(1.0,"phi2",1.0,"phi");
	     LC_debug_printvec_1cpu("phi-final","phi");
	     LC_mx_x_pvec("grad_phi","phi","H_demag");
	     LC_vec_pointwise_mult("H_demag","inv_volumes_H_demag","H_demag");
	     LC_vec_collect("arg_H_demag","H_demag");
	   |]);
	|]
    }
  in
  let () = Printf.printf "DDD make_demag_linalg_machine #4\n%!" in
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
  let () = Printf.printf "DDD make_demag_linalg_machine #5\n%!" in
  let () =
    the_machine.execute_on
      "init_inv_volume_factors"
      [|FEM_field(mwe_H_demag,None,v_inv_vols)|] [||]
  in
  let () = Printf.printf "DDD make_demag_linalg_machine #6\n%!" in
    the_machine
;;
