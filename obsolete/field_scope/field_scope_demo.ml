
#use "topfind";;
#require "voodoo";;
#require "unix";;

#thread;;

#require "pycaml";;
#require "lablgtk2";;
#require "lablgtk2.init";;
#require "lablgtk2.gtkgl";;

#require "fem_element";;
#require "mpi_petsc";;
#require "fastfields";;
#require "timestep";;
#require "mumag";;

#use "field_scope.ml";;

let () =
  begin
    register_version ["$Id$";
		      (Snippets.version());
		      (Fem_element.version());
		      (Mesh.version());
		      (Mumag.version());
		      (Timestep.version());];
    register_feature ~domain:"file-cache" "directory" "/tmp/mumag-cache.tf";
    register_feature ~domain:"file-cache" "max-age" "7.0";
  end
;;

Mpi_petsc.petsc_init [|"foo";"-log_info"|] "../mpi_petsc/petscrc" "[help message]";;


let ex_mesh =
  mesh_it
    ~cache_name:"field_scope_example"
    (fem_geometry_from_bodies
       ~mesh_exterior:false
       ~density:(fun x -> let r = sqrt(euclidean_len_sq x) in if r<=2.5 then 1.0 else (r/.2.5)**(-1.8))
       ([|-8.0;-8.0;-8.0|],[|8.0;8.0;8.0|])
       [|Body (body_trafo_id 3, bc_ellipsoid [|2.0;2.0;2.0|])|])
    !opt_mesher_defaults
    2.0;; (* XXX For now, make it fast. *)

let el_m = make_element 3 ("M",[|3|]) 1;;

let mwe_m = mesh_allocate_dof (fun _ -> el_m) ex_mesh;;

let data_m = dof_sample_vector_field_petsc mwe_m (fun c pos -> if c = 0 then 1.0 else 0.0);;

let field_m = FEM_field (mwe_m,data_m);;

let run () = interactive_field_scope [|field_m|];;

run();;

