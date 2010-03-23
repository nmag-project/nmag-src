(*
   (C) 2009 Dr. Thomas Fischbacher

   ocamlc -I ../pycaml -I ../mpi_petsc -I ../config -I ../snippets -I ../mpi_petsc -I ../ddpla -I ../pyddpla  -I ../mesh -I ../fem -i nsim_ddpla.ml 

 *)

open Pycaml;;
open Ddpla;;
open Snippets;;

(* We run into an awkward problem when we try to construct
   meshes-with-elements.

   Basically, as their construction involves non-serializable
   parameters (python function to specify boundary conditions),
   we have to construct them on the master node first, and then
   send info how to re-construct them over the network to the slaves.
   
   XXX Alternative Idea XXX

   MWEs are fairly expensive data structures - we would like to be
   able to "deadhead" them anyway, after we used them, and have them
   re-built on demand just as they are needed. We can then use the
   "deadheaded" version to be sent over the net.

   What all this really tries to tell us is: the mwe data structure
   does need some serious attention...
 *)

type mwe_tag = MWE_Tag of string;;

module HashedMWETag =
  struct
    type t = mwe_tag (* name *)
    let equal x y = (x=y)
    let hash (MWE_Tag x) =
      let len = String.length x in
      let q3 = String.make 3 '\000' in
      let rec walk pos =
        if pos=len then
          ((Char.code q3.[0]) lsl 24)
            + ((Char.code q3.[1]) lsl 24)
            + ((Char.code q3.[2]) lsl 24)
        else
          let n = pos mod 3 in
          let old_val = Char.code q3.[n] in
          let () = q3.[n] <- Char.chr (Char.code x.[pos] lxor old_val) in
          walk (pos+1)
      in walk 0
  end
;;

module WeakMWE = Weak.Make(HashedMWETag);;



let sym_simplicial_mesh="DDPLA: Nsim Simplicial Mesh";;
let sym_proto_mwe="DDPLA: Nsim proto Mesh with Elements";;
let sym_mwe="DDPLA: Nsim Mesh with Elements";;

(* Building a mesh with elements in parallel is quite tricky, as the
   build is parametrized by data which we cannot send over the network
   (such as a Python function specifying boundary types).
   
   The way we resolve this conundrum is that we first build the mwe
   on the master node, then construct a "proto_mwe" from this,
   which can be "upgraded" into a proper mwe in a process that is
   just extraction on the master node, and send-and-rebuild on
   slave nodes. But a problem remains: we have to stick the proto_mwe
   into the opcode sequence in such a way that the mwe part does not
   get serialized.

   Essentially, the core problem here is: Master and slaves need
   different opcodes.

 *)

type proto_mwe =
    ((float mwe_made_by option ref)
       * (float mesh_with_elements option ref))
      

let () = register_ocamlpill_types
    [|sym_simplicial_mesh;
      sym_proto_mwe;
      sym_mwe
    |]
;;

let (ocamlpill_from_mesh, mesh_from_ocamlpill) =
    make_ocamlpill_wrapper_unwrapper
    sym_simplicial_mesh
    Mesh.dummy_mesh
;;


let mwe_spec_from_python dim py_li_rdescs =
  let ddd =
    Printf.fprintf stderr
      "XXX TODO: mwe_spec_from_python - when dealing with fun_outer_region,
       we cannot easily move that Python function across the net.
       So, we need another strategy to get that sorted out.
       Essentially, we have three possible options:
       (a) specify what py-module to import that function from.
       (b) use fastfields
       (c) build a mwe on master first and use mwemb mechanism.
     "
  in
  let ht_props_by_region = Hashtbl.create 17 in
  let ht_elem_by_region = Hashtbl.create 17 in
  let fail () =
    failwith "fem_elements_and_properties_from_region_desc: bad region desc!"
  in
  let s_nr=pystring_fromstring "NR" in
  let s_tensors=pystring_fromstring "TENSORS" in
  let s_properties=pystring_fromstring "PROPERTIES" in
  let () = 
    array_foreach_do
      (guarded_pylist_toarray py_li_rdescs)
      (fun rdesc ->
	let () = (if pytype rdesc <> DictType then fail () else ()) in
	let region_nr = guarded_pyint_asint (pydict_getitem(rdesc,s_nr)) in
	let tensors = 
	  Array.map
	    (fun py_tspec ->
	      let py_name_order_ranges = guarded_pytuple_toarray py_tspec in
	      let name = guarded_pystring_asstring py_name_order_ranges.(0) in
	      let order = guarded_pyint_asint py_name_order_ranges.(1) in
	      let ranges = 
		Array.map guarded_pyint_asint
		  (Array.sub py_name_order_ranges 2 (Array.length py_name_order_ranges-1))
	      in
	      (name,order,ranges)
	    )
	    (guarded_pylist_toarray (pydict_getitem(rdesc,s_tensors)))
	in
	let properties =
	  Array.map guarded_pystring_asstring
	    (guarded_pylist_toarray (pydict_getitem(rdesc,s_properties)))
	in
	let () = Hashtbl.replace ht_props_by_region region_nr properties in
	let elem =
	  Array.fold_left
	    (fun elem_sf (name,order,index_ranges) ->
	      let elem = make_element dim (name,index_ranges) order in
	      fuse_elements elem_sf elem)
	    empty_element tensors
	in
	Hashtbl.replace ht_elem_by_region region_nr elem
      )
  in
  let regions = hashtbl_keys ht_props_by_region in
  let result = 
    Array.map
      (fun nr_region ->
	let props = Hashtbl.get ht_props_by_region nr_region in
	let elem = Hashtbl.get ht_elem_by_region nr_region in
	(nr_region,props,elem))
      regions
  in result
;;


let nsim_ddpla_add_opcodes ddpla = 
    begin
    (* -------------------- *)
    (Printf.printf "DDD Defining nsim_ddpla extension opcodes!\n%!");
    _def_opcode
      ~ddpla
      ~opcode:"CREATE-MESH"
      ~args:[|":NAME";
	      ":FILENAME-ON-MASTER";
	      ":RE-ORDER";
	    |]
      (* Note: We bend over backwards to serialize a mesh (after loading) to a number
	 of Bigarrays and MPI-transport these manually. Reason: We found that
	 OCaml's Marshal module seems to be broken and does not handle some
	 large data well on x86-32.
       *)
      (fun ~report_error () ->
	fun args ->
	  let name_mesh =
	    oa_string ~report_error ~opcode_name:"CREATE-MESH" ~param_name:":NAME" args.(0)
	  in
	  let filename_mesh =
	    oa_string ~report_error ~opcode_name:"CREATE-MESH" ~param_name:":FILENAME-ON-MASTER" args.(1)
	  in
	  let do_reorder =
	    0 <> oa_int ~report_error ~opcode_name:"CREATE-MESH" ~param_name:":RE-ORDER" args.(2)
	      (* XXX note that oa_bool does not work as expected - for now, I hence use int. *)
	  in
	  let myrank = Mpi_petsc.comm_rank ddpla.ddpla_comm in
	  let mysize = Mpi_petsc.comm_size ddpla.ddpla_comm in
	  let mesh = 
	    if myrank = 0
	    then
	      (* This is the master node! *)
	      let ddd = Printf.fprintf stderr "DDD TODO: CREATE-MESH opcode must handle missing mesh files properly!\n%!" in
	      let mesh = 
		match Mesh.read_mesh filename_mesh 
		with
		| None -> 
		    let s = report_error ~level:"ERROR" 
			(Printf.sprintf "Could not load mesh '%s'" filename_mesh)
		    in failwith s
		      (* XXX this is bad - not being able to load a mesh could
			 MPI-desynchronize the whole thing!
			 TODO: Safeguard against that!
			 Essentially, MPI communication scheme should be extended
			 with a first package that gets broadcast, which says: yes,
			 we DO have a valid mesh!
		       *)
		| Some x -> x
	      in
	      let () = 
		(if do_reorder 
		then 
		  let do_reorder_mesh mesh =
		    let connectivity = Mesh.mesh_connectivity mesh in
		    let reordering = Mpi_petsc.proximity_improving_reordering connectivity in
		    let () = Mesh.mesh_do_reorder_vertices mesh (fun n -> reordering.(n)) in
		    ()
		  in
		  let points_coords = Array.map (fun p -> p.Mesh.mp_coords) mesh.Mesh.mm_points in
		  (* extract coordinates of periodic points to update mesh.mm_periodic
		     after the reordering *)
		  let periodic_points =
		    Array.map
		      (fun per_ix_arr -> Array.map (fun per_ix -> points_coords.(per_ix)) per_ix_arr  )
		      mesh.Mesh.mm_periodic_points
		  in
		  let () = do_reorder_mesh mesh in
		  let new_points_coords = Array.map (fun p -> p.Mesh.mp_coords) mesh.Mesh.mm_points in
		  mesh.Mesh.mm_periodic_points <- Array.map
		      ( fun per_arr ->
			Array.map (fun p -> array_position p new_points_coords 0 ) per_arr)
		      periodic_points
		else ())
	      in
	      let () =
		(
		 if mysize = 1
		     (* No need to distribute mesh if there is just one node! *)
       		 then ()
		 else
		   let mpi_geom = Mesh.mesh_to_mpi_meshgeom mesh in
		   let dim = mpi_geom.Mesh.mmg_dim in
		   let layout =
		     (mpi_geom.Mesh.mmg_dim,
		      Bigarray.Array1.dim mpi_geom.Mesh.mmg_vertex_coords,
		      Bigarray.Array1.dim mpi_geom.Mesh.mmg_simplices,
		      Bigarray.Array1.dim mpi_geom.Mesh.mmg_simplex_regions)
		   in
		   let () =
		     begin
		       ignore(Mpi_petsc.broadcast layout 0 ddpla.ddpla_comm);
		       Mpi_petsc.broadcast_bigarray_float mpi_geom.Mesh.mmg_vertex_coords 0 ddpla.ddpla_comm;
		       Mpi_petsc.broadcast_bigarray_nativeint mpi_geom.Mesh.mmg_simplices 0 ddpla.ddpla_comm;
		       Mpi_petsc.broadcast_bigarray_nativeint mpi_geom.Mesh.mmg_simplex_regions 0 ddpla.ddpla_comm;
		       ignore(Mpi_petsc.broadcast mpi_geom.Mesh.mmg_periodicity 0 ddpla.ddpla_comm);
		       ignore(Mpi_petsc.broadcast mpi_geom.Mesh.mmg_vertex_distribution 0 ddpla.ddpla_comm);
		     end
		   in
		   ())
	      in mesh
	    else
	      (* slave node *)
	      (* We first have to fetch the mesh over the network... *)
	      let (dim,len_vertex_coords,len_simplices,len_regions) =
		Mpi_petsc.broadcast (-1,-1,-1,-1) 0 ddpla.ddpla_comm
	      in
	      let ba_vertex_coords =
		Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout len_vertex_coords
	      in
	      let ba_simplices =
		Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout len_simplices
	      in
	      let ba_regions =
		Bigarray.Array1.create Bigarray.nativeint Bigarray.c_layout len_regions
	      in
	      let () =
		begin
		  Mpi_petsc.broadcast_bigarray_float ba_vertex_coords 0 ddpla.ddpla_comm;
		  Mpi_petsc.broadcast_bigarray_nativeint ba_simplices 0 ddpla.ddpla_comm;
		  Mpi_petsc.broadcast_bigarray_nativeint ba_regions 0 ddpla.ddpla_comm;
		end
	      in
	      let periodicity = Mpi_petsc.broadcast [||] 0 ddpla.ddpla_comm in
	      let vdist = Mpi_petsc.broadcast [||] 0 ddpla.ddpla_comm in
	      let mesh = 
		Mesh.mpi_meshgeom_to_mesh
		  {
		   Mesh.mmg_dim=dim;
		   Mesh.mmg_vertex_coords=ba_vertex_coords;
		   Mesh.mmg_simplices=ba_simplices;
		   Mesh.mmg_simplex_regions=ba_regions;
		   Mesh.mmg_periodicity=periodicity;
		   Mesh.mmg_vertex_distribution=vdist;
		 }
	      in
	      mesh
	  in
	  let dres = {
	    dres_name=name_mesh;
	    dres_rebuild=[||];
            (* Rebuilding a mesh always is a no-op. *)
	    dres_destroy=(fun dres -> Printf.fprintf stderr "DDD Destroying Mesh '%s'\n%!" name_mesh);
	    dres_obj=Some (DRO_data ("MESH", Obj.magic mesh));
	    dres_dirtiness=false;
	    dres_prerequisites=[];
	    dres_dependents=[];
	  }
	  in
	  let _ =
	    ddpla_register_new
	      ~drh:(DRH (name_mesh,ref None))
	      ddpla
	      ~prerequisites:[||]
	      dres
	  in
	  ()
      );
    end
;;

let _ddpla_drh_mesh =
  python_pre_interfaced_function
    ~doc:"Extract the mesh from a DRH"
    [|CamlpillType
    |]
    (fun args ->
      let ddpla = Pyddpla.init() in
      let drh = Pyddpla.drh_from_ocamlpill args.(0) in
      let mesh = Obj.magic (get_data ddpla "MESH" drh) in
      ocamlpill_from_mesh mesh)
;;

let () =
  begin
    nsim_ddpla_add_opcodes (Pyddpla.init());
    (* -- *)
    register_pre_functions_for_python
      [|
	(* This function may come handy INSIDE some Python-defined op-codes
	   that get passed a DRH and have to do something with the actual
	   mesh.
	 *)
	("ddpla_drh_mesh",_ddpla_drh_mesh);
      |]
  end
;;
