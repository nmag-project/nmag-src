(* (C) 2006 Dr. Thomas Fischbacher
   Debugging code to quickly plot a 3d mesh from within OCaml

   XXX WORK IN PROGRESS

   PROBLEMS:

   [] The program crashes unless we resize the window immediately after start.
     I think this is a lablgtk bug, so there is not much we can do about
     it now. (Does not occur now as we switched to lablgl + togl variant! XXX just reverting that switch!)

   * Camera orientation configuration is buggy.

   * menu widgets are somewhat broken.

   * The method used to work through pending widget set events
     when the main window
     is closed is haphazard and stupid!

   * We get a SEGV if we re-open the GUI and close it for a second time.
     (But we can have two GUIs open!)

TODO: vertigo, field selection

 *)

(* Note: we include LOTS of stuff here. Linker-wise, this is a Monster Application(TM) *)

open Snippets;;
open Fem_element;;
open Mesh;;
open Timestep;;
open Mumag;;
open Pycaml;;

let version () = Snippets.md5
    (String.concat ":" ["$Id$";
			(Snippets.version());
			(Fem_element.version());
			(Mesh.version());
			(Mumag.version());
			(Timestep.version());])
;;


type fs_camera_info =
    {
     fci_pos: float array;
     fci_ori: float array;
     mutable fci_zoom: float;
     (* For some options, we just use strings
	and not enum datatypes, as this is less hassle - 
	especially if it comes to mapping to strings!
      *)
     mutable fci_mode: string;
     mutable fci_wander: string;
 }
;;

let copy_fs_camera_info z =
  {fci_pos=Array.copy z.fci_pos;
   fci_ori=Array.copy z.fci_ori;
   fci_zoom=z.fci_zoom;
   fci_mode=z.fci_mode;
   fci_wander=z.fci_wander;
 }
;;
   

type fs_probe_info =
    {
     fpi_pos: float array;
     mutable fpi_color1: int;
     mutable fpi_color2: int;
     mutable fpi_field: string;
     mutable fpi_mode: string;
   }
;;

let copy_fs_probe_info z =
  {fpi_pos=Array.copy z.fpi_pos;
   fpi_color1=z.fpi_color1;
   fpi_color2=z.fpi_color2;
   fpi_field=z.fpi_field;
   fpi_mode=z.fpi_mode;
 }
;;


let opt_default_camera =
  ref
    {fci_pos=[|0.0;-20.0;0.0|];
     fci_ori=[|90.0;0.0;0.0|];
     fci_zoom=1.0;
     fci_mode="wireframe";
     fci_wander="off";
 }
;;

let opt_default_probe =
  ref
    {
     fpi_pos=[|0.0;0.0;0.0|];
     fpi_color1= 0xff0000;
     fpi_color2= 0x00ff00;
     fpi_field="M [0]";
     fpi_mode="Inactive";
   }
;;

type fs_state =
    {
     mutable fss_active_camera: int;
     mutable fss_active_probe: int;
     fss_cameras: fs_camera_info array;
     fss_probes: fs_probe_info array;
   }
;;

let gl_init () =
  let () = Printf.printf "GL Init!\n%!" in
  let light_ambient = 0.5, 0.5, 0.5, 1.0
  and light_diffuse = 1.0, 1.0, 1.0, 1.0
  and light_specular = 1.0, 1.0, 1.0, 1.0
  (*  light_position is NOT default value	*)
  and light_position0 = 20.0, -40.0, 100.0, 0.0
  and light_position1 = -40.0, -60.0, 20.0, 0.0
    in
      begin
	List.iter (GlLight.light ~num:0)
	  [ `ambient light_ambient; `diffuse light_diffuse;
	    `specular light_specular; `position light_position0 ];
	List.iter (GlLight.light ~num:1)
	  [ `ambient light_ambient; `diffuse light_diffuse;
	    `specular light_specular; `position light_position1 ];
	GlFunc.depth_func `less;
	List.iter Gl.enable [`lighting; `light0; `light1; `depth_test;`color_material];
	GlDraw.shade_model `smooth;
      end
;;

let gl_reshape ~width:w ~height:h =
  begin
    Printf.printf "GL RESHAPE!\n%!";
    GlDraw.viewport ~x:0 ~y:0 ~w ~h;
    GlMat.mode `projection;
    GlMat.load_identity();
    GluMat.perspective ~fovy:60.0 ~aspect:(float w /. float h) ~z:(1.0,20.0);
    GlMat.mode `modelview;
    GlMat.load_identity();
    GlMat.translate ~z:(-5.0) ()
  end
;;

let hexstring_to_int str =
  let len = String.length str in
  let rec walk sum pos =
    if pos = len then sum
    else
      let c = str.[pos] in
      if      c = ' '
      then walk sum (1+pos)
      else if c >= '0' && c <= '9'
      then
	let z = (Char.code c) - (Char.code '0') in
	walk (sum*16+z) (1+pos)
      else if c >= 'a' && c <= 'f'
      then
	let z = (Char.code c) - (Char.code 'a') + 10 in
	walk (sum*16+z) (1+pos)
      else if c >= 'A' && c <= 'F'
      then
	let z = (Char.code c) - (Char.code 'A') + 10 in
	walk (sum*16+z) (1+pos)
      else failwith "Bad hex string"
  in walk 0 0
;;
	  

let draw_octahedron color ?(scale=1.0) v123 base_point =
  let vertex x y z = GlDraw.vertex ~x:x ~y:y ~z:z () in
  let normal xyz = GlDraw.normal ~x:xyz.(0) ~y:xyz.(1) ~z:xyz.(2) () in
  begin
    GlDraw.color (color.(0),color.(1),color.(2));
    GlDraw.begins `triangle_strip;
    vertex
      (base_point.(0)+.scale*.v123.(2).(0))
      (base_point.(1)+.scale*.v123.(2).(1))
      (base_point.(2)+.scale*.v123.(2).(2));
    normal v123.(2);
    vertex
      (base_point.(0)-.scale*.v123.(1).(0))
      (base_point.(1)-.scale*.v123.(1).(1))
      (base_point.(2)-.scale*.v123.(1).(2));
    normal v123.(1);
    vertex
      (base_point.(0)-.scale*.v123.(0).(0))
      (base_point.(1)-.scale*.v123.(0).(1))
      (base_point.(2)-.scale*.v123.(0).(2)); (* face --+ *)
    normal v123.(0);
    vertex
      (base_point.(0)-.scale*.v123.(2).(0))
      (base_point.(1)-.scale*.v123.(2).(1))
      (base_point.(2)-.scale*.v123.(2).(2)); (* face --- *)
    normal v123.(2);
    vertex
      (base_point.(0)+.scale*.v123.(1).(0))
      (base_point.(1)+.scale*.v123.(1).(1))
      (base_point.(2)+.scale*.v123.(1).(2)); (* face -+- *)
    normal v123.(1);
    vertex
      (base_point.(0)+.scale*.v123.(0).(0))
      (base_point.(1)+.scale*.v123.(0).(1))
      (base_point.(2)+.scale*.v123.(0).(2)); (* face ++- *)
    normal v123.(0);
    vertex
      (base_point.(0)+.scale*.v123.(2).(0))
      (base_point.(1)+.scale*.v123.(2).(1))
      (base_point.(2)+.scale*.v123.(2).(2)); (* face +++ *)
    normal v123.(2);
    vertex
      (base_point.(0)-.scale*.v123.(1).(0))
      (base_point.(1)-.scale*.v123.(1).(1))
      (base_point.(2)-.scale*.v123.(1).(2)); (* face ++- *)
    normal v123.(1);
    vertex
      (base_point.(0)+.scale*.v123.(0).(0))
      (base_point.(1)+.scale*.v123.(0).(1))
      (base_point.(2)+.scale*.v123.(0).(2)); (* face +-+ *)
    normal v123.(0);
    vertex
      (base_point.(0)-.scale*.v123.(2).(0))
      (base_point.(1)-.scale*.v123.(2).(1))
      (base_point.(2)-.scale*.v123.(2).(2)); (* face +-- *)
    normal v123.(2);
    GlDraw.ends ();
  end
;;


let draw_lines color points links =
  let vertex x y z = GlDraw.vertex ~x:x ~y:y ~z:z () in
  begin
    GlDraw.color (color.(0),color.(1),color.(2));
    GlDraw.begins `lines;
    Array.iter
      (fun (ix1,ix2) ->
	 let p1 = points.(ix1)
	 and p2 = points.(ix2)
	 in
	   begin
	     vertex p1.(0) p1.(1) p1.(2);
	     vertex p2.(0) p2.(1) p2.(2);
	   end)
      links;
    GlDraw.ends ();
  end
;;

let draw_arrow  =
  let dir1 = [|1.0;0.0;0.0|]
  and dir2 = [|0.0;1.0;0.0|]
  in
    fun color base_point target_point ->
      let vertex x y z = GlDraw.vertex ~x:x ~y:y ~z:z () in
      let normal xyz = GlDraw.normal ~x:xyz.(0) ~y:xyz.(1) ~z:xyz.(2) () in
      let dir = array_pointwise (-.) target_point base_point in
      let len = sqrt(euclidean_len_sq dir) in
	if len = 0.0 then ()
	else
	  let n_x = Array.map (fun x -> x/.len) dir in
	  let n_y =
	    let v = cross_product_3d dir1 n_x in
	      if euclidean_len_sq v < 0.01 then cross_product_3d dir2 n_x else v
	  in
	  let n_y = let len = sqrt(euclidean_len_sq n_y) in Array.map (fun x -> x/.len) n_y in
	  let n_z = cross_product_3d n_x n_y in
	  let mn_y = Array.map (fun x -> -.x) n_y in
	  let mn_z = Array.map (fun x -> -.x) n_z in
	  let r_stem=len*.0.05 in
	  let r_hat=r_stem*.1.5 in
	  let len_stem=0.9 in
	    begin
	      GlDraw.color (color.(0),color.(1),color.(2));
	      GlDraw.begins `quad_strip; (* The stem *)
	      normal n_y;
	      vertex
		(base_point.(0)+.r_stem*.n_y.(0))
		(base_point.(1)+.r_stem*.n_y.(1))
		(base_point.(2)+.r_stem*.n_y.(2));
	      normal n_y;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_stem*.n_y.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_stem*.n_y.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_stem*.n_y.(2));
	      normal n_z;
	      vertex
		(base_point.(0)+.r_stem*.n_z.(0))
		(base_point.(1)+.r_stem*.n_z.(1))
		(base_point.(2)+.r_stem*.n_z.(2));
	      normal n_z;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_stem*.n_z.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_stem*.n_z.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_stem*.n_z.(2));
	      normal mn_y;
	      vertex
		(base_point.(0)+.r_stem*.mn_y.(0))
		(base_point.(1)+.r_stem*.mn_y.(1))
		(base_point.(2)+.r_stem*.mn_y.(2));
	      normal mn_y;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_stem*.mn_y.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_stem*.mn_y.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_stem*.mn_y.(2));
	      normal mn_z;
	      vertex
		(base_point.(0)+.r_stem*.mn_z.(0))
		(base_point.(1)+.r_stem*.mn_z.(1))
		(base_point.(2)+.r_stem*.mn_z.(2));
	      normal mn_z;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_stem*.mn_z.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_stem*.mn_z.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_stem*.mn_z.(2));
	      normal n_y;
	      vertex
		(base_point.(0)+.r_stem*.n_y.(0))
		(base_point.(1)+.r_stem*.n_y.(1))
		(base_point.(2)+.r_stem*.n_y.(2));
	      normal n_y;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_stem*.n_y.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_stem*.n_y.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_stem*.n_y.(2));
	      GlDraw.ends ();
	      GlDraw.begins `triangle_fan; (* The hat *)
	      normal n_x;
	      vertex target_point.(0) target_point.(1) target_point.(2);
	      normal n_y;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_hat*.n_y.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_hat*.n_y.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_hat*.n_y.(2));
	      normal n_z;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_hat*.n_z.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_hat*.n_z.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_hat*.n_z.(2));
	      normal mn_y;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_hat*.mn_y.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_hat*.mn_y.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_hat*.mn_y.(2));
	      normal mn_z;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_hat*.mn_z.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_hat*.mn_z.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_hat*.mn_z.(2));
	      normal n_y;
	      vertex
		(base_point.(0)+.len_stem*.dir.(0)+.r_hat*.n_y.(0))
		(base_point.(1)+.len_stem*.dir.(1)+.r_hat*.n_y.(1))
		(base_point.(2)+.len_stem*.dir.(2)+.r_hat*.n_y.(2));
	      GlDraw.ends ();
	    end
;;

(* The code below is quite a monolithic mess, beacuse
   the GUI is hard-coded. Rather, a GUI should be abstracted into
   some kind of tag-based language. (GUI builders are the wrong route:
   rather than having code that writes GUI code, we need a more abstract
   way to specify GUIs. HTML is a small step in the right direction there.)
*)

let interactive_field_scope ?(title="Field Scope") ?(nr_cameras=5) ?(nr_probes=5) fields =
  let rx_hexcolor = Str.regexp "^ *[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F] *$" in (* XXX improve! *)
  let fs_state =
    {fss_active_camera=0;
     fss_active_probe=0;
     fss_cameras = Array.init nr_cameras (fun _ -> copy_fs_camera_info !opt_default_camera);
     fss_probes = Array.init nr_probes (fun _ -> copy_fs_probe_info !opt_default_probe);
   }
  in
  let (mesh, field_vertex_indices) =
    (* XXX Note: this is quite hackish and ad-hoc,
       tailored for plotting M fields only for now!

       XXX Note also that we should directly re-read the field
       from the ML vector for interactive plotting!
    *)
    if Array.length fields = 0
    then failwith "Need at least one field!"
    else
      let (FEM_field (mwe,_)) = fields.(0) in
      let nr_m = Array.length mwe.mwe_mesh.mm_points in
      let fvi = (* XXX ignore higher order as well... *)
	Array.init nr_m (fun _ -> Array.make 3 0)
      in
      let () =
	Array.iter
	  (fun dof ->
	     match dof.dof_site with
	       | [|n|] ->
		   (match dof.dof_name with
		      | (_,[|ix|]) ->
			  fvi.(n).(ix) <- dof.dof_nr
		      | _ -> ())
	       | _ -> ())
	  mwe.mwe_dofs
      in
	(mwe.mwe_mesh,fvi)
  in
  let (plotinfo_coords,plotinfo_links,plotinfo_simplex_info,_)=mesh_plotinfo mesh in
  let r_set_camera_box = ref []
  and r_set_probe_box = ref []
  in
  let w = GWindow.window ~title:title () in
  let quit_thread () =
    begin
      Printf.fprintf stderr "Destroying Interactive Window!\n%!"; (* DDD *)
      w#destroy();
      (* Do a few more event iterations to make sure the widget gets destroyed.
	 XXX Note: this is quite ad-hoc. Should work in any practical case,
	 but there is no guarantee. We should rather check for pending events!
d      *)
      for i=0 to 20-1 do
	Voodoo.gtk_main_iteration_do false;
      done;
      (* GMain.Main.quit (); -- no, we do not quit GTK! *)
      Thread.exit ();
      (* XXX Note that if I create/destroy repeatedly, I may still get segfaults! *)
    end
  in
  let _ = w#connect#destroy ~callback:quit_thread in
  let _ = w#set_resize_mode `IMMEDIATE in
  let vbox_window = GPack.vbox ~packing:w#add () in
  let table_master = GPack.table ~rows:4 ~columns:2 ~show:false () in
  let _ = GObj.pack_return table_master (Some (vbox_window#add)) None in
  let hbox_graphics = GPack.hbox ~packing:vbox_window#add () in
  let area = GlGtk.area [`DOUBLEBUFFER;`RGBA;`DEPTH_SIZE 1]
    ~width:550 ~height:550 ~packing:hbox_graphics#add () in
  let _ = area#misc#set_can_focus true in
  let display () =
    let camera = fs_state.fss_cameras.(fs_state.fss_active_camera) in
    let now = Unix.gettimeofday() in
    let wander_factor speed = 
      match camera.fci_wander
      with
	| "off" -> 0.0
	| "slow" -> sin(now*.speed*.0.2)
	| "fast" -> sin(now*.speed)
	| _ -> impossible()
    in
    let wf1 = wander_factor 1.2
    and wf2 = wander_factor 1.5
    and wf3 = wander_factor 1.7
    and wf4 = wander_factor 2.2
    in
    let ori0 = deg_rad (camera.fci_ori.(0) +. 0.7*.wf1 +. 0.7*.wf4)
    and ori1 = deg_rad (camera.fci_ori.(1) +. 0.25*.wf2 +. 0.5*.wf4)
    and ori2 = deg_rad (camera.fci_ori.(2) +. 0.15*.wf1 +. 0.1*.wf3)
    in
    let sh = sin ori0
    and ch = cos ori0
    and sy = sin ori1
    and cy = cos ori1
    and sr = sin ori2
    and cr = cos ori2
    in
    let look_direction_forward =[|ch*.cy;sh*.cy;sy|]
    and look_direction_up = [|ch*.sr+.sh*.sy*.cr;-.sh*.sr+.ch*.sy*.cr;cy*.cr|]
    in
    let look_direction_sideways = cross_product_3d look_direction_forward look_direction_up in
    let pos0 =
      camera.fci_pos.(0)
      +. look_direction_sideways.(0)*.(0.2*.wf2+.0.15*.wf3)
      +. look_direction_up.(0)*.(0.05*.wf1+.0.02*.wf2)
    and pos1 =
      camera.fci_pos.(1)
      +. look_direction_sideways.(1)*.(0.2*.wf2+.0.15*.wf3)
      +. look_direction_up.(1)*.(0.05*.wf1+.0.02*.wf2)
    and pos2 =
      camera.fci_pos.(2)
      +. look_direction_sideways.(2)*.(0.2*.wf2+.0.15*.wf3)
      +. look_direction_up.(2)*.(0.05*.wf1+.0.02*.wf2)
    in
      begin
	GlClear.clear [`color;`depth];
	(* XXX this does not work yet! *)
	GlMat.mode `projection;
	GlMat.load_identity();
        GluMat.perspective ~fovy:25.0 ~aspect:1. ~z:(0.01, 10000.);
        (* GluMat.look_at ~eye:(0.0,-20.0,0.0) ~center:(0.0,0.0,0.0) ~up:(0.0,0.0,1.0); *)
	GluMat.look_at
	  ~eye:(pos0,pos1,pos2)
	  ~center:(pos0+.look_direction_forward.(0),
		   pos1+.look_direction_forward.(1),
		   pos2+.look_direction_forward.(2))
	  ~up:(look_direction_up.(0),look_direction_up.(1),look_direction_up.(2));
	(* "Improvement" to directly create "up" in that braindead triplet format
	   deliberately not done! *)
	GlMat.mode `modelview;
	GlMat.load_identity();
	GlDraw.color (1.0, 1.0, 1.0);
	GlDraw.shade_model `smooth;	(* Or: `flat *)
	GlLight.material ~face:`front(`shininess 128.0);
	(* GlDraw.shade_model `smooth; *)
        (* stuff *)
	let dirs = [|[|1.0;0.0;0.0|];[|0.0;1.0;0.0|];[|0.0;0.0;1.0|]|] in
	  (*
            for xi = 0 to 5-1 do
            for yi = 0 to 5-1 do
            for zi = 0 to 5-1 do
            let x = (float_of_int (xi-2))*.1.5
	    and y = (float_of_int (yi-2))*.1.5
	    and z = (float_of_int (zi-2))*.1.5
	    in
	    draw_octahedron [|1.0;0.2;0.2|] ~scale:0.2 dirs [|x;y;z|]
	    done; done; done;
	  *)
	  (* GluQuadric.sphere ~radius:1.0 ~slices:12 ~stacks:12 (); *)
	  draw_lines [|0.8;0.8;0.8|] plotinfo_coords plotinfo_links;
	  (let FEM_field (_,v_data) = fields.(0) in
	     Mpi_petsc.with_petsc_vector_as_bigarray v_data
	       (fun ba ->
		  Array.iteri
		    (fun n indices ->
		       let coords = mesh.mm_points.(n).mp_coords in
		       let vec = [|ba.{indices.(0)};ba.{indices.(1)};ba.{indices.(2)};|] in
			 draw_arrow [|1.0;0.6;0.6|] coords (array_pointwise (+.) coords vec))
		    field_vertex_indices
	       )
	  );
	  (* end stuff *)
	  Gl.flush ();
	  area#swap_buffers ();
      end
  in
  let left_labels = (* Set left labels *)
    Array.mapi
      (fun n text ->
	GMisc.label
	  ~text:text
	  ~packing:(table_master#attach ~top:n ~left:0 ~expand:`BOTH)
	  ()
      )
      [|"Camera";"Camera  1 Info  ";"Probe";"Probe  1 Info  "|]
  in
  let select_cam n = 
    let _ = left_labels.(1)#set_text (Printf.sprintf "Camera %2d Info  " (1+n)) in
    let _ = fs_state.fss_active_camera <- n in
    List.iter (fun f -> f()) !r_set_camera_box
  in
  let select_probe n =
    let _ = left_labels.(3)#set_text (Printf.sprintf "Probe %2d Info  " (1+n)) in
    let _ = fs_state.fss_active_probe <- n in
    List.iter (fun f -> f()) !r_set_probe_box
  in
  let cams_hbox = GPack.hbox ~packing:(table_master#attach ~top:0 ~left:1 ~expand:`BOTH) () in
  let cams_buttons =
    let first_radiobutton =
      GButton.radio_button ~label:" 1" ~packing:cams_hbox#add ~active:true () in
    Array.init nr_cameras
      (fun n ->
	let b =
	  if n=0 then first_radiobutton
	  else
	    GButton.radio_button
	      ~label:(Printf.sprintf "%2d" (1+n))
	      ~packing:cams_hbox#add
	      ~group:first_radiobutton#group ()
	in
	let _ = b#connect#clicked ~callback:(fun _ -> select_cam n) in
	b
      )
  in
  (* Time for the camera box & setter function... *)
  let cam_table =
    GPack.table ~rows:3 ~columns:7 ~show:false () in
  let _ =
    GObj.pack_return cam_table
      (Some (table_master#attach ~top:1 ~left:1 ~expand:`BOTH)) None
  in
  let _ =
    let cam_table_labels =
      Array.map
	(fun (x,y,text) ->
	  GMisc.label
	    ~text:text ~packing:(cam_table#attach ~top:y ~left:x ~expand:`BOTH) ())
	[|(0,0,"Position");(1,0,"x");(3,0,"y");(5,0,"z");
	  (0,1,"Orientation");(1,1,"dir");(3,1,"tilt");(5,1,"roll");
	  (0,2,"View");(1,2,"zoom");(3,2,"mode");(5,2,"wander");
	|]
    in
    let cam_table_pos_fields =
      Array.init 3
	(fun n ->
	  let entry =
	    GEdit.entry
	      ~max_length:8 ~width:60
	      ~text:(Printf.sprintf "%8.4f"
		       fs_state.fss_cameras.(fs_state.fss_active_camera).fci_pos.(n))
	      ~packing:(cam_table#attach ~top:0 ~left:(2+2*n) ~expand:`BOTH) ()
	  in
	  let _ = entry#connect#activate
	      (fun () ->
		let nr_active = fs_state.fss_active_camera in
		let txt = entry#text in
		let new_val =
		  try float_of_string txt
		  with
		  | Failure _ -> 
		      fs_state.fss_cameras.(fs_state.fss_active_camera).fci_pos.(n)
		in
		begin
		  fs_state.fss_cameras.(fs_state.fss_active_camera).fci_pos.(n) <- new_val;
		  entry#set_text (Printf.sprintf "%8.4f" new_val);
		  display();
		end)
	  in entry)
    in	
    let cam_table_ori_fields =
      Array.init 3 (* XXX Note: cut&paste programming - unify with above! *)
	(fun n ->
	  let entry =
	    GEdit.entry
	      ~max_length:8 ~width:60
	      ~text:(Printf.sprintf "%8.4f"
		       fs_state.fss_cameras.(fs_state.fss_active_camera).fci_ori.(n))
	      ~packing:(cam_table#attach ~top:1 ~left:(2+2*n) ~expand:`BOTH) ()
	  in
	  let _ = entry#connect#activate
	      (fun () ->
		let nr_active = fs_state.fss_active_camera in
		let txt = entry#text in
		let new_val =
		  try float_of_string txt
		  with
		  | Failure _ -> 
		      fs_state.fss_cameras.(fs_state.fss_active_camera).fci_ori.(n)
		in
		begin
		  fs_state.fss_cameras.(fs_state.fss_active_camera).fci_ori.(n) <- new_val;
		  entry#set_text (Printf.sprintf "%8.4f" new_val);
		  display();
		end)
	  in entry)
    in
    let cam_table_zoom_field =
      GEdit.entry ~max_length:8 ~width:60
	~text:(Printf.sprintf "%8.4f" fs_state.fss_cameras.(fs_state.fss_active_camera).fci_zoom)
	~packing:(cam_table#attach ~top:2 ~left:2 ~expand:`BOTH)
	()
    in
    let _ = cam_table_zoom_field#connect#activate
	(fun () ->
	  let nr_active = fs_state.fss_active_camera in
	  let txt = cam_table_zoom_field#text in
	  let new_val =
	    try
	      let z = float_of_string txt in
	      if z<0.0 || z>100.0 then failwith "out of bounds"
	      else z
	    with
	    | Failure _ -> 
		fs_state.fss_cameras.(fs_state.fss_active_camera).fci_zoom
	  in
	  begin
	    fs_state.fss_cameras.(fs_state.fss_active_camera).fci_zoom <- new_val;
	    cam_table_zoom_field#set_text (Printf.sprintf "%8.4f" new_val);
	    display();
	  end)
    in
    let cam_table_mode_field =
      let opt_menu =
	GMenu.option_menu
	  ~packing:(cam_table#attach ~top:2 ~left:4 ~expand:`BOTH) () in
      let menu = GMenu.menu () in
      let mgroup = ref None in
      let m_items = 
	Array.map
	  (fun m ->
	    let mi =
	      GMenu.radio_menu_item ?group:!mgroup ~label:m 
		~packing:(menu#append) ~show_toggle:true ()
	    in
	    let _ =
	      mi#connect#activate 
		(fun () ->
		  let () = Printf.printf "MODE: %s\n%!" m in
		  fs_state.fss_cameras.(fs_state.fss_active_camera).fci_mode
		  <- m)
	    in
	    let _ = mgroup:= Some (mi#group) in
	    (m,mi)
	  )
	  [|"wireframe";"surfaces"|]
      in
      let _ = opt_menu#set_menu menu in
      (opt_menu,m_items)
    in
    let cam_table_wander_field =
      let opt_menu =
	GMenu.option_menu
	  ~packing:(cam_table#attach ~top:2 ~left:6 ~expand:`BOTH) () in
      let menu = GMenu.menu () in
      let mgroup = ref None in
      let m_items = 
	Array.map
	  (fun m ->
	    let mi =
	      GMenu.radio_menu_item ?group:!mgroup ~label:m
		~packing:(menu#append) ~show_toggle:true ()
	    in
	    let _ =
	      mi#connect#activate 
		(fun () ->
		  let () = Printf.printf "WANDER: %s\n%!" m in
		  fs_state.fss_cameras.(fs_state.fss_active_camera).fci_wander
		  <- m)
	    in
	    let _ = mgroup:= Some (mi#group) in
	    (m,mi)
	  )
	  [|"off";"slow";"fast"|]
      in
      let _ = opt_menu#set_menu menu in
      (opt_menu,m_items)
    in
    let () =
      r_set_camera_box:=
      (fun () ->
	let nr_active = fs_state.fss_active_camera in
	begin
	  for i=0 to 3-1 do
	    cam_table_pos_fields.(i)#set_text 
	      (Printf.sprintf "%8.4f" fs_state.fss_cameras.(nr_active).fci_pos.(i));
	  done;
	  for i=0 to 3-1 do
	    cam_table_ori_fields.(i)#set_text 
	      (Printf.sprintf "%8.4f" fs_state.fss_cameras.(nr_active).fci_ori.(i));
	  done;
	  cam_table_zoom_field#set_text 
	    (Printf.sprintf "%8.4f" fs_state.fss_cameras.(nr_active).fci_zoom);
	  (let mode = fs_state.fss_cameras.(nr_active).fci_mode in
	   let (_,m_items) = cam_table_mode_field in
	   let (_,widget) = List.find (fun (x,_) -> x=mode) (Array.to_list m_items) in
	   let () = Printf.printf "Change mode: %s\n%!" mode (* XXX Strangely, this gets called twice! *)
	   in
	   widget#select () 
	  );
	  (let wander = fs_state.fss_cameras.(nr_active).fci_wander in
	   let (_,m_items) = cam_table_wander_field in
	   let (_,widget) = List.find (fun (x,_) -> x=wander) (Array.to_list m_items) in
	   let () = Printf.printf "Change wander: %s\n%!" wander (* XXX Strangely, this gets called twice! *)
	   in
	   widget#select ()
	  );
	end):: !r_set_camera_box
    in
    ()
  in
  let probes_hbox = GPack.hbox ~packing:(table_master#attach ~top:2 ~left:1 ~expand:`BOTH) () in
  let probes_buttons =
    Array.init nr_probes
      (fun n ->
	let b=
	  GButton.button
	    ~label:(Printf.sprintf "%2d" (1+n))
	    ~packing:probes_hbox#add ()
	in
	let _ = b#connect#clicked ~callback:(fun _ -> select_probe n) in
	b
      )
  in
  (* Next, probe box & setter function... *)
  let probe_info_hbox =
    GPack.hbox ~packing:(table_master#attach ~top:3 ~left:1 ~expand:`BOTH) ()
  in
  let probe_controls_table =
    GPack.table ~rows:4 ~columns:2 ~show:false () in
  let _ =
    GObj.pack_return probe_controls_table
      (Some (probe_info_hbox#add)) None
  in  
  let probe_info_vbox_data =
    GPack.vbox ~packing:(probe_info_hbox#add) ()
  in
  let probe_data_scroll =
    GBin.scrolled_window ~border_width:2 ~width:350 ~height:120
      ~packing:(probe_info_vbox_data#add) ()
  in
  let probe_data=GText.buffer ~text:"Hello" () in
  let probe_data_view =
    GText.view ~editable:false
      ~packing:(probe_data_scroll#add) ~show:true ()
  in
  let _ = probe_data_view#set_buffer probe_data in
  let _ = probe_data_view#misc#set_can_focus false in
  let probe_control_labels =
    Array.mapi
      (fun n text ->
	GMisc.label
	  ~text:text ~packing:(probe_controls_table#attach ~top:n ~left:0 ~expand:`BOTH) ())
      [|"Colours ";"Field ";"Mode ";"Position "|]
  in
  let probe_color_hbox =
    GPack.hbox ~packing:(probe_controls_table#attach ~top:0 ~left:1 ~expand:`BOTH) ()
  in
  let probe_color1 =
    let entry =
      GEdit.entry ~max_length:6 ~width:60 ~text:"ff0000"
	~packing:(probe_color_hbox#add) ()
    in
    let _ = entry#connect#activate
	(fun () ->
	  let nr_active = fs_state.fss_active_probe in
	  let txt = entry#text in
	  let new_val =
	    try hexstring_to_int txt
	    with
	    | Failure _ -> 
		fs_state.fss_probes.(fs_state.fss_active_probe).fpi_color1
		in
		begin
		  fs_state.fss_probes.(fs_state.fss_active_probe).fpi_color1
		  <- new_val;
		  entry#set_text (Printf.sprintf "%06x" new_val);
		  display();
		end)
    in entry
  in
  let probe_color12 =
    GMisc.label ~text:" -- " ~packing:(probe_color_hbox#add) ()
  in
  let probe_color2 =
    let entry =
      GEdit.entry ~max_length:6 ~width:60 ~text:"00ff00"
	~packing:(probe_color_hbox#add) ()
    in
    let _ = entry#connect#activate
	(fun () ->
	  let nr_active = fs_state.fss_active_probe in
	  let txt = entry#text in
	  let new_val =
	    try hexstring_to_int txt
	    with
	    | Failure _ -> 
		fs_state.fss_probes.(fs_state.fss_active_probe).fpi_color2
		in
		begin
		  fs_state.fss_probes.(fs_state.fss_active_probe).fpi_color2
		  <- new_val;
		  entry#set_text (Printf.sprintf "%06x" new_val);
		  display();
		end)
    in entry
  in
  let probe_pos_hbox =
    GPack.hbox ~packing:(probe_controls_table#attach ~top:3 ~left:1 ~expand:`BOTH) ()
  in
  let probe_pos_entries =
    Array.mapi
	(fun n name ->
	  let lbl = GMisc.label ~text:name ~packing:(probe_pos_hbox#add) () in
	  let entry =
	    GEdit.entry
	      ~max_length:8 ~width:80
	      ~text:(Printf.sprintf "%8.4f"
		       fs_state.fss_probes.(fs_state.fss_active_probe).fpi_pos.(n))
	      ~packing:(probe_pos_hbox#add) ()
	  in
	  let _ = entry#connect#activate
	      (fun () ->
		let nr_active = fs_state.fss_active_probe in
		let txt = entry#text in
		let new_val =
		  try float_of_string txt
		  with
		  | Failure _ -> 
		      fs_state.fss_probes.(fs_state.fss_active_probe).fpi_pos.(n)
		in
		begin
		  fs_state.fss_probes.(fs_state.fss_active_probe).fpi_pos.(n)
		    <- new_val;
		  entry#set_text (Printf.sprintf "%8.4f" new_val);
		  display();
		end)
	  in entry)
      [|" x ";" y ";" z "|]
  in	
  let probe_mode =
    let opt_menu = 
      GMenu.option_menu
	~packing:(probe_controls_table#attach ~top:2 ~left:1 ~expand:`BOTH) () in
    let menu = GMenu.menu () in
    let mgroup = ref None in
    let mitems =    
      Array.map
	(fun m ->
	  let mi =
	    GMenu.radio_menu_item ?group:!mgroup ~label:m
	      ~packing:(menu#append) ~show_toggle:true ()
	  in
	  let _ =
	    mi#connect#activate 
	      (fun () ->
		let () = Printf.printf "PROBE MODE: %s\n%!" m in
		fs_state.fss_probes.(fs_state.fss_active_probe).fpi_mode
		<- m)
	  in
	  let _ = mgroup:= Some (mi#group) in
	  (m,mi)
	)
	[|"Inactive";"Single";"Lattice (5)";"Lattice (10)";"Lattice (15)";
	  "On Mesh";"Colored Mesh"|]
    in
    let _ = opt_menu#set_menu menu in
    (opt_menu,mitems)
  in
  let probe_field =
    let opt_menu = 
      GMenu.option_menu
	~packing:(probe_controls_table#attach ~top:1 ~left:1 ~expand:`BOTH) () in
    let menu = GMenu.menu () in
    let mgroup = ref None in
    let mitems =
      Array.map
	(fun m ->
	  let mi =
	    GMenu.radio_menu_item ?group:!mgroup ~label:m
	      ~packing:(menu#append) ~show_toggle:true ()
	  in
	  let _ =
	    mi#connect#activate 
	      (fun () ->
		let () = Printf.printf "PROBE FIELD: %s\n%!" m in
		fs_state.fss_probes.(fs_state.fss_active_probe).fpi_field
		<- m)
	  in
	  let _ = mgroup:= Some (mi#group) in
	  (m,mi)
	)
	[|"M [0]";"H_demag [1]";"H_exch [2]";"J [3]";"T [4]";|]
    in
    let _ = opt_menu#set_menu menu in
    (opt_menu,mitems)
  in
  let () =
    r_set_probe_box:=
      (fun () ->
	let nr_active = fs_state.fss_active_probe in
	begin
	  for i=0 to 3-1 do
	    probe_pos_entries.(i)#set_text 
	      (Printf.sprintf "%8.4f" fs_state.fss_probes.(nr_active).fpi_pos.(i));
	  done;
	  probe_color1#set_text 
	    (Printf.sprintf "%06x" fs_state.fss_probes.(nr_active).fpi_color1);
	  probe_color2#set_text 
	    (Printf.sprintf "%06x" fs_state.fss_probes.(nr_active).fpi_color2);
	  (let mode = fs_state.fss_probes.(nr_active).fpi_mode in
	  let (_,m_items) = probe_mode in
	  let () = Printf.printf "set mode=%s\n%!" mode in
	  let (_,widget) = List.find (fun (x,_) -> x=mode) (Array.to_list m_items) in
	  let () = Printf.printf "Change P mode: %s\n%!" mode
	  in
	  widget#select ()
	  );
	  (let field = fs_state.fss_probes.(nr_active).fpi_field in
	  let (_,m_items) = probe_field in
	  let () = Printf.printf "set field=%s\n%!" field in
	  let (_,widget) = List.find (fun (x,_) -> x=field) (Array.to_list m_items) in
	  let () = Printf.printf "Change P field: %s\n%!" field
	  in
	  widget#select ()
	  );
	end) :: !r_set_probe_box
  in
  (* --- OpenGL --- *)
  let () = gl_init () in
  (* let _ = w#event#add [`KEY_PRESS] in *)
  begin
    w#event#connect#key_press
      ~callback:
      (let gl_mode = ref false in
      fun ev ->
	let key = GdkEvent.Key.keyval ev in
	if key = GdkKeysyms._Escape then
	  let _ = w#destroy () in true
	else
	if key = GdkKeysyms._KP_Enter then
	  begin
	    Printf.fprintf stderr "Switching Mode!\n%!";
	    gl_mode := not !gl_mode;
	    true
	  end
	else
	  if !gl_mode = true then
	    (* Handle GL keypress *)
	    let _ = Printf.printf "GL Key Pressed!\n%!" in      
	    (* let _ = field_scope#display () in *)
	    true		(* Consumed this event *)
	  else
	    false (* Non-GL keypress - not for us! *)
      );
    (* No animation running - otherwise, we would use this!
       Timeout.add ~ms:200 ~callback:
       (fun () -> let _ =  (* advance time... *) field_scope#display () in true)
     *)
    r_set_camera_box:= display :: !r_set_camera_box;
    r_set_probe_box:= display :: !r_set_probe_box;
    area#connect#display ~callback:display;
    area#connect#reshape ~callback:gl_reshape;
    area#connect#realize ~callback:
      (fun () ->
	let _ = Printf.printf "AREA realize!\n%!" in
	let _ = gl_init () in
	let _ = gl_reshape ~width:550 ~height:550 in
	  ());
    w#show ();
    let thr =
      Thread.create
	(fun () -> 
	  let rec work n =
	    let _ = Unix.select [] [] [] 0.02 in (* delay *)
	    let _ = Voodoo.gtk_main_iteration_do false in
	    let _ = (if n=0 then display() else ()) in
	    work ((n+1) mod 3)
	  in work 0)
	()
    in
      (* GMain.Main.main () *)
      display
  end
;;

(* === Python-related stuff === *)

let ddd_python_field_scope_registered_fields = ref [||];;

let _py_ddd_field_scope_register_fields =
  python_interfaced_function
    [|ListType;|]
    (fun arr ->
       let pyarr = pylist_toarray arr.(0) in
       let pos_nonfield =
	 array_position_if
	   (fun py -> (pytype py <> OtherType) || (ocamlpill_type_of py <> "FEM Field"))
	   (* Actually, we would like to compare against Pyfem.pysym_field, but that is
	      not exported.
	   *)
	   pyarr 0
       in
	 if pos_nonfield <> (-1) then
	   begin
	     Printf.printf "XXX Problem: not a proper field given!\n%!";
	     pynone() (* Failure *)
	   end
	 else
	   let () =
	     ddd_python_field_scope_registered_fields :=
	       Array.map
		 (fun x -> (ocamlpill_hard_unwrap:(pyobject -> float Fem_element.fem_field)) x)
		 pyarr
	   in pynone()
		(* XXX Should report success/failure somehow? *)
    )
;;


let ddd_start_scope () =
  let do_redraw = interactive_field_scope (!ddd_python_field_scope_registered_fields) in
    (* Note: ddd_start_scope has to be called from within OCaml, but we still
       have to announce the result to python. Furthermore, as we do not want to
       export anything from pyfem, we have to just duplicate the ocamlpill wrapper.

       All this is quite hackish and ugly, but hey, field_scope after all is just
       one big experiment.
    *)
  let py_do_redraw = Pycaml.make_ocamlpill_wrapper "Internal void -> void function" do_redraw do_redraw in
  let _ =
    register_for_python
      [|("field_scope_do_redraw",py_do_redraw);|]
  in ()
;;

let _ =
  register_for_python
    [|("field_scope_register_fields",_py_ddd_field_scope_register_fields);
    |]
;;

