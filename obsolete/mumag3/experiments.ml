
(* (C) 2007 Dr. Thomas Fischbacher
   
   Experimenting with periodic boundary conditions
   and the Lindholm formula...

   For now, I restrict myself to investigating a cubic cell...
 *)

#use "topfind";;
#require "snippets";;
open Snippets;;

let ht_nodes_by_len_sq = Hashtbl.create 100;;

let r_max = 31 (* 121 *);;

let () =
  for n_x = -r_max/2 to r_max/2 do
    for n_y = -r_max/2 to r_max/2 do
      for n_z = -r_max/2 to r_max/2 do
	let len_sq = 9*n_x*n_x+1*n_y*n_y+n_z*n_z in
	  hashtbl_push ht_nodes_by_len_sq len_sq [|n_x;n_y;n_z|]
      done
    done
  done
;;

let a_nodes_by_len_sq = map_hashtbl_to_array ~sorter:compare (fun k v -> (k,Array.of_list v)) ht_nodes_by_len_sq;;

let for_shell r_min r_max f =
  let r2_min = r_min*r_min
  and r2_max = r_max*r_max
  in
  let rec walk_len2 pos =
    if pos = Array.length a_nodes_by_len_sq 
    then
      let () = Printf.fprintf stderr "WARNING: run out of nodes!\n%!" in
	()
    else 
      let (r2,entries) = a_nodes_by_len_sq.(pos) in
	if r2 >= r2_max then ()
	else if r2 < r2_min then walk_len2 (1+pos)
	else
	  let () = Array.iter f entries in
	    walk_len2 (1+pos)
  in walk_len2 0
;;

(*
val lindholm_triangle_contributions :
  outward_surface_normal:float array ->
  store_lh012:float array ->
  observer:float array -> float array -> float array -> float array -> unit
*)

let lindholm_triangle_contributions =
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
		v_log.(0) <- log ((r1l+.r2l+.s0l)/.(r1l+.r2l-.s0l));
		v_log.(1) <- log ((r2l+.r0l+.s1l)/.(r2l+.r0l-.s1l));
		v_log.(2) <- log ((r0l+.r1l+.s2l)/.(r0l+.r1l-.s2l));
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
		let must_change_orientation =
		  sprod v_zeta outward_surface_normal > 0.0
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

let lindholm_shell ~a0 ~observer ~p0 ~p1 ~p2 ~lattice_r_min ~lattice_r_max () =
  let surface_normal = 
    let p01 = array_pointwise (-.) p1 p0 in
    let p02 = array_pointwise (-.) p2 p0 in
    let xprod = cross_product_3d p01 p02 in
    let inv_len = 1.0/.(sqrt(euclidean_len_sq xprod)) in
      Array.map (fun x -> x*.inv_len) xprod
  in
  let dim = Array.length p0 in
  let sum = Array.make 3 0.0 in
  let contribs = Array.make 3 0.0 in
  let offset_observer = Array.make 3 0.0 in
  let () = for_shell lattice_r_min lattice_r_max
    (fun n_xyz ->
       begin
	 for i=0 to dim-1 do
	   offset_observer.(i) <- observer.(i)+.a0*.(float_of_int n_xyz.(i));
	 done;
	 lindholm_triangle_contributions
	   ~outward_surface_normal:surface_normal
	   ~store_lh012:contribs
	   ~observer:offset_observer
	   p0 p1 p2;
	 (* Printf.printf "n_xyz=%s contribs=%s\n%!" (int_array_to_string n_xyz) (float_array_to_string contribs); *)
	 for i=0 to dim-1 do
	   sum.(i) <- sum.(i) +. contribs.(i)
	 done
       end)
  in
    sum
;;

(*
for r_max=1 to 15-1 do
  let a0=10.0 in
  let lh=lindholm_shell
    ~a0
    ~observer:[|0.0;0.0;0.0|]
    ~p0:[|3.0;2.0;1.0|] ~p1:[|4.0;6.0;2.0|] ~p2:[|3.0;5.0;4.0|]
    ~lattice_r_min:0 ~lattice_r_max:r_max ()
  in
    Printf.printf "r_max=%7.2f: %16.10f %16.10f %16.10f\n%!" (a0*.(float_of_int r_max)) lh.(0) lh.(1) lh.(2)
done
;;

r_max=  10.00:     0.0023292410     0.0013744447     0.0014490734
r_max=  20.00:     0.0030828709     0.0026653739     0.0026290526
r_max=  30.00:     0.0031213357     0.0027380120     0.0026978433
r_max=  40.00:     0.0031215246     0.0027383980     0.0026979588
r_max=  50.00:     0.0031227371     0.0027407005     0.0027000824
r_max=  60.00:     0.0031230908     0.0027413676     0.0027007286
r_max=  70.00:     0.0031233983     0.0027419510     0.0027012754
r_max=  80.00:     0.0031233903     0.0027419374     0.0027012621
r_max=  90.00:     0.0031234505     0.0027420510     0.0027013683
r_max= 100.00:     0.0031234719     0.0027420940     0.0027014081
r_max= 110.00:     0.0031235212     0.0027421848     0.0027014961
r_max= 120.00:     0.0031235035     0.0027421477     0.0027014670
r_max= 130.00:     0.0031234935     0.0027421183     0.0027014408
r_max= 140.00:     0.0031235159     0.0027421510     0.0027014942

Re-scaling y and z axes by a factor 3:

r_max=  10.00:     0.0023292410     0.0013744447     0.0014490734
r_max=  20.00:     0.0011712705     0.0003746383     0.0005721621
r_max=  30.00:     0.0009765715     0.0001710876     0.0003905213
r_max=  40.00:     0.0018717562     0.0013990855     0.0014529069
r_max=  50.00:     0.0019712052     0.0014540654     0.0015386758
r_max=  60.00:     0.0016167337     0.0010649193     0.0011875888
r_max=  70.00:     0.0022701799     0.0018043991     0.0018545341
r_max=  80.00:     0.0018305492     0.0013118269     0.0014075980
r_max=  90.00:     0.0018676436     0.0013498127     0.0014434837
r_max= 100.00:     0.0019955197     0.0014954904     0.0015736522
r_max= 110.00:     0.0019809439     0.0014792414     0.0015599326
r_max= 120.00:     0.0018179552     0.0012969917     0.0013948680
r_max= 130.00:     0.0019956772     0.0014954910     0.0015741421
r_max= 140.00:     0.0019559188     0.0014508161     0.0015337856

Just re-scaling z axis by a factor 2:

r_max=  10.00:     0.0023292410     0.0013744447     0.0014490734
r_max=  20.00:     0.0028013310     0.0024355081     0.0024920793
r_max=  30.00:     0.0030595023     0.0027072134     0.0027135530
r_max=  40.00:     0.0029827389     0.0026527977     0.0027142050
r_max=  50.00:     0.0030373691     0.0026858488     0.0027087994
r_max=  60.00:     0.0030031691     0.0026663015     0.0027146053
r_max=  70.00:     0.0030364683     0.0026879567     0.0027127078
r_max=  80.00:     0.0030121213     0.0026720854     0.0027142856
r_max=  90.00:     0.0030328078     0.0026854579     0.0027128867
r_max= 100.00:     0.0030173048     0.0026754404     0.0027140474
r_max= 110.00:     0.0030319279     0.0026850037     0.0027130540
r_max= 120.00:     0.0030193660     0.0026769620     0.0027142052
r_max= 130.00:     0.0030284565     0.0026828209     0.0027134974
r_max= 140.00:     0.0030211070     0.0026781221     0.0027141471

Re-scaling z axis by a factor 3:

r_max=  10.00:     0.0023292410     0.0013744447     0.0014490734
r_max=  20.00:     0.0028013310     0.0024355081     0.0024920793
r_max=  30.00:     0.0027695381     0.0024440821     0.0025414933
r_max=  40.00:     0.0030092386     0.0026680184     0.0027087782
r_max=  50.00:     0.0029589773     0.0026354666     0.0027147109
r_max=  60.00:     0.0029206130     0.0026143036     0.0027242420
r_max=  70.00:     0.0029991212     0.0026653859     0.0027180672
r_max=  80.00:     0.0029634509     0.0026417147     0.0027202915
r_max=  90.00:     0.0029425084     0.0026286415     0.0027230711
r_max= 100.00:     0.0029885809     0.0026578285     0.0027183234
r_max= 110.00:     0.0029696952     0.0026459545     0.0027203129
r_max= 120.00:     0.0029511903     0.0026341767     0.0027223596
r_max= 130.00:     0.0029835485     0.0026548093     0.0027192627
r_max= 140.00:     0.0029707013     0.0026465699     0.0027203165

Re-scaling x axis by a factor 3:

r_max=  10.00:     0.0023292410     0.0013744447     0.0014490734
r_max=  20.00:     0.0049993565     0.0046378910     0.0043955048
r_max=  30.00:     0.0056186996     0.0053475303     0.0050420299
r_max=  40.00:     0.0042945441     0.0040295482     0.0038636228
r_max=  50.00:     0.0048269839     0.0046206344     0.0043988778
r_max=  60.00:     0.0052634819     0.0051061422     0.0048384227
r_max=  70.00:     0.0044512538     0.0042101552     0.0040287289
r_max=  80.00:     0.0048167927     0.0046132685     0.0043930107
r_max=  90.00:     0.0050403224     0.0048607708     0.0046169110
r_max= 100.00:     0.0045570040     0.0043274068     0.0041349417
r_max= 110.00:     0.0047553753     0.0045461807     0.0043325710
r_max= 120.00:     0.0049503793     0.0047618191     0.0045275860
r_max= 130.00:     0.0046123946     0.0043890030     0.0041907223
r_max= 140.00:     0.0047455048     0.0045355683     0.0043230740

*)

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
       corresponding lattice vector.
    *)
    ~cell_rescaling (* >1, we use the inverse. *)
    =
  let dim = Array.length v_lattice in
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
	   if fun_global_shape real_pos > 0.0
	   then r_nr_cells_inside := 1+ !r_nr_cells_inside
	   else ()
	 end)
    in
      (float_of_int (!r_nr_cells_inside)) *. oversampling_inv_nr_cells
  in
  let lattice_points_in_sphere_and_greyfactors =
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
	    else if euclidean_len_sq (intcoords_to_pos ~rescaled:true i_xyz) > 1.0
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
    lattice_points_in_sphere_and_greyfactors
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
		lindholm_triangle_contributions ~outward_surface_normal
		  ~store_lh012:v_buffer
		  ~observer:displaced_observer p0 p1 p2;
		for i=0 to 3-1 do
		  store_lh012.(i) <- store_lh012.(i)+.v_buffer.(i)*.greyfactor
		done;
	      end
	  done
	end
;;

	    
