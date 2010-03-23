(*
   (C) 2005 Dr. Thomas Fischbacher

  XXX Note: for now, we only support (double-precision) float degrees
  of freedom.

  Later on, we may want to support complex as well.

  I suppose that there should be little point in trying to support
  even more complicated stuff, such as lie-algebra-valued DOFs, as
  those are maybe better regarded as collections of individual DOFs -
  I did not yet make up my mind about nonabelian theories...

  XXX Some of the stuff in here should be migrated to a mumag
  package, as it's not fem-element specific...

  ocamlc -I ../nsim_grammars -I ../fastfields -I ../diffop_parser -I ../qhull -I ../snippets -I ../mesh -I ../nlog -I ../mpi_petsc -I ../sundials -i fem.ml 

*)

(*
   Terminology: we call:

   - the "natural coordinates" L_i the "shape functions" of a simplex,

   - vertices or intermediate higher-order points of a mesh "sites".

   - the "shape functions" associated to sites "site functions",

   - the "shape functions" associated to a dof "dof functions",

   - all the functions defined on a simplex "element functions"

*)

(* First thing we have to take care of is the allocation of function
   degrees of freedom to the mesh. Note that if we go higher-order,
   we may have DOFs located not only at mesh vertices, but also other sites
   (such as face midpoints, say).

   What do we have to know:

 * For every DOF, what simplices does it belong to, and which simplex-site
   number is it in the corresponding simplex?

 * Where is the dof located in space (coordinates, shape function)?

*)

open Snippets;;
open Mesh;;

let version () = 
  String.concat ":" ["$Id$";
		      (Snippets.version());
		      (Mesh.version());
		      (Mpi_petsc.version());]
;;

(* Removed md5 from version string. This should happen later in my opinion. (fangohr 13/02/2007) :
let version () = Snippets.md5
  (String.concat ":" ["$Id$";
		      (Snippets.version());
		      (Mesh.version());
		      (Mpi_petsc.version());])
;;
*)


let logmsg = Nlog.getLogger("nfem.ocaml");;
let loginfo = logmsg Nlog.Info;;
let loginfo2 = logmsg Nlog.Info2;;
let logdebug =logmsg Nlog.Debug;;


type abssite = int array;;
 (* Something like [|2;1;0;0|], meaning 1/3(2*P0+P1) *)

type site = int array;;
 (* Something like [|21;21;29|], which is a concrete realization of the above *)

type dof_mapping_long_to_short = int array;;
type dof_mapping_short_to_long = int array;;
type distribution_information = int array;;

(* XXX OBSOLETE: *)
type dof_region_spec = (string * bool * (int option array)) array;;
(* array of possibilities of region combinations a DOF must belong to.
   (dof_stem,strictly_these_and_no_extra_regions,regions) *)

let print_dof_region_spec drs =		(* DDD *)
  let () = Printf.printf "#<DOF Region Spec" in
  array_foreach_do drs
    (fun (stem,strict,regions) ->
       Printf.printf " [%s, %s, regions=%s]" stem (if strict then "strict" else "lax")
	 (int_array_to_string (Array.map (fun x -> match x with | None -> -100 | Some n -> n) regions)))
;;

type dof_region_logic = Ddiffop_parser.dof_logic;;

(* A data vector shortening selection (dvss) will consist of information about 
   subfield name stems plus associated region logic. (Note: every subfield name
   may only appear ONCE in here!)

   A "None" dvss will mean: "No restriction".

   I principle, we could do without the dvss abstraction and just
   extend the region_logic specification in such a way that it also
   checks subfield names. Then, however, that would complicate the
   vivificator which would have to do many more checks to make a
   single matrix entry. Hence we (for now) go for this design:
*)
type dvss = (string * dof_region_logic) array option;;

let dvss_to_string dvss =
  match dvss with
    | None -> "#[DVSS: No restriction]"
    | Some a ->
	let rec dof_region_logic_to_string drl =
	  match drl with
	    | Ddiffop_parser.DLOG_true -> "TRUE"
	    | Ddiffop_parser.DLOG_and xs ->
		(Printf.sprintf "(AND %s)"
		   (String.concat " " (List.map dof_region_logic_to_string xs)))
	    | Ddiffop_parser.DLOG_or xs ->
		(Printf.sprintf "(OR %s)"
		   (String.concat " " (List.map dof_region_logic_to_string xs)))
	    | Ddiffop_parser.DLOG_not x ->
		(Printf.sprintf "(NOT %s)"
		   (dof_region_logic_to_string x))
	    | Ddiffop_parser.DLOG_all x ->
		(Printf.sprintf "(ALL %s)" x)
	    | Ddiffop_parser.DLOG_some x ->
		(Printf.sprintf "(SOME %s)" x)
	    | Ddiffop_parser.DLOG_nregions x ->
		(Printf.sprintf "(NREGIONS %d)" x)
	in
	  Printf.sprintf "#[DVSS: %s]"
	    (String.concat "; "
	       (Array.to_list
		  (Array.map 
		     (fun (stem, dof_region_logic) ->
			(Printf.sprintf "%s => %s" stem
			   (dof_region_logic_to_string dof_region_logic)))
		     a)))
;;


(* The problem with degree-of-freedom names is that we may have in parallel
   dofs with very different type on our mesh. For example, one such set may be:

   (T, M_x,M_y,M_z,
   J_x_x, J_x_y, J_x_z,
   J_y_x, J_y_y, J_y_z,
   J_z_x, J_z_y, J_z_z)

   where T is temperature, M is magnetization, and J are
   components of the spin current.

   If we made a DOF name a string, we would have to (regexp-)parse
   data from it in various places.

   If we made a DOF name a 'name type parameter, we could not
   have DOFs with different type on the same mesh.

   This may suggest using some variadic type here.
   However, the major question is:
   how to do that in such a way that we retain all of our flexibility?

   What labels may there be? First of all, our DOF will have a proper name,
   like "T", or "M", or "Density", etc. Then, we may have additional
   name tags, as well as tensor indices for a variety of group
   representations. Therefore, the simplest way to do the most general case
   seems to be to use a generic string name as well as a vector of indices.
 *)

type dof_name = string * int array;;

type el_name = string;;
(* Same goes for these *)


(* The analytic functions we will have to deal with have the general structure

   coefficient*(sparse product-of dL_i/dx_j - some of them with higher powers)*(product of L_i)

   On those, we have to define addition, multiplication,
   differentiation.  These functions need not be especially fast, as
   the corresponding calculations are to be done only once for a given
   element type, but we want them easy to understand and convenient.

*)


(* dL powers
   Note: stylistic issue: I really do not like putting capital letters
   into names, but 'l' and '1' just look too similar...
 *)

type dL_power =
    {dlp_nr_L: int;
     dlp_nr_dx: int;
     dlp_pow: int;
   }
;;

type 'num femfun_summand =
    {ff_coefficient: 'num;
     ff_dL_powers: dL_power array; (* ordered by nr_L, dx, pow *)
     ff_L_powers: int array;
   }
;;

type 'num femfun = (int * 'num femfun_summand array);; 
(* The integer is the number of the femfun - we number them by order of occurrence *)

let ht_all_femfuns_by_fem = Hashtbl.create 17;; (* raw_femfun -> (nr,raw_femfun) = femfun *)
let ht_all_femfun_products = Hashtbl.create 17;; (* (nr1, nr2) -> (nr_product,raw_femfun) *)
let ht_all_femfun_linsubs = Hashtbl.create 17;; (* (nr1, nr_substituent, nr_L) -> (nr_result,raw_femfun) *)
let ht_all_femfun_powers = Hashtbl.create 17;; (* (nr, power) -> (nr_result,raw_femfun) *)
let ht_all_femfun_integrals = Hashtbl.create 17;; (* (nr, nr_L) -> (nr_result,raw_femfun) *)
let ht_all_femfun_surface_integrals = Hashtbl.create 17;; (* (dim, nr, nr_L) -> (nr_result,raw_femfun) *)
let ht_all_femfun_derivatives = Hashtbl.create 17;; (* (nr, nr_x) -> (nr_result,raw_femfun) *)

let registered_femfun raw_femfun =
  try
    Hashtbl.find ht_all_femfuns_by_fem raw_femfun
  with
    | Not_found ->
	let n = Hashtbl.length ht_all_femfuns_by_fem in
	let ff = (n,raw_femfun) in
	let () = Hashtbl.add ht_all_femfuns_by_fem raw_femfun ff in
	  ff
;;

(* In every FEM application, we will actually only encounter a few FEM
   functions - these are general N-th order shape functions on simplices.

   So, we do record them all in these tables.
*)


(* One problem with the dof data structure is that actually, we would
   like it to contain a dof_name field. This however would mean
   that we get into considerable trouble if we want to create "parallel"
   mwes, i.e. mwes where the corresponding data vectors can be added
   entry-wise.

   For example, we may want to have around H_ext, H_demag, H_exch in a
   micromagnetic simulation. It would be nice if we could mostly re-use
   the mwe data structure, only with re-namings in place on the dofs.

   This means that effectively, a dof can only be interpreted in the
   context of a mwe. In particular, it should neither reference
   its name stem, nor the elements it belongs to.
 *)

(* We want to be able to serialize MWEs. For this, we in particular have to serialize
   elements. Now, our element data structure is flexible enough to support things
   that go considerably beyond what can be constructed with make_element and fuse_elements
   alone, but for now, we restrict serializability to elements that can be built using such
   a path.
*)

(* This is internal: *)
type element_construction_plan =
  | ECP_alien (* We may encounter elements from Mars *)
  | ECP_empty (* The empty element is special *)
  | ECP_renamed	(* We are not specific on the renaming, as derived MWEs are not cached anyway. *)
  | ECP_make_element of (string*dof_name*int*int) (* el_name, dof_name, dim, order *)
  | ECP_fuse_elements of (string*element_construction_plan*element_construction_plan)
;;

type 'num element =
    {
     el_name: el_name;
     (* May change. We need this element identifier e.g.
	when forming stiffness matrices, so that we can identify
	elements / element combinations.

	Note: NO TWO DIFFERENT ELEMENTS MAY USE THE SAME NAME!
      *)
     el_subfields: dof_name array;
     (*
	The "subfields" contain information about a dof type and max indices,
	e.g. ("M",[|3|]) would tell us that magnetization is a 3-vector
	with components ("M",[|0|]),("M",[|1|]),("M",[|2|]).
	(XXX Need a better name for that. Note that this combines
	the old el_dof_type_names and el_dof_max_indices)
      *)
     el_dof_type_ranges: int array;
     (* This tells us which ranges of DOFs are same-type.
	Example: 2nd order mesh in 3 dimensions, 4+6 sites per simplex,
	two scalar fields, Phi on all 10 sites, Theta on the 4 vertices only.

	If el_dof_names are [|Phi;Phi;...;Phi;Theta;Theta;Theta;Theta|],
	this array will be [|10;14|]

	What do we need this for? Answer: inner products of dof functions
	are formed for same-type dofs only.
      *)
     el_dof_names: dof_name array;
     el_abssites: abssite array;
     el_abssite_index_by_dof_index: int array;
     el_dof_indices_by_abssite_index: int array array;
     el_dof_funs: 'num femfun array;
     el_made_by: element_construction_plan;
   };;


type dof =
    {
     dof_nr: int;
     dof_pos: coords;
     dof_site: site;
     (* ^ This is the mesh-interim-pos, e.g. [|17;17;23|] would mean:
	on the edge connecting vertex 17 with vertex 23,
	but twice as far from 23 as from 17.
	(Take weighted average of by-index-sorted vector of vertices)
      *)
     mutable dof_disappears_across: (simplex * int) list;
     mutable dof_sx_nel_ix_fillcount: int;
     mutable dof_sx_nel_ix: int array;     
     (* simplex, nr of element, nr of entry in the element.

	This originally was:
	(simplex * int * int) list

	But: we want to encode this as tight as possible, as this may
	eat a lot of memory (quite a bit of bookkeeping information
	for every degree of freedom).

	Can we encode (nr_simplex,nr_element,element_index) in a
	single int?  On 32-bit machines, OCaml integers give us
	sign+30 valid bits. Doing the math, we see that this is not
	really sufficient, so we use this scheme instead:
	
	We choose pairs of integers to represent one sx_nel_ix 
	entry, in this form:

        simplex-nr ; (nr_element<<16 + element_index)

	That should give us ample space.
     *)
     dof_in_body: simplex_region list;
     mutable dof_volume: float;
   }
;;


(* Truncate the sx_nel_ix array down to the length actually used *)
let dof_sx_nel_ix_truncate dof =
  dof.dof_sx_nel_ix <- Array.sub dof.dof_sx_nel_ix 0 (2*dof.dof_sx_nel_ix_fillcount)
;;

let dof_sx_nel_ix_extend ?(extra=25) dof =
  let v_old = dof.dof_sx_nel_ix in
  let len_old = Array.length v_old in
  let len_new = len_old+2*extra in
  let v_new = Array.init len_new (fun n -> if n<len_old then v_old.(n) else 0) in
  let () = dof.dof_sx_nel_ix <- v_new in
    ()
;;

let dof_sx_nel_ix_add dof sx nel ix =
  let fillcount = dof.dof_sx_nel_ix_fillcount in
  let () = 
    (if 2*fillcount = Array.length dof.dof_sx_nel_ix
     then dof_sx_nel_ix_extend dof
     else ())
  in
    begin
      dof.dof_sx_nel_ix.(2*fillcount) <- sx.ms_id;
      dof.dof_sx_nel_ix.(2*fillcount+1) <- (nel lsl 16) + ix;
      dof.dof_sx_nel_ix_fillcount <- 1+fillcount;
    end
;;

let dof_sx_nel_ix_iter simplices dof f =
  let sx_nel_ix = dof.dof_sx_nel_ix in
  let fillcount = dof.dof_sx_nel_ix_fillcount in
    for i=0 to fillcount-1 do
      let sx = simplices.(sx_nel_ix.(2*i)) in
      let nel_ix = dof.dof_sx_nel_ix.(2*i+1) in
      let ix = nel_ix land 0xffff in
      let nel = nel_ix lsr 16 in
	f sx nel ix
    done
;;

let dof_sx_nel_ix_find simplices dof pred =
  let sx_nel_ix = dof.dof_sx_nel_ix in
  let fillcount = dof.dof_sx_nel_ix_fillcount in
  let rec walk n = 
    if n = fillcount then raise Not_found
    else
      let sx = simplices.(sx_nel_ix.(2*n)) in
      let nel_ix = dof.dof_sx_nel_ix.(2*n+1) in
      let ix = nel_ix land 0xffff in
      let nel = nel_ix lsr 16 in
	if pred sx nel ix
	then (sx,nel,ix)
	else walk (n+1)
  in walk 0
;;


let dof_sx_nel_ix_reduce simplices dof f x0 =
  let sx_nel_ix = dof.dof_sx_nel_ix in
  let fillcount = dof.dof_sx_nel_ix_fillcount in
  let rec walk n x = 
    if n = fillcount then x
    else
      let sx = simplices.(sx_nel_ix.(2*n)) in
      let nel_ix = dof.dof_sx_nel_ix.(2*n+1) in
      let ix = nel_ix land 0xffff in
      let nel = nel_ix lsr 16 in
	walk (1+n) (f sx nel ix x)
  in walk 0 x0
;;


let dof_name_to_string dof_name =
  let (name,indices)=dof_name in
  if Array.length indices = 0
  then name
  else
    let indices_str = String.concat ", " (Array.to_list (Array.map string_of_int indices)) in
    Printf.sprintf "%s[%s]" name indices_str
;;


(* Note that quite a lot of the bookkeeping information in a MWE is redundant.
   But that is just the purpose: make things available without having to do
   all kinds of complicated processing.

   XXX Note: mesh_with_elements should be renamed to field_layout (with tag mwe_XYZ -> flo_XYZ)
*)

type 'a mwe_made_by =
  | MWEMB_make_mwe of 
      (string *
       (int * string array) array * (* properties by region *)
       (int array) * (* This replaces fun_outer_region: outer region array indexed by site-id, 0=not an outer site *)
       (Mesh.simplex -> 'a element))
  | MWEMB_mwe_sibling of
      (string * string * (string * string) array * string)
	(* This gets a little bit tricky: the last string replaces the
           MWE we derive from: we have to specify this by name here.
	   Within every nsim linalg_machine, these MWEs are uniquely specified
	   by their name - remember, they have to, because this is how we
	   then create field vectors!
	*)
;;

(* XXX NOTE: this is in flux: the mwe_sites_simplex_info_pmid data
   structure is quite large and contains data that may be rather useful,
   but is very rarely needed, and only in quite obscure situations. So,
   we try to find a way to reduce our memory footprint by only computing it
   when we have to. The general situation here seems to indicate we should
   consider a re-design of this part of the data structure...

   Every mesh with elements will have to have information available on
   which site belongs to which simplex, with what abssite index. This is
   a complicated piece of information that will take up a lot of memory,
   but is needed quite rarely. When it is needed, however, we do need it
   a lot. So, our strategy is to wrap that up in such a way that we
   retain the closure that produced it, and destroy the data structure
   once we no longer need it and have pressing memory issues, rebuilding
   it when we use it again.

   NOTE: this massive and complicated data structure actually seems
   not to be used much at all - after all, if we set up the whole thing,
   then each DOF will know about its shape function anyway, so there is 
   little reason why the *sites* should retain knowledge about which simplex
   and which abssite they belong to...
*)

type ssip_body = (site * (int array) * string * int) array;;
type hibernatable_sites_simplex_info_pmid = ((unit -> ssip_body) * (ssip_body option ref))


type 'num mesh_with_elements =
    {mwe_mesh: mesh;
     mwe_made_by: 'num mwe_made_by;
     mwe_name: string; (* This is for displaying (also, of fields), as well as for creating petsc names. *)
     mwe_elements: 'num element array;
     mwe_is_primary: bool;
     mwe_subfields: dof_name array;
     mwe_properties_by_region: (Mesh.simplex_region,string array) Hashtbl.t;
     mwe_sites: site array;
     mwe_pmids: int array; (* Array parallel to mwe_sites, containing Primary master IDs of sites,
			      for periodic boundary condition support. *)
     mwe_sites_simplex_info_pmid: hibernatable_sites_simplex_info_pmid;
     (* For all the sites in the MWE, the info part tells us about:
	- the simplex indices this site belongs to (int array)
	- the corresponding abssite indices (in the form of a string "parallel" to the above int array)
	- the periodic master id, for periodically identified DOFs.
     *)
     mwe_dofs: dof array; (* A vector of all the DOFs *)
     mwe_dof_periodic_images: int array array;
     (* which dof also appears where else? length: nr_dofs, non-periodic entries: [||]
     *)
     (* Note that among the nontrivial entries in mwe_dof_periodic_images,
	there are some that are special: those where pi.(j).(0) = j, i.e. those 
	where the leading dof index in the array is the index of the group, so this is
	"the master dof's home entry" for periodicity info. We use these entries to
	"equalize" fields.
     *)
     mwe_dofs_by_site: (site,dof array) Hashtbl.t; (* Note: in there, the DOFs are sorted by DOF id. *)
     mwe_nel_by_simplex_index: int array; (* Element number by simplex index *)
     mwe_dof_nrs_by_simplex_index: int array array;
     (* Note: the G-matrix related fields should be refs.
	Reason: if we derive mwes #2 and #3 from mwe #1,
	and #1 initially did not have G initialized, and
	later on we lazily compute G for mwe #2, it should
	also become available for #1 and #3 as well.
     *)
     (* Note: the entries below are "ref" and not "mutable" for better
	sharing between primary and derived mwes!
     *)
     mwe_dof_neighbour_pairs: (int array * int array) array option ref;
     (* This is tricky: the "option ref" behaviour is as below; 
	the outermost array is indexed by subfield index,
	and the inner arrays are parallel and give all pairs of dofs
	which have all tensor indices =0 that belong to neighbouring sites.
     *)
     (* NOTE: these entries -- mwe_dof_funs_g,mwe_dof_funs_g_solver,mwe_dof_funs_g_entries --
	are relics that have become obsolete.
     *)
     mwe_dof_funs_g: Mpi_petsc.matrix option ref;
     mwe_dof_funs_g_solver:
       (?output:Mpi_petsc.vector -> input:Mpi_petsc.vector ->
	 unit -> Mpi_petsc.vector) option ref;
     mwe_dof_funs_g_entries: (int * int * float) array ref;
     (* row, col, value - may be empty if it was not initialized yet.

	(1) Do we really need this? As long as we only consider caching,
	we may implement a scheme that works without.

	(2) Should this really be an array? We are quite limited
	in the number of entries an array may have (~2M, meaning ~200000 DOFs)!
     *)
     mwe_dof_funs_volumes: float array option ref;
     (* These inverse volumes are needed to approximately map a co-field to a field - 
	if we want to stick with sparse operators at the expense of some mathematical
	cleanliness...
      *)
     mwe_distribution: int array;
     (* How to distribute entries across multiple nodes -
	might be superseded/obsoleted by the following entry 
	sometime in the future (presumably not, as we also use it
	to read off the cvode vector distribution): *)
     mwe_dofs_and_distribution_by_dvss:
       (dvss,dof_mapping_long_to_short*dof_mapping_short_to_long*distribution_information)
       Hashtbl.t;
     (* Note: this Hashtbl is also shared across siblings. *)
    };;

let get_mwe_sites_simplex_info_pmid mwe =
  let (f,r_data) = mwe.mwe_sites_simplex_info_pmid in
    match !r_data with
      | Some x -> x
      | None ->
	  let data = f () in
	  let () = (r_data := Some data) in
	    data
;;

let slim_mwe_sites_simplex_info_pmid mwe =
  let (f,r_data) = mwe.mwe_sites_simplex_info_pmid in
    r_data := None
;;


(* OBSOLETE (superseded by dvss): *)
type opt_field_restriction = dof_region_spec option;;

(* Note: the field/cofield distinction presumably is to go away soon as well: *)
type 'num fem_field = FEM_field of
  'num mesh_with_elements * dvss * Mpi_petsc.vector;;

type 'num fem_cofield = FEM_cofield of
  'num mesh_with_elements * dvss * Mpi_petsc.vector;;	   

let the_dof_name mwe dof = (* Note that every dof must belong to some element! *)
  let nel_ix = dof.dof_sx_nel_ix.(1) in
    (* Note: this entry MUST be present, otherwise the dof does not belong to any MWE site! *)
  let ix = nel_ix land 0xffff in
  let nr_el = nel_ix lsr 16 in
  let elem = mwe.mwe_elements.(nr_el) in
    elem.el_dof_names.(ix)
;;

let the_dof_femfun mwe match_sx dof =
  let fillcount = dof.dof_sx_nel_ix_fillcount in
  let rec walk n =
    if n = fillcount then impossible ()
    else
      let sx_id = dof.dof_sx_nel_ix.(2*n)
      and nel_ix = dof.dof_sx_nel_ix.(2*n+1) in
      let ix = nel_ix land 0xffff in
      let nr_el = nel_ix lsr 16 in
	if sx_id <> match_sx.ms_id then walk (1+n)
	else
	  let elem = mwe.mwe_elements.(nr_el) in
	    elem.el_dof_funs.(ix)
  in walk 0
;;

let the_dof_machine mwe dof =		(* get the parallel node a given DOF is on *)
  let distrib = mwe.mwe_distribution in
  let rec walk rest_dof_ix nr_machine =
    if rest_dof_ix < distrib.(nr_machine)
    then nr_machine
    else walk (rest_dof_ix-distrib.(nr_machine)) (1+nr_machine)
  in walk dof.dof_nr 0
;;

let dof_belongs_to mwe dof region_logic =
  let properties_by_region = mwe.mwe_properties_by_region in
  let no_props=[||] in
  let regions = dof.dof_in_body in
  let region_has_property r p = 
    let props = try Hashtbl.find properties_by_region r with | Not_found -> no_props in
    let nr_props = Array.length props in
    let rec check_prop n =
      if n = nr_props then false
      else if p = props.(n) then true 
      else check_prop (1+n)
    in check_prop 0
  in
  let rec
      dlog_and xs =
    let rec walk rest =
      match rest with
	| [] -> true
	| (head::tail) -> if dlog head then walk tail else false
    in walk xs
  and
      dlog_or xs =
    let rec walk rest =
      match rest with
	| [] -> false
	| (head::tail) -> if dlog head then true else walk tail
    in walk xs
  and
      dlog_not x = not (dlog x)
  and 
      dlog_nregions n = List.length regions = n
  and
      dlog_all str = 
    let rec walk rest =
      match rest with 
	| [] -> true
	| (head::tail) -> if region_has_property head str then walk tail else false
    in walk regions
  and
      dlog_some str = 
    let rec walk rest =
      match rest with 
	| [] -> false
	| (head::tail) -> if region_has_property head str then true else walk tail
    in walk regions
  and dlog x =
    match x with
      | Ddiffop_parser.DLOG_true -> true
      | Ddiffop_parser.DLOG_and x -> dlog_and x
      | Ddiffop_parser.DLOG_or x -> dlog_or x
      | Ddiffop_parser.DLOG_not x -> dlog_not x
      | Ddiffop_parser.DLOG_all x -> dlog_all x
      | Ddiffop_parser.DLOG_some x -> dlog_some x
      | Ddiffop_parser.DLOG_nregions x -> dlog_nregions x
  in dlog region_logic
;;

let dvss_constraint mwe dof dvss =
  let result =
    match dvss with
      | None -> true
      | Some a ->
	  let (stem,indices) = the_dof_name mwe dof in
	  let p = array_position_if (fun (n,_) -> n=stem) a 0 in
	    if p = (-1) then false
	    else let (_,logic) = a.(p) in
	      dof_belongs_to mwe dof logic
  in
    (*
      let ddd =
      (if dof.dof_nr mod 100 = 0
      then
      Printf.printf
      "DDD dvss_constraint CPU %d dof %s #%d dvss %s => %s\n%!"
      (Mpi_petsc.comm_rank (Mpi_petsc.petsc_get_comm_world()))
      (dof_name_to_string (the_dof_name mwe dof))
      dof.dof_nr
      (dvss_to_string dvss)
      (if result then "#T" else "#F")
      else ())
      in
    *)
    result
;;

let mwe_shortvec_info mwe dvss =
    try Hashtbl.find mwe.mwe_dofs_and_distribution_by_dvss dvss
    with
      | Not_found ->
	  let ht_dof_nrs = Hashtbl.create 100 in
	  (* let ddd = Printf.printf "DDD CPU %d mwe \"%s\" distrib %s\n%!" (Mpi_petsc.comm_rank (Mpi_petsc.petsc_get_comm_world())) mwe.mwe_name (int_array_to_string mwe.mwe_distribution) in *)
	  let distrib = Array.make (Array.length mwe.mwe_distribution) 0 in
	  let () = 
	    array_foreach_do mwe.mwe_dofs
	      (fun dof ->
		 if dvss_constraint mwe dof dvss 
		 then
		   if Hashtbl.mem ht_dof_nrs dof.dof_nr 
		   then ()
		   else
		     let () = Hashtbl.add ht_dof_nrs dof.dof_nr true in
		     let m = the_dof_machine mwe dof in
		       distrib.(m) <- 1+distrib.(m)
		 else ())
	  in
	  let v_short_to_long = hashtbl_keys ~sorter:compare ht_dof_nrs in
	  let v_long_to_short = Array.make (Array.length mwe.mwe_dofs) (-1) in
	  let () = Array.iteri (fun nr_short nr_long -> v_long_to_short.(nr_long) <- nr_short) v_short_to_long in
	  let entry = (v_long_to_short,v_short_to_long,distrib) in
	  let canonical_entry = ref entry in
	  let () = hashtbl_foreach_do mwe.mwe_dofs_and_distribution_by_dvss 
	    (fun _ ((_,old_stl,_) as old_entry) ->
	       if old_stl = v_short_to_long then canonical_entry := old_entry else ())
	  in
	  let () = Hashtbl.add mwe.mwe_dofs_and_distribution_by_dvss dvss (!canonical_entry)
	  in
	  let result = !canonical_entry in
	  let ddd = 
	    let (lts,stl,distrib) = result in
	      logdebug (Printf.sprintf "mwe_shortvec_info (CPU %d mwe: %s dvss: %s): (lts=#[array length=%d],stl=#[array length=%d],distrib=%s"
			  (Mpi_petsc.comm_rank (Mpi_petsc.petsc_get_comm_world()))
			  mwe.mwe_name
			  (dvss_to_string dvss)
			  (Array.length lts) (Array.length stl) (int_array_to_string distrib))
	  in
	    result
;;

let boundary_shortvec_info dof_stem mwe boundary_spec = 
  let the_region_logic = 
    parse_or_error
      Ddiffop_lexer.token
      Ddiffop_parser.region_logic (* parser entry point *)
      boundary_spec
  in
  mwe_shortvec_info mwe (Some [|(dof_stem,the_region_logic)|])
;;


let mwe_shortvec_selections_match mwe sel1 sel2 =
  if sel1=sel2 then true
  else
    let (x,_,_) = mwe_shortvec_info mwe sel1 in
    let (y,_,_) = mwe_shortvec_info mwe sel2 in
      (x == y) || (x=y)
;;

let ensure_field_is_unrestricted (FEM_field (mwe,restr,_)) =
  let (_,stl,_) = mwe_shortvec_info mwe restr in
    if Array.length stl = Array.length mwe.mwe_dofs then ()
    else failwith "FEM_field MUST NOT have DOF restrictions on it!"
;;

let ensure_cofield_is_unrestricted (FEM_cofield (mwe,restr,_)) =
  let (_,stl,_) = mwe_shortvec_info mwe restr in
    if Array.length stl = Array.length mwe.mwe_dofs then ()
    else failwith "FEM_cofield MUST NOT have DOF restrictions on it!"
;;


let femfun_zero=registered_femfun [||];;


(* Provided for the interface, but not used below *)
let make_bare_femfun dim nr =
  registered_femfun
    [|{ff_coefficient=1.0;
       ff_dL_powers=[||];
       ff_L_powers=Array.init (1+dim) (fun n -> if n=nr then 1 else 0);
      }|]
;;

(* Provided for the interface, but not used below *)
let dL_as_femfun nr_vertices nr_L dx =
  registered_femfun
    [|{ff_coefficient=1.0;
       ff_dL_powers=[|{dlp_nr_L=nr_L;dlp_nr_dx=dx;dlp_pow=1}|];
       ff_L_powers=Array.make nr_vertices 0;
      }|]
;;


(* Note: here we assume that the folded-out DOFs are in the proper order as suggested
   by just folding out and combining el_subfields.

   When two elements are compatible, they differ only in the names
   that have been given to DOFs. In particular, if two MWEs are compatible,
   their data vectors may sensibly be added point-wise.

   XXX Conceptual question: does something like that really make that
   much sense, or wouldn't it be much better to only have a single,
   simple "fields_site_wise" C-interfacing function that can handle
   even things like adding fields? After all, this renaming business
   is somewhat strange, as we may have MWEs that "basically should be
   equivalent, but are not due to some element ordering problems".

   (of course, it at least makes sense to the degree that we should avoid
   re-computation of the G matrix... And furthermore, in order to share that,
   it should show up in the form of a reference, so that lazily making the G
   matrix for one mwe will also make it available for the others!
   )

   -> XXX Design decision: a mwe must list its elements in order of occurrence when
   one goes through the mesh simplices.

   XXX Note further that we have to pay attention to the constraint
   that no two different elements may share the same name!

   XXX Also note that some cases of confusion just cannot be avoided.
   For example, if we create element A, which holds two vector dofs,
   and later on element B, which holds the same two vector dofs
   in reverse order, these will be comptible (technically speaking),
   but adding fields will produce great nonsense. This intrinsic ambiguity
   in the order of constituents of an element lies at the heart of quite some
   relabeling problems.

   Another issue: relabeling of MWEs and disk caching. If we have
   computed the G matrix, we would (usually) like to be able to cache
   it. Now how does this relate to having multiple MWEs around, which
   may share G matrices? We certainly want to avoid the destruction of the
   sharing through saving/loading of MWEs.

   Ad hoc idea: introduce an extra field, which may designate a MWE as
   "primary" or "derived/logical". We enforce the additional
   constraint that only primary MWEs can be used in renaming to derive
   logical ones.

   Only primary MWEs are cached. For the derived ones,
   we just redo the renaming.

   This is a little bit inconvenient for the user (because he has to
   worry about one additional concept), but makes things much simpler
   and straightforward for us implementors.

   (If we do it that way, we can also demand that the user specifies
   an uniform renaming element name prefix - to enforce name uniqueness
   for elements.)
*)

let elements_are_compatible e1 e2 =
       (e1.el_abssites = e2.el_abssites)
    && (e1.el_dof_funs = e2.el_dof_funs)
    && (e1.el_abssite_index_by_dof_index = e2.el_abssite_index_by_dof_index)
    && (e1.el_dof_indices_by_abssite_index = e2.el_dof_indices_by_abssite_index)
    && (e1.el_dof_type_ranges = e2.el_dof_type_ranges)
    && (array_compare_pointwise
	  (fun (_,mi1) (_,mi2) -> mi1 = mi2)
	  e1.el_subfields e2.el_subfields)
;;

let make_element
    ?element_name
    dim (dof_name_stem,dof_max_indices) order =
  let the_element_name =
    match element_name with
    | None -> Printf.sprintf "%s[ord=%d]" dof_name_stem order
    | Some x -> x
  in
  let nr_vertices=1+dim in
  let nr_dofs_per_abssite = Array.fold_left ( * ) 1 dof_max_indices in
  let abssites_not_gcd_reduced = all_distributions_to_buckets nr_vertices order in
  let nr_abssites = Array.length abssites_not_gcd_reduced in
  let nr_dofs_total = nr_dofs_per_abssite * nr_abssites in
  (* Layout of same-type DOFs is:
     first, all at abssite 0, then, all at abssite 1, ... *)
  let dof_names_one_site =
    array_multiinit dof_max_indices
      (fun _ v_indices -> (dof_name_stem, Array.copy v_indices))
  in
  let dof_names = array_repeat_ntimes nr_abssites dof_names_one_site in
  let dof_type_ranges = [|nr_dofs_total|] in
  let abssite_index_by_dof_index =
    Array.init nr_dofs_total (fun n -> n / nr_dofs_per_abssite)
  in
  let dof_indices_by_abssite_index =
    Array.init nr_abssites
      (fun nr_abssite ->
	let offset = nr_abssite*nr_dofs_per_abssite in
	Array.init nr_dofs_per_abssite
	  (fun nr_dof_this_abssite -> offset+nr_dof_this_abssite))
  in
  let abssite_funs =
    let raw_polynomials = abssites_not_gcd_reduced in
    let denom = 1.0/.(float_of_int order) in
    let eval_polynomial_at_point poly_desc point_desc =
      let rec work n so_far =
	if n = nr_vertices
	then so_far
	else
	  let contrib =
	    (denom*.float_of_int (point_desc.(n)))
	      **(float_of_int poly_desc.(n))
	  in work (n+1) (so_far*.contrib)
      in work 0 1.0
    in
    let mx_pointval_poly =
      Array.map
	(fun pt ->
	  Array.map
	    (fun poly -> eval_polynomial_at_point poly pt)
	    raw_polynomials)
	abssites_not_gcd_reduced
    in
    let (_,mx_poly_coeffs) =
      det_and_inv nr_abssites mx_pointval_poly in
    let result =
      Array.init nr_abssites
	(fun nr_fun ->
	  let ht = Hashtbl.create 10 in
	  begin
	    for nr_poly=0 to nr_abssites-1 do
	      let coeff = mx_poly_coeffs.(nr_poly).(nr_fun) in
	      hashtbl_increase (+.) ht raw_polynomials.(nr_poly) coeff;
	    done;
	    array_filter (fun x -> x.ff_coefficient<>0.0)
	      (map_hashtbl_to_array
		 ~sorter:(fun s1 s2 -> compare (s1.ff_dL_powers, s1.ff_L_powers) (s2.ff_dL_powers, s2.ff_L_powers))
		 (fun raw_poly coeff ->
		   {ff_coefficient=coeff;
		    ff_dL_powers=[||];
		    ff_L_powers=raw_poly;
		  })
		 ht)
	  end)
    in Array.map registered_femfun result
  in
  let dof_funs = Array.init nr_dofs_total
      (fun n ->
	let nr_abssite = n / nr_dofs_per_abssite in
	abssite_funs.(nr_abssite))
  in
  {
   el_name = the_element_name;
   el_subfields = [|(dof_name_stem,dof_max_indices)|];
   el_dof_type_ranges = dof_type_ranges;
   el_dof_names = dof_names;
   el_abssites = Array.map array_gcd_reduced abssites_not_gcd_reduced;
   el_abssite_index_by_dof_index = abssite_index_by_dof_index;
   el_dof_indices_by_abssite_index = dof_indices_by_abssite_index;
   el_dof_funs = dof_funs;
   el_made_by = ECP_make_element (the_element_name,(dof_name_stem,dof_max_indices),dim,order);
  }
;;

(* This is actually used internally,
   when we rename all the elements in an MWE
   to generate another MWE.
 *)

let _element_rename_dofs prefix renamings elem = (* renamings is [|(old_name_stem,new_name_stem)|] *)
  let rename old_name =
    try
      let (_,new_name) = array_find (fun (k,_) -> k = old_name) renamings in
	new_name
    with | Not_found -> old_name
  in
  let rename_dof (n,ix) = (rename n,ix) in
  {
   el_name = Printf.sprintf "%s:%s" prefix elem.el_name;
   el_subfields = Array.map rename_dof elem.el_subfields;
   el_dof_type_ranges = elem.el_dof_type_ranges;
   el_dof_names = Array.map rename_dof elem.el_dof_names;
   el_abssites = elem.el_abssites;
   el_abssite_index_by_dof_index = elem.el_abssite_index_by_dof_index;
   el_dof_indices_by_abssite_index = elem.el_dof_indices_by_abssite_index;
   el_dof_funs = elem.el_dof_funs;
   el_made_by = (if elem.el_made_by = ECP_empty then ECP_empty else ECP_renamed);
 }
;;

(* What is an empty element good for?

   Consider e.g. meshing a magnetic material which does not have
   magnetism DOFs outside the relevant region. We nevertheless have
   to associate elements to the outer simplices.
 *)

let empty_element =
  {el_name = "Empty";
   el_subfields = [||];
   el_dof_type_ranges = [||];
   el_dof_names = [||];
   el_abssites = [||];
   el_abssite_index_by_dof_index = [||];
   el_dof_indices_by_abssite_index = [||];
   el_dof_funs = [||];
   el_made_by = ECP_empty;
 }
;;

(* Sometimes we have to know whether an element is the empty element.
   Neither fuse_elements nor make_element can create an empty element,
   so we can read this off by inspecting its made_by plan.
*)
let element_is_empty elem = elem.el_made_by = ECP_empty;;


(* Note that we do not have to support re-naming DOFs here.
   If you have to rename, you are fusing the wrong elements!

   Further note that fusing does nothing when operating on the empty element.
   In particular, the name argument is ignored.
*)

let fuse_elements ?name e1 e2 =
  if element_is_empty e1 then e2 else
  if element_is_empty e2 then e1 else
  (* Now, we may assume that e1 actually does have entries! *)
  let fused_name =
    match name with
    | None -> Printf.sprintf "%s;%s" e1.el_name e2.el_name
    | Some n -> n
  in
  let fused_subfields = Array.append e1.el_subfields e2.el_subfields in
  let fused_dof_type_ranges =
    let offset = e1.el_dof_type_ranges.(Array.length e1.el_dof_type_ranges -1) in
    Array.append
      e1.el_dof_type_ranges
      (Array.map (fun n -> n+offset) e2.el_dof_type_ranges)
  in
  let fused_dof_names = Array.append e1.el_dof_names e2.el_dof_names in
  let fused_abssites =
    (* It really does not matter if we use sub-optimal algorithms here. *)
    array_uniqify [|e1.el_abssites;e2.el_abssites|] in
  let fused_nr_abssites = Array.length fused_abssites in
  let abssite_index_in_fused abssite = array_position abssite fused_abssites 0 in
  let fused_abssite_index_by_dof_index =
    array_join
      [|Array.map
	  (fun ix -> abssite_index_in_fused e1.el_abssites.(ix))
	  e1.el_abssite_index_by_dof_index;
	Array.map
	  (fun ix -> abssite_index_in_fused e2.el_abssites.(ix))
	  e2.el_abssite_index_by_dof_index;
      |]
  in
  let fused_dof_indices_by_abssite_index =
    Array.init fused_nr_abssites
      (fun abssite_ix ->
	array_nums_with_property
	  (fun n -> fused_abssite_index_by_dof_index.(n) = abssite_ix)
	  (Array.length fused_dof_names))
  in
  let fused_dof_funs = Array.append e1.el_dof_funs e2.el_dof_funs
  (* Here, I just do not care about duplication of data. *)
  in
  {
   el_name=fused_name;
   el_subfields=fused_subfields;
   el_dof_type_ranges=fused_dof_type_ranges;
   el_dof_names=fused_dof_names;
   el_abssites=fused_abssites;
   el_abssite_index_by_dof_index=fused_abssite_index_by_dof_index;
   el_dof_indices_by_abssite_index=fused_dof_indices_by_abssite_index;
   el_dof_funs=fused_dof_funs;
   el_made_by = ECP_fuse_elements (fused_name,e1.el_made_by,e2.el_made_by);
 }
;;

let element_to_string el =
  Printf.sprintf "#<element '%s', dof_names=%s>" el.el_name (string_array_to_string (Array.map dof_name_to_string el.el_dof_names))
;;

(* Some example elements *)
(*
let element_T = make_element 3 ("Temperature",[||]) 1;;
let element_M = make_element 3 ("Magnetization",[|3|]) 2;;
let element_TM = fuse_elements element_T element_M;;

let element_Js = make_element 3 ("Spin Current",[|3;3|]) 1;;
*)

let mwes_are_compatible ?(restr1=None) ?(restr2=None) mwe1 mwe2 =
  mwe1.mwe_mesh == mwe2.mwe_mesh
  && (mwe1.mwe_nel_by_simplex_index = mwe2.mwe_nel_by_simplex_index)
  && (array_compare_pointwise elements_are_compatible
	mwe1.mwe_elements mwe2.mwe_elements)
  &&
  ((restr1 = restr2) ||
     (let (_,stl1,_) = mwe_shortvec_info mwe1 restr1 in
      let (_,stl2,_) = mwe_shortvec_info mwe2 restr2 in
	stl1 = stl2))
;;


(* === Helpers === *)

let femfun_to_string (nr,f) =
  let body =
    String.concat "\n "
      (Array.to_list
	 (Array.map
	    (fun {ff_L_powers=f_L; ff_dL_powers=f_dL; ff_coefficient=f_c;} ->
	      Printf.sprintf "%s%10.5f %s %s" (if f_c > 0.0 then "+" else "-") (abs_float f_c)
		(String.concat " "
		   (Array.to_list
		      (array_mapfilteri
			 (fun n pow ->
			   if pow = 0 then None
			   else
			     Some
			       (if pow=1 then Printf.sprintf "L_%d" n
			       else Printf.sprintf "L_%d^%d" n pow))
			 f_L)))
		(String.concat " "
		   (Array.to_list
		      (Array.map
			 (fun {dlp_nr_L=nr_L;dlp_nr_dx=nr_dx;dlp_pow=pow} ->
			   if pow=1 then Printf.sprintf "(dL_%d/dx_%d)" nr_L nr_dx
			   else Printf.sprintf "(dL_%d/dx_%d)^%d" nr_L nr_dx pow)
			 f_dL))))
	    f))
  in Printf.sprintf "#<FEM fun #%d\n %s\n>" nr body
;;

let mwe_to_string mwe =
  Printf.sprintf "#<%d-dim mesh with elements '%s', %d vertices, %d simplices, %d DOFs, subfields: %s>"
    mwe.mwe_mesh.mm_dim
    mwe.mwe_name
    (Array.length mwe.mwe_mesh.mm_points)
    (Array.length mwe.mwe_mesh.mm_simplices)
    (Array.length mwe.mwe_dofs)
    (String.concat ", " (Array.to_list (Array.map dof_name_to_string mwe.mwe_subfields)))
;;

let print_mwe fmt mwe =
  Format.fprintf fmt "%s" (mwe_to_string mwe)
;;

(* In the end, we will have to evaluate integrals over products of powers of the L_j.

   I suppose that the proper generalization of formula (3.24) from
   Adam Spargo's dissertation is

   Int_V L_0^(p_0) L_1^(p_1) ... L_dim^(p_dim)

   =   (dim! * V * p_0! * p_1! * ... * p_dim!)
     / (p_0+p_1+ ... + p_dim + dim)!

   XXX this has to be checked! (Should not be too difficult.)

   If we take out the factor V, the rest is purely combinatorical.

   Note: the factor (float_factorial dim) also is artificial. This is
   just the factor between a simplex volume and a jacobi
   determinant. We do not have this and directly multiply with the
   Jacobi determinant later on.
*)

let element_shape_function_power_integral_factor shape_function_powers =
  let nr_shape_funs = Array.length shape_function_powers in
  let dim = nr_shape_funs-1 in
  let rec walk pos sum_powers numerator_now =
    if pos = nr_shape_funs
    then
      (* (float_factorial dim)*. *)
      numerator_now/.(float_factorial (dim+sum_powers))
    else
      walk
	(pos+1)
	(sum_powers+shape_function_powers.(pos))
	(numerator_now*.(float_factorial shape_function_powers.(pos)))
  in
  let result = walk 0 0 1.0 in
    result
;;

(* Note: Maybe we can make this faster via lambda lifting... maybe not. *)
let femfun_eval mx_dL (_,femfun) point_L_coords =
  let eval_dL dL_factors =
    let nr_factors = Array.length dL_factors in
    let rec walk pos now =
      if pos=nr_factors then now
      else let next = dL_factors.(pos) in
      walk (1+pos) now*.((mx_dL.(next.dlp_nr_L).(next.dlp_nr_dx))**(float_of_int next.dlp_pow))
    in walk 0 1.0
  in
  let eval_L powers =
    let nr_L = Array.length powers in
    let rec walk pos now =
      if pos = nr_L then now
      else walk (1+pos) now*.(point_L_coords.(pos)**(float_of_int powers.(pos))) in
    walk 0 1.0
  in
  let nr_summands = Array.length femfun in
  let rec walk_summands nr_summand now =
    if nr_summand = nr_summands then now
    else
      let this_summand = femfun.(nr_summand) in
      let coeff = this_summand.ff_coefficient in
      let factor_dL = eval_dL this_summand.ff_dL_powers in
      let factor_L = eval_L this_summand.ff_L_powers in
      walk_summands (1+nr_summand)
	(now+.coeff*.factor_dL*.factor_L)
  in
  walk_summands 0 0.0
;;

(* Going via a hash is a somewhat brute method,
   especially as we have both vectors sorted.
   But it's simple and easy to implement.
 *)
let fuse_dL_powers dlp1 dlp2 =
  let nr_dlp1 = Array.length dlp1 in
  let nr_dlp2 = Array.length dlp2 in
  let ht_fused = Hashtbl.create (max nr_dlp1 nr_dlp2) in
  let insert_from_array a =
    Array.iter (fun {dlp_nr_L=x1;dlp_nr_dx=x2;dlp_pow=x3} -> hashtbl_increase (+) ht_fused (x1,x2) x3) a
  in
  let () = insert_from_array dlp1 in
  let () = insert_from_array dlp2 in
  map_hashtbl_to_array
    ~sorter:compare (fun (x1,x2) x3 -> {dlp_nr_L=x1;dlp_nr_dx=x2;dlp_pow=x3}) ht_fused
;;

let _femfun_from_hash ht =
  registered_femfun
    (map_hashtbl_to_array
       ~sorter:compare
       (fun (x1,x2) x3 ->
	  {ff_L_powers=x1;
	   ff_dL_powers=x2;
	   ff_coefficient=x3;}) ht)
;;

(* NOTE: as femfun_add is not used but only provided for the interface,
   it is not yet wrapped up to use table lookups to speed up computation!
*)
let femfun_add ?(scale1=1.0) ?(scale2=1.0) (_,f1) (_,f2) =
  let ht_result = Hashtbl.create (max (Array.length f1) (Array.length f2)) in
  let enter_summands factor summands =
    Array.iter
      (fun {ff_L_powers=x1; ff_dL_powers=x2; ff_coefficient=x3;} ->
	hashtbl_increase (+.) ht_result (x1,x2) (factor*.x3))
      summands
  in
  let () = enter_summands scale1 f1 in
  let () = enter_summands scale2 f2 in
  _femfun_from_hash ht_result
;;

let _raw_femfun_mult f1 f2 =
  let ht_result = Hashtbl.create (max (Array.length f1) (Array.length f2)) in
  let () =
    Array.iter
      (fun {ff_L_powers=f1_L; ff_dL_powers=f1_dL; ff_coefficient=f1_c;} ->
	Array.iter
	  (fun {ff_L_powers=f2_L; ff_dL_powers=f2_dL; ff_coefficient=f2_c;} ->
	    hashtbl_increase (+.) ht_result
	      (array_pointwise (+) f1_L f2_L,fuse_dL_powers f1_dL f2_dL)
	      (f1_c*.f2_c)) f2) f1
  in
  _femfun_from_hash ht_result
;;

let femfun_mult (n1,f1) (n2,f2) =
  try
    Hashtbl.find ht_all_femfun_products (n1,n2)
  with
    | Not_found ->
	_raw_femfun_mult f1 f2
;;

let femfun_power dim femfun pow =
  let rec walk ff n =
    if n=0
    then registered_femfun
      [|{ff_L_powers=Array.make (dim+1) 0;ff_dL_powers=[||];ff_coefficient=1.0}|]
    else if n=1 then ff
    else
      let ffn1 = walk ff (n-1) in
      let ffn = femfun_mult ffn1 ff in
	ffn
  in
  let (nr_ff,_) = femfun in
    try
      Hashtbl.find ht_all_femfun_powers (nr_ff,pow)
    with
      | Not_found ->
	  let result = walk femfun pow in
	  let () = Hashtbl.add ht_all_femfun_powers (nr_ff,pow) result
	  in result
;;

(* When we integrate over faces, we have to do two things:
 (1) set one of the L-functions to zero, (2) make a linear substitution
 for another L-function: L3 = 1-L1-L2.

 Note that we can easily do both if we just have a notion of linear
 substitution of femfuns.
*)

let _raw_femfun_linsubs
    dim raw_femfun nr_L femfun_to_subs =
  let ht_result = Hashtbl.create (Array.length raw_femfun) in
    begin
      array_foreach_do raw_femfun
	(fun {ff_L_powers=pows_L; ff_dL_powers=pows_dL; ff_coefficient=coeff;} ->
	   let new_pows_L =
	     let a = Array.copy pows_L in
	     let () = a.(nr_L) <- 0 in
	       a
	   in
	   let pow_L = pows_L.(nr_L) in
	   let pow_substituent = femfun_power dim femfun_to_subs pow_L in
	   let (_,substituted_summand) =
	     femfun_mult
	       pow_substituent
	       (registered_femfun
		  [|{ff_L_powers = new_pows_L;
		     ff_dL_powers = pows_dL;
		     ff_coefficient = coeff;
		    }|])
	   in
	     Array.iter
	       (fun {ff_L_powers=x1; ff_dL_powers=x2; ff_coefficient=c;} ->
		  hashtbl_increase (+.) ht_result (x1,x2) c)
	       substituted_summand
	);
      _femfun_from_hash ht_result
    end
;;

let femfun_linsubs dim femfun nr_L femfun_to_subs =
  let (nr_ff,raw_ff) = femfun
  and (nr_subs,_) = femfun_to_subs in
    try
      Hashtbl.find ht_all_femfun_linsubs (nr_ff,nr_subs,nr_L)
    with
      | Not_found ->
	  let result = _raw_femfun_linsubs dim raw_ff nr_L femfun_to_subs in
	  let () = Hashtbl.add ht_all_femfun_linsubs (nr_ff,nr_subs,nr_L) result in
	    result
;;

let _raw_femfun_integrate raw_femfun nr_L =
  let ht_result = Hashtbl.create (Array.length raw_femfun) in
    begin
      array_foreach_do raw_femfun
	(fun {ff_L_powers=pows_L; ff_dL_powers=pows_dL; ff_coefficient=coeff;} ->
	   let new_pows_L = Array.copy pows_L in
	   let new_exponent = 1+pows_L.(nr_L) in
	   let () = new_pows_L.(nr_L) <- new_exponent in
	   let new_coeff = coeff /. (float_of_int new_exponent) in
	     hashtbl_increase (+.) ht_result (new_pows_L,pows_dL) new_coeff
	);
      _femfun_from_hash ht_result
    end
;;

let femfun_integrate femfun nr_L =
  let (nr_ff,raw_ff) = femfun in
    try
      Hashtbl.find ht_all_femfun_integrals (nr_ff,nr_L)
    with
      | Not_found ->
	  let result = _raw_femfun_integrate raw_ff nr_L in
	  let () = Hashtbl.add ht_all_femfun_integrals (nr_ff,nr_L) result in
	    result
;;

(* A simplex_decomposition is given as an array of ([|L-values|],partial_volume)

   Note that the numfun must be able to handle extended point coordinates.
 *)

let femfun_numerical_integrator simplex_decomposition femfun simplex =
  let dim=Array.length simplex.ms_points.(0).mp_coords in
  let scratch_pos = Array.make (1+dim) 0.0 in
  let mx_dL_dx = 
    match simplex.ms_inv_ext_point_coords with
    | None -> failwith "Essential simplex information missing!"
    | Some m -> m
  in
  let mx_dx_dL =
    match simplex.ms_ext_point_coords with
    | None -> failwith "Essential simplex information missing!"
    | Some m -> m
  in
  let sx_volume = simplex.ms_point_coords_det/.(float_factorial dim) in
  let points_and_coeffs = 
    Array.map
      (fun (coords_L,partial_volume) ->
	let ext_pos = mx_x_vec ~store_result:scratch_pos mx_dx_dL coords_L in
	(Array.sub ext_pos 0 dim,
	 (femfun_eval mx_dL_dx femfun coords_L)*.partial_volume))
      simplex_decomposition
  in
  fun numfun ->
    Array.fold_left
      (fun sf (pos,coeff) -> sf+.(numfun pos)*.coeff) 0.0 points_and_coeffs
;;



(* Integration over surface:

   Note: the femfun to be integrated most likely is a product of
   a LHS and a RHS femfun.

   (1) We have to go from natural coordinates to a more appropriate
       parametrization: kill a L different from the one specifying
       the surface, e.g. subtitute L2 = 1-L1-L3.

   (2) Set the L which specifies the surface to zero.

   (3) Subsequently integrate out the other Ls.
 *)

let _raw_femfun_integrate_over_surface dim femfun nr_L =
  let nr_vertices = 1+dim in
  let nr_v_zeroes = Array.make nr_vertices 0 in
  let nr_L_to_kill = if nr_L = 0 then 1 else 0 in
  let make_substituent those_Ls =
    (* L_j = 1-L_k-L_m-L_n-... *)
    let nr_summands = 1+Array.length those_Ls in
    let s =
      Array.init nr_summands
	(fun n ->
	   if n = 0 then
	     {ff_L_powers=nr_v_zeroes;
	      ff_dL_powers = [||];
	      ff_coefficient=1.0;
	     }
	   else
	     {ff_L_powers =
		 Array.init nr_vertices (fun k -> if k=those_Ls.(n-1) then 1 else 0);
	      ff_dL_powers = [||];
	      ff_coefficient = (-1.0);
	     })
    in registered_femfun s
  in
  let all_Ls = Array.init nr_vertices (fun x -> x) in
  let substituent_kill_L =
    make_substituent (array_filter (fun x -> x <> nr_L_to_kill) all_Ls)
  in
  let femfun_L_killed =
    femfun_linsubs dim
      femfun nr_L_to_kill substituent_kill_L
  in
  let femfun_face =
    femfun_linsubs dim
      femfun_L_killed nr_L femfun_zero
  in
  let integrating_Ls =
    array_filter (fun x -> x <> nr_L_to_kill && x <> nr_L) all_Ls
  in
  (* It is good to both have and return those, as their order conveys sign
     information!
     XXX But, do we really need that?!? If our femfun were constant, then we would get
     a positive value from our integration, so all the faces are automatically oriented
     "in the positive sense".
   *)
  let rec work_integrate femfun todo_Ls =
    let nr_todo = Array.length todo_Ls in
    if nr_todo = 0
    then femfun	(* Should be just a coefficient now! *)
    else
      let next_L = todo_Ls.(nr_todo-1) in
      let further_todo_Ls = Array.sub todo_Ls 0 (nr_todo-1) in
      let ff_integrated = femfun_integrate femfun next_L in
      let ff_integral_evaled =
	femfun_linsubs dim ff_integrated next_L
	  (make_substituent further_todo_Ls)
      in work_integrate ff_integral_evaled further_todo_Ls
  in
  (nr_L_to_kill, integrating_Ls, work_integrate femfun_face integrating_Ls)
;;

let femfun_integrate_over_surface dim femfun nr_L =
  let (nr_ff,raw_ff) = femfun in
    try
      Hashtbl.find ht_all_femfun_surface_integrals (dim,nr_ff,nr_L)
    with
      | Not_found ->
	  let result = _raw_femfun_integrate_over_surface dim femfun nr_L in
	  let () = Hashtbl.add ht_all_femfun_surface_integrals (dim,nr_ff,nr_L) result in
	    result
;;


(* femfun_face is the one which we will have to integrate.
   However, we still have to take care that the dLj
   integration introduces some extra inv-ext-coeff factors.

   XXX We will deal with this issue in an outer function, and this
   discussion therefore is bound to become obsolete (at least in
   that particular place)!

   How does this work? On dimensional grounds, our d^nL integral
   has to be converted into a space integral, hence we have to
   add factors from ext_coords (dx/dL).

   XXX NOTE: this is unfortunate, as it means that we have to extend
   the femfun data structure!

   The only construction that makes sense geometrically is to:

   (1) throw away that one ext_point corresponding to the omitted vector

   (2) Dualize the (N-1) vector formed by the others into a 1-form by means
   of epsilon_...

   (3) Use the metric(!) to convert this into a surface normal vector.

   Now, it seems to me as if what we do with this vector depends on the
   type of problem we are trying to solve. Here, this gives us an extra
   contribution to div M if we dot it into M on one side.

   Is it exluded that we never come across situations where we just
   have to take its length and look at a surface normal derivative?
   Consider a charged conducting sphere in potential theory for
   example.

   So, what to do once we have the surface normal vector? It's up to
   the DOFs - we need a lambda abstraction there.

   XXX No, actually, no extension will be needed, as the factors that enter
   just are the co-factors. Those can be obtained from
   <volume>*<inv-ext-point-coords>!
 *)


let _raw_femfun_diff_x raw_femfun nr_dx =
  let ht_result = Hashtbl.create (Array.length raw_femfun) in (* rough estimate *)
  let do_diff_summand {ff_coefficient=c;ff_dL_powers=pows_dL;ff_L_powers=pows_L} =
    Array.iteri
      (fun nr_L pow ->
	if pow > 0 then
	  let new_L_powers = Array.copy pows_L in
	  let () = new_L_powers.(nr_L) <- new_L_powers.(nr_L)-1 in
	  let extra_pows_dL = [|{dlp_nr_L=nr_L;dlp_nr_dx=nr_dx;dlp_pow=1}|] in
	  let () =
	    hashtbl_increase (+.) ht_result
	      (new_L_powers,fuse_dL_powers pows_dL extra_pows_dL)
	      (c*.(float_of_int pow))
	  in ()
	else ()
      ) pows_L
  in
  let () = Array.iter do_diff_summand raw_femfun in
  _femfun_from_hash ht_result
;;

let femfun_diff_x femfun nr_dx =
  let (nr_ff,raw_ff) = femfun in
    try
      Hashtbl.find ht_all_femfun_derivatives (nr_ff,nr_dx)
    with
      | Not_found ->
	  let result = _raw_femfun_diff_x raw_ff nr_dx in
	  let () = Hashtbl.add ht_all_femfun_derivatives (nr_ff,nr_dx) result in
	    result
;;

let femfun_integrate_over_simplex (_,femfun) simplex =
  let dL = match simplex.ms_inv_ext_point_coords with
  | None -> failwith "Simplex needs inv_ext_point_coords!"
        (* Any properly constructed mesh will provide this!
	   XXX But maybe, we nevertheless should
	   raise another exception here.
	 *)
  | Some x -> x
  in
  let dim = Array.length dL-1 in
  let integral =
    let nr_summands = Array.length femfun in
    let rec walk_summands sf n =
      if n = nr_summands then sf
      else
	let summand = femfun.(n) in
	let coeff = summand.ff_coefficient in
	let powers_dL = summand.ff_dL_powers in
	let powers_L = summand.ff_L_powers in
	let factor_L = element_shape_function_power_integral_factor powers_L in
	let contrib =
	  let nr_factors = Array.length powers_dL in
	  let rec walk_factors sf nr_factor =
	    if nr_factor = nr_factors then sf
	    else
	      let factor = powers_dL.(nr_factor) in
	      let dL_factor  = dL.(factor.dlp_nr_L).(factor.dlp_nr_dx) in 
	      let dL_factor_pow = int_power factor.dlp_pow dL_factor in
		walk_factors (sf*.dL_factor_pow) (1+nr_factor)
	  in walk_factors (coeff*.factor_L) 0
	in
	  walk_summands (sf+.contrib) (1+n)
    in walk_summands 0.0 0
  in integral *. abs_float(simplex.ms_point_coords_det)
    (* multiply with volume - note that the total cell volume,
       and not just the simplex volume goes into that formula.
       Also note that we explicitly have to use abs_float to get the positive volume.
       This may be traced back to a hidden implicit convention to use positively
       oriented L-coordinates for integration.
     *)
;;



(* XXX for now, this is just a cut&paste hack, which duplicates
   femfun_integrate_over_simplex. Redo these two functions properly!
*)
let femfun_integrate_over_simplex_face ?(bare_integral=false) femfun simplex nr_face =
  let dL = match simplex.ms_inv_ext_point_coords with
  | None -> failwith "Simplex needs inv_ext_point_coords!"
        (* Any properly constructed mesh will provide this!
	   XXX But maybe, we nevertheless should
	   raise another exception here.
	 *)
  | Some x -> x
  in
  let dim = Array.length dL-1 in
  let (_,_,(_,integrated_over_L)) = femfun_integrate_over_surface dim femfun nr_face in
  (* This will now only contain some dL factors.
     XXX change call signature!
   *)
  let integral =
    Array.fold_left
      (fun sf summand ->
	let coeff = summand.ff_coefficient in
	let powers_dL = summand.ff_dL_powers in
	let contrib =
	  Array.fold_left
	    (fun sf factor ->
	      let dL_factor  = dL.(factor.dlp_nr_L).(factor.dlp_nr_dx) in
	      let dL_factor_pow = int_power factor.dlp_pow dL_factor in
	      sf*.dL_factor_pow)
	    coeff powers_dL
	in
	sf+.contrib) 0.0 integrated_over_L
  in if bare_integral
    then integral
    else (integral *. abs_float(simplex.ms_point_coords_det))
      (* multiply with volume - note that the total cell volume,
	 and not just the simplex volume goes into that formula.
	 Also note that we explicitly have to use abs_float to get the positive volume.
	 This may be traced back to a hidden implicit convention to use positively
	 oriented L-coordinates for integration.
	 
	 Note furthermore that the necessity of this volume factor already can be seen from
	 dimensions: dL entries have dimensions 1/Length (x has dimension Length,
	 L has dimension 1), and we need Length**(dim-1).
	 
	 (Originally, I included an extra factor -1 here - this turned out to be wrong!)
      *)
;;

(* NOTE: below we use the guarantee that the dofs in mwe_dofs_by_site
   are sorted lexicographically wrt. their subfields and indices.
*)

let mwe_subfield_info mwe subfield_name =
  let (_,subfield_indices) = 
    try
      array_find (fun (n,_) -> n=subfield_name) mwe.mwe_subfields
    with
      | Not_found ->
      failwith (Printf.sprintf "mwe_subfield_info: unknown subfield '%s' in mwe '%s'"
		  subfield_name mwe.mwe_name)
  in
  let sites_and_relevant_dofs = 
    array_mapfilter
      (fun (site,dofs) ->
	 if Array.length dofs = 0
	 then None
	 else Some (site,dofs.(0).dof_pos,dofs))
      (map_hashtbl_to_array 
	 ~sorter:(fun (k1,_) (k2,_) -> compare k1 k2)
	 (fun site_ix dofs ->
	    (site_ix,
	     array_filter
	       (fun dof ->
		  let (dof_stem,_) = the_dof_name mwe dof in
		    dof_stem = subfield_name)
	       dofs))
	 mwe.mwe_dofs_by_site
      )
  in
    ((subfield_name,subfield_indices),sites_and_relevant_dofs)
;;


let mwe_ensure_has_volumes mwe =
  if !(mwe.mwe_dof_funs_volumes) <> None
  then () (* We have all the information we need *)
  else
    let pname s = Printf.sprintf "%s-MWE-%s" s mwe.mwe_name in
    let nr_dofs = Array.length mwe.mwe_dofs in
    let v_vols = Array.make nr_dofs 0.0 in
    let () =
      for nr_dof=0 to nr_dofs-1 do
	dof_sx_nel_ix_iter mwe.mwe_mesh.mm_simplices mwe.mwe_dofs.(nr_dof)
	  (fun sx nel ix ->
	     let elem = mwe.mwe_elements.(nel) in
	     let dof_femfun = elem.el_dof_funs.(ix) in
	     let vol_contrib = femfun_integrate_over_simplex dof_femfun sx in
	     let images = mwe.mwe_dof_periodic_images.(nr_dof) in
	       if Array.length images <=1 then
		 v_vols.(nr_dof) <- v_vols.(nr_dof)+.vol_contrib
	       else
		 Array.iter 
		   (fun ix_img ->
		      (* let () = Printf.printf "PERIODIC-VOL %s: dof %4d += %f\n%!" mwe.mwe_name ix_img vol_contrib in *)
		      v_vols.(ix_img) <- v_vols.(ix_img)+.vol_contrib) images)
      done
    in
    let () = 
      for nr_dof=0 to nr_dofs-1 do
	mwe.mwe_dofs.(nr_dof).dof_volume <- v_vols.(nr_dof);
      done
    in
      mwe.mwe_dof_funs_volumes := Some v_vols
;;

(* Conceptually:

   Suppose we are given a description of an interim point
   of the type [|3;0;0;2|], which means: fifth-order interim point,
   which is the average of vertex 1 and vertex 4, with weights 3 and 2.

   Now, given a simplex' vertex ID vector, e.g. [|17;25;49;203|], map
   this to an interim point ID of the form [|17;17;17;203;203|].

   For efficiency, we provide the vector from the outside, so that it
   does not have to be consed.

*)
let fold_out_abssite interim_point_desc simplex_vertex_ids =
  let nr_points = Array.length simplex_vertex_ids in
  let order = Array.fold_left (+) 0 interim_point_desc in
  let target = Array.make order 0 in
  let rec fill pos_target pos_ipd =
    if pos_target = order
    then target
    else
      let nr_copies_this_one = interim_point_desc.(pos_ipd) in
	begin
	  for i=0 to nr_copies_this_one-1 do
	    target.(pos_target+i) <- simplex_vertex_ids.(pos_ipd);
	  done;
	  fill (pos_target+nr_copies_this_one) (pos_ipd+1)
	end
  in
  let result = fill 0 0 in
  result
;;

(* Example:
# let x = Array.make 5 0 in
   let () = do_fold_out_abssite_desc x [|2;0;1;2|] [|1001;1002;1003;1004|]
  in x;;
- : int array = [|1001; 1001; 1003; 1004; 1004|]
*)

let abssite_coords abssite sx =
  let dim = Array.length sx.ms_points.(0).mp_coords in
  let pt = Array.make dim 0.0 in
  let weight_factor = 1.0/.(float_of_int (Array.fold_left (+) 0 abssite)) in
  begin
    for i=0 to dim do
      let weight_i = abssite.(i) in
      let coords_i = sx.ms_points.(i).mp_coords in
	(if weight_i <> 0 then
	     let f_weight_i = float_of_int weight_i in
	       for k=0 to dim-1 do
		 pt.(k) <- pt.(k) +. f_weight_i *. coords_i.(k)
	       done;
	 else ());
    done;
    for k=0 to dim-1 do
      pt.(k) <- pt.(k)*.weight_factor
    done;
    pt
  end
;;

(* Ad differential operators:

   The idea behind the Galerkin method is to turn a continuum system
   into a system of finitely many degrees of freedom by projecting
   equations of motion to a set of basis functions which are
   associated to a geometrical structure that conveniently encodes
   proximity properties (i.e. a mesh). The fundamental quantity
   we have to compute is

   Integral(Simplex_j) (D1 Psi_j,k) (D2 Phi_j,m) d_Simplex

   where Psi_j, Phi_j are functions associated to simplex #j, and D1, D2
   are 0th order or 1st order differential operators.

   Note, however, that we may have to deal with surface
   contributions. This happens if there is a "jump" in the field, in
   particular when the elements on both sides of a face carry
   different DOFs.

   Observe the following points:

   - If we use restrictions of polynomials to simplices as shape
     functions Phi,Psi, and furthermore only use field values
     (and no derivatives) at support points belonging to the simplex
     to determine the shape of mesh-interpolated functions, then we can
     always model a kink at an element boundary.
     This would become a step in the first derivative, and a delta
     distribution located at the boundary in the second derivative.

     Now, as it is quite ugly to properly work with delta and higher
     derivatives located at boundaries, we can only use first-order
     differential operators here.

   - By means of partial integration, A D B may usually be re-written
     as (D1 A) (D2 B). So, we may conveniently do second-order.

   - For higher order, there also is a problem with boundary
     conditions. It is perhaps appropriate then to re-write the system
     as one of lower order by introducing extra Phi', Phi'' fields and
     bring the system into the form of a first-order one.

   What lessons do we learn from this?

   - We perhaps should only support acting with linear differential
     operators on element functions.

   - So, what *is* a differential operator?
     XXX this discussion has to be re-done!
     It is given as a mapping which we eventually want to apply to a simplex,
     and later on even to a complete mesh. We have to specify how
     operator-rhs DOFs associated to a simplex are mapped to operator-lhs
     DOFs associated to a simplex. In principle, we can either go for a
     "readable" data structure that uses symbolic names to identify DOFs,
     like "F01" or "E2", or a not-so-readable more low level one that just
     uses indices. The latter seems more appropriate, as (1) we only want
     to do the most basic case for now, and (2) if things get somewhat more
     complicated, we may be better off providing a much more convenient
     parsed language for it anyway.

     Hence, our representation is an array2, with

     operator.(nr_dof_out).(nr_dof_in) =
      [|coeff_d/dx0,coeff_d/dx1,...coeff_d/dx(dim-1),coeff_no_derivative|]

     Where nr_dof_out and nr_dof_in refer to el_dof_funs indices.

   - If we know the differential operator, what next? Answer: we have to
     apply it to a mesh in order to get a stiffness matrix.

     Note that we may want to have element types as well as diff-ops
     depend on the simplex. (Consider, for example, a "mass" parameter
     in Laplace + m^2 depending on spatial position, i.e. a "hard" and
     a "soft" material with an interface in between.)

   So, the first step is to associate to a mesh a set of degrees of
   freedom.

   However, how do we form the stiffness matrix?

   The key observation here is that a matrix entry is always of the
   form scalar-product(left-fun,right-fun), no matter what. Now,
   left-fun and right-fun are obtained by applying differential
   operators to the original dof functions.

   The question is: how to associate them? Alas, only the differential
   operator knows, or rather: let us define it as the entity that does
   know just about that.
 *)


(* Associating degrees of freedom to a mesh.

   How does this work?

   Given a mesh and a function mapping simplices to elements (usually,
   a constant function), collect all the DOFs in an array.

   Furthermore, we have to know for every simplex what the
   corresponding element is and where the associated dofs live.

*)

let copy_field (FEM_field (mwe,r,v)) =
  FEM_field (mwe,r,Mpi_petsc.vector_pack (Mpi_petsc.vector_extract v))
;;

let copy_cofield (FEM_cofield (mwe,r,v)) =
  FEM_cofield (mwe,r,Mpi_petsc.vector_pack (Mpi_petsc.vector_extract v))
;;

let copy_field_into (FEM_field (mwe_s,r_s,v_s)) ((FEM_field (mwe_d,r_d,v_d)) as result) =
  let () =
    (if mwe_s != mwe_d || r_s <> r_d then failwith "copy_field_into: incompatible fields!" else ())
  in
  let _ = Mpi_petsc.vector_copy v_s v_d
  in result
;;

let copy_cofield_into (FEM_cofield (mwe_s,r_s,v_s)) ((FEM_cofield (mwe_d,r_d,v_d)) as result) =
  let () =
    (if mwe_s != mwe_d || r_s <> r_d then failwith "copy_cofield_into: incompatible cofields!" else ())
  in
  let _ = Mpi_petsc.vector_copy v_s v_d
  in result
;;


(* What is the constant_value argument good for?

   Actually, there are only two values that make sense:

   - zero, for proper initialization

   - one, because if we map this to a co-field, we get the volumes
     associated to every DOF.
*)

let make_field ?(name=gensym "Field_") ?(restriction=None) ?constant_value mwe =
  let nr_dof = 
    match restriction with
      | None -> Array.length mwe.mwe_dofs
      | Some restr ->
	  let (_,long_to_short,_) = mwe_shortvec_info mwe restriction in
	    Array.length long_to_short
  in
  let vec =
    match constant_value with
      | None ->
	  let v = Mpi_petsc.vector_create nr_dof name in
	  let () = Mpi_petsc.vector_assemble v in
	    v
      | Some c ->
	  Mpi_petsc.vector_pack (Array.make nr_dof c)
  in
    FEM_field (mwe,restriction,vec)
;;

let make_cofield ?(name=gensym "Cofield_") ?(restriction=None) ?constant_value mwe =
  let nr_dof = 
    match restriction with
      | None -> Array.length mwe.mwe_dofs
      | Some restr ->
	  let (_,long_to_short,_) = mwe_shortvec_info mwe restriction in
	    Array.length long_to_short
  in
  let vec =
    match constant_value with
      | None ->
	  let v = Mpi_petsc.vector_create nr_dof name in
	  let () = Mpi_petsc.vector_assemble v in
	    v
      | Some c ->
	  Mpi_petsc.vector_pack (Array.make nr_dof c)
  in
    FEM_cofield (mwe,restriction,vec)
;;


(* XXX ZZZ This should use petsc's axpby! *)
let field_axpby ((FEM_field (mwe_target,rt,v_target)) as target)
    coeff1 (FEM_field (mwe1,r1,v_src1))
    coeff2 (FEM_field (mwe2,r2,v_src2))
    =
  if mwe1 != mwe2 || mwe_target != mwe1 || r1 <> r2 || rt <> r1
  then failwith "field_axpby: incompatible fields provided!"
  else
    let nr_dofs = Array.length mwe1.mwe_dofs in
    let () =
      Mpi_petsc.with_petsc_vector_as_bigarray v_target
	(fun ba_target ->
	   Mpi_petsc.with_petsc_vector_as_bigarray v_src1
	     (fun ba_src1 ->
		Mpi_petsc.with_petsc_vector_as_bigarray v_src2
		  (fun ba_src2 ->
		     for i=0 to nr_dofs-1 do
		       ba_target.{i} <- coeff1*.ba_src1.{i}+. coeff2*.ba_src2.{i}
		     done)))
    in target
;;


let cofield_to_field ?target ?(box_method=false) ((FEM_cofield (mwe,_,v)) as cofield) =
  let () = ensure_cofield_is_unrestricted cofield in
  let gs =
    if box_method 
    then
      let () = mwe_ensure_has_volumes mwe in
      let v_vols = match !(mwe.mwe_dof_funs_volumes) with
	| None -> impossible()
	| Some v -> v
      in
      fun ?output ~input () ->
	let v_out = 
	  match output with
	    | None ->
		let v = Mpi_petsc.vector_create (Array.length mwe.mwe_dofs) (gensym "field-") in
		let () = Mpi_petsc.vector_assemble v in
		  v
	    | Some v -> v
	in
	let () =
	  Mpi_petsc.with_petsc_vector_as_bigarray v_out
	    (fun ba_out ->
	       Mpi_petsc.with_petsc_vector_as_bigarray input
		 (fun ba_input ->
		    for i=0 to Array.length mwe.mwe_dofs-1 do
		      ba_out.{i} <- ba_input.{i}/.v_vols.(i)
		    done))
	in
	  v_out
    else
      failwith "NOTE: cofield_to_field now only supports the box method! Code using the inner product matrix was removed!"
  in
    match target with
      | None -> FEM_field (mwe,None, (gs ~input:v ()))
      | Some (FEM_field (mwe_target,_,v_target) as field) ->
	  let () = ensure_field_is_unrestricted field in
	    if mwe_target != mwe
	    then failwith "cofield_to_field: invalid target field"
	    else
	      FEM_field (mwe, None, (gs ~input:v ~output:v_target ()))
;;

let field_to_cofield ?target ?(box_method=false) ((FEM_field (mwe,_,v)) as field) =
  let () = ensure_field_is_unrestricted field in
  let () = mwe_ensure_has_volumes mwe in
  let v_vols = match !(mwe.mwe_dof_funs_volumes) with
    | None -> impossible()
    | Some v -> v
  in
  let () = 
    (if box_method=false
     then      
       failwith "NOTE: field_to_cofield now only supports the box method! Code using the inner product matrix was removed!"
     else ())
  in
  let vc = 
    match target with
      | None ->
	  let v = Mpi_petsc.vector_create (Array.length mwe.mwe_dofs) (gensym "field-") (* XXX gensym! *)
	  in 
	  let () = Mpi_petsc.vector_assemble v in
	    v
      | Some ((FEM_cofield (mwe_target,_,v_target)) as cofield) ->
	  let () = ensure_cofield_is_unrestricted cofield in
	    if mwe_target != mwe
	    then failwith "field_to_cofield: invalid target cofield"
	    else
	      v_target
  in
  let () =
    Mpi_petsc.with_petsc_vector_as_bigarray vc
      (fun ba_vc ->
	 Mpi_petsc.with_petsc_vector_as_bigarray v
	   (fun ba_v ->
	      for i=0 to Array.length mwe.mwe_dofs-1 do
		ba_vc.{i} <- ba_v.{i} *. v_vols.(i)
	      done))
  in
    FEM_cofield (mwe,None,vc)
;;


let field_mwe (FEM_field (mwe,_,_)) = mwe;;
let cofield_mwe (FEM_cofield (mwe,_,_)) = mwe;;

let field_do_scale (FEM_field (mwe,_,v)) x =
  Mpi_petsc.vector_scale v x
;;

let cofield_do_scale (FEM_cofield (mwe,_,v)) x =
  Mpi_petsc.vector_scale v x
;;

(* Source "pushes" its entries into dst *)
let field_push (FEM_field (mwe_src,restr_src,v_src)) (FEM_field (mwe_dst,restr_dst,v_dst)) =
  let () = (if mwes_are_compatible mwe_src mwe_dst then () else failwith "field_push: incompatible MWEs!") in
  let (src_lts, src_stl, _) = mwe_shortvec_info mwe_src restr_src in
  let (dst_lts, dst_stl, _) = mwe_shortvec_info mwe_dst restr_dst in
    Mpi_petsc.with_petsc_vector_as_bigarray v_src
      (fun ba_src ->
	 Mpi_petsc.with_petsc_vector_as_bigarray v_dst
	   (fun ba_dst ->
	      for i=0 to Array.length src_stl-1 do
		ba_dst.{dst_lts.(src_stl.(i))} <- ba_src.{i};
	      done))
;;

(* dst "pulls" its entries from src *)
let field_pull (FEM_field (mwe_src,restr_src,v_src)) (FEM_field (mwe_dst,restr_dst,v_dst)) =
  let () = (if mwes_are_compatible mwe_src mwe_dst then () else failwith "field_pull: incompatible MWEs!") in
  let (src_lts, src_stl, _) = mwe_shortvec_info mwe_src restr_src in
  let (dst_lts, dst_stl, _) = mwe_shortvec_info mwe_dst restr_dst in
    Mpi_petsc.with_petsc_vector_as_bigarray v_src
      (fun ba_src ->
	 Mpi_petsc.with_petsc_vector_as_bigarray v_dst
	   (fun ba_dst ->
	      for i=0 to Array.length dst_stl-1 do
		ba_dst.{i} <- ba_src.{src_lts.(dst_stl.(i))};
	      done))
;;

let cofield_push (FEM_cofield (mwe_src,restr_src,v_src)) (FEM_cofield (mwe_dst,restr_dst,v_dst)) =
  let () = (if mwes_are_compatible mwe_src mwe_dst then () else failwith "cofield_push: incompatible MWEs!") in
  let (src_lts, src_stl, _) = mwe_shortvec_info mwe_src restr_src in
  let (dst_lts, dst_stl, _) = mwe_shortvec_info mwe_dst restr_dst in
    Mpi_petsc.with_petsc_vector_as_bigarray v_src
      (fun ba_src ->
	 Mpi_petsc.with_petsc_vector_as_bigarray v_dst
	   (fun ba_dst ->
	      for i=0 to Array.length src_stl-1 do
		ba_dst.{dst_lts.(src_stl.(i))} <- ba_src.{i};
	      done))
;;

let cofield_pull (FEM_cofield (mwe_src,restr_src,v_src)) (FEM_cofield (mwe_dst,restr_dst,v_dst)) =
  let () = (if mwes_are_compatible mwe_src mwe_dst then () else failwith "cofield_pull: incompatible MWEs!") in
  let (src_lts, src_stl, _) = mwe_shortvec_info mwe_src restr_src in
  let (dst_lts, dst_stl, _) = mwe_shortvec_info mwe_dst restr_dst in
    Mpi_petsc.with_petsc_vector_as_bigarray v_src
      (fun ba_src ->
	 Mpi_petsc.with_petsc_vector_as_bigarray v_dst
	   (fun ba_dst ->
	      for i=0 to Array.length dst_stl-1 do
		ba_dst.{i} <- ba_src.{src_lts.(dst_stl.(i))};
	      done))
;;


(* XXX ZZZ make_field, set_field, sample_field, scale_field, combine_fields, ... *)

(* Internal function *)
let write_mwe_to_fh ?(handle=stdout) mwe = failwith "XXX WRITE ME - write_mwe_to_fh!";;

let write_mwe filename mwe =
  let h = open_out filename in
  let () = write_mwe_to_fh ~handle:h mwe in
    close_out h
;;

let mwe_sibling new_name element_renaming_prefix dof_renamings mwe =
  if not mwe.mwe_is_primary
  then failwith "Can only derive from primary MWEs!"
    (* XXX maybe we should use an exception which is more python-interface-friendly? *)
  else
    let rename old_name =
      try
	let (_,new_name) = array_find (fun (k,_) -> k = old_name) dof_renamings in
	  new_name
      with | Not_found -> old_name
    in
    let rename_dof (n,ix) = (rename n,ix) in
    let mwe_mb = 
      MWEMB_mwe_sibling (new_name,element_renaming_prefix,dof_renamings,mwe.mwe_name)
    in
      {
	mwe_mesh=mwe.mwe_mesh;
	mwe_made_by=mwe_mb;
	mwe_name=new_name;
	mwe_elements=
	  Array.map (_element_rename_dofs element_renaming_prefix dof_renamings) mwe.mwe_elements;
	mwe_is_primary=false;
	mwe_subfields=Array.map rename_dof mwe.mwe_subfields;
	mwe_properties_by_region=mwe.mwe_properties_by_region;
	mwe_sites = mwe.mwe_sites;
	mwe_pmids = mwe.mwe_pmids;
	mwe_sites_simplex_info_pmid = mwe.mwe_sites_simplex_info_pmid;
	mwe_dofs = mwe.mwe_dofs; (* Note that this large chunk of data just can be shared! *)
	mwe_dof_periodic_images = mwe.mwe_dof_periodic_images;
        mwe_dofs_by_site = mwe.mwe_dofs_by_site;
	mwe_nel_by_simplex_index = mwe.mwe_nel_by_simplex_index;
	mwe_dof_nrs_by_simplex_index = mwe.mwe_dof_nrs_by_simplex_index;
	mwe_dof_neighbour_pairs = mwe.mwe_dof_neighbour_pairs;
	mwe_dof_funs_g = mwe.mwe_dof_funs_g;
	mwe_dof_funs_g_solver = mwe.mwe_dof_funs_g_solver;
	mwe_dof_funs_g_entries = mwe.mwe_dof_funs_g_entries;
        mwe_dof_funs_volumes = mwe.mwe_dof_funs_volumes;
	mwe_distribution = mwe.mwe_distribution;
	mwe_dofs_and_distribution_by_dvss=
	  mwe.mwe_dofs_and_distribution_by_dvss;
      }
;;

let mwe_memstats mwe =
  let mf = Snippets.memory_footprint in
  let stats = [|
    ("mwe_mesh", mf (mwe.mwe_mesh));
    ("mwe_made_by", mf (mwe.mwe_made_by));
    ("mwe_name", mf (mwe.mwe_name));
    ("mwe_elements", mf (mwe.mwe_elements));
    ("mwe_is_primary", mf (mwe.mwe_is_primary));
    ("mwe_subfields", mf (mwe.mwe_subfields));
    ("mwe_properties_by_region", mf (mwe.mwe_properties_by_region));
    ("mwe_sites", mf (mwe.mwe_sites));
    ("mwe_pmids", mf (mwe.mwe_pmids));
    ("mwe_sites_simplex_info_pmid", mf (mwe.mwe_sites_simplex_info_pmid));
    ("mwe_dofs", mf (mwe.mwe_dofs));
    ("mwe_dof_periodic_images", mf (mwe.mwe_dof_periodic_images));
    ("mwe_dofs_by_site", mf (mwe.mwe_dofs_by_site));
    ("mwe_nel_by_simplex_index", mf (mwe.mwe_nel_by_simplex_index));
    ("mwe_dof_nrs_by_simplex_index", mf (mwe.mwe_dof_nrs_by_simplex_index));
    ("mwe_dof_neighbour_pairs", mf (mwe.mwe_dof_neighbour_pairs));
    ("mwe_dof_funs_g", mf (mwe.mwe_dof_funs_g));
    ("mwe_dof_funs_g_solver", mf (mwe.mwe_dof_funs_g_solver));
    ("mwe_dof_funs_g_entries", mf (mwe.mwe_dof_funs_g_entries));
    ("mwe_dof_funs_volumes", mf (mwe.mwe_dof_funs_volumes));
    ("mwe_distribution", mf (mwe.mwe_distribution));
    ("mwe_dofs_and_distribution_by_dvss", mf (mwe.mwe_dofs_and_distribution_by_dvss));
    ("mwe (total)", mf mwe);|]
  in
    stats
;;

let mwe_memstats_str stats =
  let in_b nr_bytes = Printf.sprintf "%d B" (int_of_float nr_bytes) in
  let in_kb nr_bytes = Printf.sprintf "%d KB" (int_of_float (nr_bytes/.1024.0))
  in
  let lines =
    Array.map
      (fun (member_name, (data, headers, depth)) ->
        Printf.sprintf "%35s:\t data=%12s\t headers=%12s\t depth=%8d\n"
          member_name (in_b data) (in_b headers) (int_of_float depth))
      stats
  in
    String.concat "" (Array.to_list lines)
;;


(* A field cloning function - XXX ZZZ obsolete! *)
let make_compatible_field (FEM_field (mwe,r,v)) =
  FEM_field (mwe,r, Mpi_petsc.vector_duplicate v)
;;

(* This is used e.g. to compute a total energy if one field is defined as
   dE/d<other field>
*)
let field_cofield_inner_product
    (FEM_field (mwe_f,r1,data_f))
    (FEM_cofield (mwe_cf,r2,data_cf)) =
  if not (mwes_are_compatible mwe_f mwe_cf) || r2 <> r2
  then failwith "Problem: incompatible field and cofield given for field_cofield_inner_product"
  else
    let sum = ref 0.0 in
    let nr_dofs = Array.length mwe_f.mwe_dofs in
    let () =
      Mpi_petsc.with_petsc_vector_as_bigarray data_f
	(fun ba_f ->
	   Mpi_petsc.with_petsc_vector_as_bigarray data_cf
	     (fun ba_cf ->
		let rec walk n s =
		  if n=nr_dofs then sum:=s
		  else 
		    walk (1+n) (s+.ba_f.{n}*.ba_cf.{n})
		in walk 0 0.0))
    in !sum
;;


(* Should belong to the mesher. We use this for testing purposes... *)
let square_mesh nr_rows nr_cols x_spacing y_spacing =
  let vertices = ref [] in
  let triangles = ref [] in
    begin
      for r=0 to nr_rows-1 do
	for c=0 to nr_cols-1 do
	  vertices:= [|(x_spacing*.(float_of_int c));(y_spacing*.(float_of_int r))|] :: !vertices;
	done;
      done;
      for r=0 to nr_rows-1-1 do
	for c=0 to nr_cols-1-1 do
	  let base_ix = r*nr_cols+c in
	  let tri_up_right = (Mesh.Body_Nr 1, [|base_ix;base_ix+1;base_ix+1+nr_cols|])
	  and tri_down_left = (Mesh.Body_Nr 1,[|base_ix;base_ix+nr_cols;base_ix+1+nr_cols|])
	  in
	    triangles:= tri_up_right :: tri_down_left :: !triangles;
	done;
      done;
      Mesh.mesh_from_known_delaunay (Array.of_list !vertices) (Array.of_list !triangles)
    end
;;



(* (also to go into mesh.ml):

   For a given site, produce a list of all the neighbour indices of
   faces this site is on.

   Evidently, a site is on a face if it does not contain the opposing
   point index.
 *)

let site_face_nrs simplex site =
  let nr_vertices = Array.length simplex.ms_points in
  let len_site = Array.length site in
  let rec walk pos have =
    if pos = (-1) then have
	(* Downward-counting so that we produce the entries
	   in the proper order already.
	   CPU prefetch questions are not an issue here:
	   we fetched the cache line above!
	*)
    else
      let pt_id = simplex.ms_points.(pos).mp_id in
      let rec find_point n =
	if n=len_site then false
	else if site.(n) = pt_id then true
	else find_point (1+n)
      in
      walk (pos-1) (if find_point 0 then have else pos::have)
  in walk (nr_vertices-1) []
;;


(* Find out which dofs disappear when going from one element to another
   XXX MAYBE OBSOLETE!
 *)
let mwe_dof_jumps mwe =
  let do_foreach_count x f = Array.iteri f x in
  let do_foreach x f = Array.iter f x in
  let ht_jumps_by_elem_nr_combinations = Hashtbl.create 5 in
  let () =
    do_foreach_count mwe.mwe_elements
      (fun n_elem1 elem1 ->
	 do_foreach_count mwe.mwe_elements
	   (fun n_elem2 elem2 ->
	      do_foreach elem1.el_subfields
		(fun ((stem,indices) as maxname) ->
		   if -1 <> array_position_if (fun x -> x=maxname) elem2.el_subfields 0
		   then () (* DOF present on both sides *)
		   else
		     hashtbl_push
		       ht_jumps_by_elem_nr_combinations
		       (n_elem1,n_elem2)
		       stem
		)))
  in ht_jumps_by_elem_nr_combinations
;;

(* Guarantees provided by make_mwe:

   (1) mwe_elements contains elements in the order of appearance, as we walk through
       all simplices.

   (2) mwe_subfields entries are listed in lexicographical order.

   (3) The mwe_sites_simplex_info array lists entries in
       site-lexicographical order, entries being

      (site,[(simplex_index,element_nr_abssite);...])
      ...where the simplex_index list is ordered lexicographically ascending.

   (4) dofs are ordered first by site, then by order of appearance through all 
   the abstract elements connected to the (lexically ordered) simplices, 
   which in particular implies that all the DOFs instantiating one subfield
   at a given site appear as a bunch, in index-lexicographical order.

   (This may not be the case when we deal with non-uniform subfield
   distribution across a simplex, but we will exclude such rather
   esoteric ideas for now. They may become important much farther down
   in the future.)

*)

let debug_make_mwe 
    name 
    ?(retain_simplex_info_pmid=false)
    ?(fun_outer_region=fun _ _ -> -1)
    ?(properties_by_region=[||])
    fun_simplex_to_element
    mesh
    =
  let ht_prop_by_region = Hashtbl.create 17 in 
  let () = Array.iter
    (fun (nr,props) ->
       Hashtbl.replace ht_prop_by_region (Body_Nr nr) props)
    properties_by_region
  in
  let dim = mesh.mm_dim in
  let simplices = mesh.mm_simplices in
  let points = mesh.mm_points in
  let nr_simplices = Array.length simplices in
  let nr_points = Array.length points in
    (* We need some support for mesh periodicity in here: *)
  let mesh_pp = mesh.mm_periodic_points in
  let canonicalize_point =
    let a = Array.make nr_points (-1) in
    let () = array_foreach_do mesh_pp 
      (fun periodic_points ->
	 let npp = Array.length periodic_points in
	   for i=1 to npp-1 do
	     a.(periodic_points.(i)) <- periodic_points.(0)
	   done)
    in
      fun ix -> let ix_canon = a.(ix) in if ix_canon = (-1) then ix else ix_canon
  in
  let ht_outer_face_opposing_points_by_sx_id =
    let ht = Hashtbl.create 100 in
    let () =
      Array.iter
	(fun ((_,Body_Nr nr_neighbour),entries) ->
	   if nr_neighbour >= 0 then ()
	   else
	     Array.iter
	       (fun (sx_id,nr_face) ->
		  let sx=simplices.(sx_id) in
		  let point_id = sx.ms_points.(nr_face).mp_id in
		    hashtbl_push ht sx_id point_id)
	       entries)
	mesh.mm_boundaries
    in ht
  in
  let (v_elements,v_nel_by_sx_index, subfields) =
    let all_elements = Array.map fun_simplex_to_element simplices in
    let ht_el = Hashtbl.create 10 in
    let nr_new_el = ref 0 in
    let () = Array.iter
      (fun el ->
	 try
	   let _ = Hashtbl.find ht_el el.el_name in ()
	 with
	   | Not_found -> (* We just encountered a new element *)
	       begin
		 Hashtbl.add ht_el el.el_name (!nr_new_el,el);
		 nr_new_el:= 1+ !nr_new_el;
	       end)
      all_elements
    in
    let v_elem =
      Array.map (fun (_,el) -> el)
	(map_hashtbl_to_array
	   ~sorter:(fun (nr1,el1) (nr2,el2) -> compare nr1 nr2)
	   (fun k v -> v) ht_el)
    in
    let v_nel =
      Array.map
	(fun el -> let (nr,_) = Hashtbl.find ht_el el.el_name in nr)
	all_elements
    in
    let subfields =
      (* was: array_join (Array.map (fun el -> el.el_subfields) v_elem)
	 Note: this is wrong, as different elements may carry same dofs!
      *)
      let a = array_uniqify (Array.map (fun el -> el.el_subfields) v_elem) in
      let () = Array.sort compare a in
	a
    in
      (v_elem,v_nel, subfields)
  in
  (* Next, we have to make all the DOFs.
     XXX Actually, there is more bookeeping information the MWE should
     know about, such as all the relevant sites in the MWE.
   *)
  let compute_sites_simplex_info_pmid () =
    (* Compute a vector of (site,[data on simplices that have that site])
       where for a single site, "data on simplices that have that site" is
       (simplex_index,element_nr_abssite)
    *)
    let ht_sites = Hashtbl.create 97 in
    let ht_site_to_pmid = Hashtbl.create 97 in
      begin
	for ix_sx=0 to nr_simplices-1 do
	  let sx = simplices.(ix_sx)
	  and el = v_elements.(v_nel_by_sx_index.(ix_sx))
	  in
	  let abssites = el.el_abssites in
	  let sx_vertex_ids = Array.map (fun p -> p.mp_id) sx.ms_points in
	  Array.iteri
	    (fun nr_abssite x ->
	       let site = fold_out_abssite x sx_vertex_ids in
	       let canon_site = Array.map canonicalize_point site in
	       let pmid =
		 try Hashtbl.find ht_site_to_pmid canon_site 
		 with
		   | Not_found ->
		       let n = Hashtbl.length ht_site_to_pmid in
		       let () = Hashtbl.add ht_site_to_pmid canon_site n in
			 n
	       in
		 hashtbl_push ht_sites site (ix_sx,nr_abssite))
	    abssites
	done;
	map_hashtbl_to_array
	  ~sorter:(fun (site1,_,_,_) (site2,_,_,_) -> compare site1 site2)
	  (fun site sx_info -> 
	     let canon_site = Array.map canonicalize_point site in
	     let a_sx_info = Array.of_list (List.rev sx_info) in
	     let sx_indices = Array.map (fun (x,_) -> x) a_sx_info in
	     let sx_abssite = 
	       let s = String.make (Array.length sx_indices) (Char.chr 0) in
		 begin
		   for i=0 to Array.length sx_indices-1 do
		     s.[i] <- Char.chr (let (_,y) = a_sx_info.(i) in y)
		   done;
		   s
		 end
	     in
	       (site,sx_indices, sx_abssite, Hashtbl.find ht_site_to_pmid canon_site))
	  ht_sites
	  (* A word on ordering: We use parmetis to bring the mesh into reasonable
	     proximity order. If we put higher-order elements onto the mesh,
	     and just order the sites lexicographically (as we do here), this will
	     roughly try to preserve that order. It's not really good, but actually
	     not too bad either. If we ordered first by site order (=length of the vector),
	     then lexicographically, that may be much tidier, but would result in quite
	     bad caching/prefetching behaviour.
	  *)
      end
  in
  let v_sites_simplex_info_pmid = compute_sites_simplex_info_pmid () in
  let v_sites = Array.map (fun (x,_,_,_) -> x) v_sites_simplex_info_pmid in
  let v_pmid = Array.map (fun (_,_,_,x) -> x) v_sites_simplex_info_pmid in
  let the_hibernatable_sites_simplex_info_pmid =
    (* (compute_sites_simplex_info_pmid, ref (Some v_sites_simplex_info_pmid)) *)
    (if retain_simplex_info_pmid
     then
       (compute_sites_simplex_info_pmid, ref None)
     else
       ((fun _ -> failwith "Hibernation of sites_simplex_info_pmid turned off!"), ref None))
  in
  let nr_sites = Array.length v_sites in
  let ht_site_outer_region = Hashtbl.create 1987 in
  let dofs =
    (* What follows below is a bit complicated. The basic idea is that we walk all sites,
       all simplices touching that site, and all dofs having that site of the corresponding
       simplex's element.
    *)
    let ht_dofs_by_id = Hashtbl.create 100 in
    let rec walk_sites nr_site have_dofs =
      if nr_site = nr_sites
      then Array.of_list (List.rev have_dofs)
      else
	let (site,v_simplex_ix,vs_abssite,_) = v_sites_simplex_info_pmid.(nr_site) in
	  (* First, we should find out what bodies this site belongs to... *)
	let nr_simplices_this_site = Array.length v_simplex_ix in
	let rec regions known_regions ix_sx_ix =
	  if ix_sx_ix = nr_simplices_this_site
	  then List.sort compare known_regions
	  else
	    let sx_ix = v_simplex_ix.(ix_sx_ix) in
	    let body = simplices.(sx_ix).ms_in_body in
	    let pos =
	      try list_position_if (fun b -> b=body) known_regions
	      with | Not_found -> (-1)
	    in
	      if pos=(-1) then regions (body::known_regions) (1+ix_sx_ix)
	      else regions known_regions (1+ix_sx_ix) in
	  (* We also have to find out whether the "Nonmeshed space" region Body_Nr (-1)
	     (or -something)
	     should be added to this site's regions.

	     We have to do so if this site belongs to a simplex face that does not lead
	     to a neighbouring simplex.

	     The faces a site belongs to are just the faces opposing the points
	     NOT in the site.
	  *)
	let rec check_if_boundary_site v_ix_sx =
	  let nr_simplices = Array.length v_ix_sx in
	  let rec walk ix_sx_ix =
	    if ix_sx_ix = nr_simplices then false
	    else
	      let sx_ix = v_ix_sx.(ix_sx_ix) in
	      let belongs_to_outer_face_of_this_simplex =
		try
		  let outer_faces_opposing_points =
		    Hashtbl.find ht_outer_face_opposing_points_by_sx_id sx_ix
		  in
		    (* If the lookup succeeded (i.e. the simplex has an outer face),
		       and if there is at least one outer-face-opposing point not
		       contained in this site, it actually is on an outer face.
		       Otherwise, we have to investigate the other simplices
		       this site belongs to.
		    *)
		    (try
		       let _ = List.find
			 (fun pt_id -> -1 = array_position_if (fun x -> x=pt_id) site 0)
			 outer_faces_opposing_points
		       in true
		     with | Not_found -> false)
		with | Not_found -> false
	      in
		if belongs_to_outer_face_of_this_simplex
		then true
		else walk (1+ix_sx_ix)
	  in walk 0
	in
	let site_pos =
	  let sx_ix = v_simplex_ix.(0)
	  and nr_abssite = Char.code vs_abssite.[0] in
	  let sx = simplices.(sx_ix) in
	  let elem = v_elements.(v_nel_by_sx_index.(sx_ix)) in
	  let abssite = elem.el_abssites.(nr_abssite) in
	    abssite_coords abssite sx
	in
	let site_regions =
	  regions
	    (if check_if_boundary_site v_simplex_ix
	     then 
	       let r = fun_outer_region nr_site site_pos in
	       let () = Hashtbl.replace ht_site_outer_region nr_site r in
	       [Body_Nr r]
	     else [])
	    0
	in
	let rec walk_simplices ix_sx_ix have_dofs =
	  if ix_sx_ix = Array.length v_simplex_ix
	  then walk_sites (1+nr_site) have_dofs
	  else
	    let sx_ix = v_simplex_ix.(ix_sx_ix) in
	    let nr_abssite = Char.code vs_abssite.[ix_sx_ix] in
	    let nr_elem = v_nel_by_sx_index.(sx_ix) in
	    let elem = v_elements.(nr_elem) in
	    let elem_dof_indices = elem.el_dof_indices_by_abssite_index.(nr_abssite) in
	    let nr_dofs_here = Array.length elem_dof_indices in
	    let rec add_dofs nr_dof have_dofs =
	      if nr_dof = nr_dofs_here
	      then walk_simplices (1+ix_sx_ix) have_dofs
	      else
		let el_dof_ix = elem_dof_indices.(nr_dof) in
		let dof_name = elem.el_dof_names.(el_dof_ix) in
		let dof_id = (dof_name,site) in
		let new_have_dofs =
		  try
		    let seen_dof = Hashtbl.find ht_dofs_by_id dof_id in
		      (* If this succeeded, we already encountered this dof.
			 Just modify it & do not make a new one.
		      *)
		    let () =
		      dof_sx_nel_ix_add seen_dof simplices.(sx_ix) nr_elem el_dof_ix
		    in have_dofs
		  with
		    | Not_found ->
			(* Have to make a new dof *)
			let the_dof =
			  {
			    dof_nr=(Hashtbl.length ht_dofs_by_id); (* a convenient way to count what we already have *)
			    dof_pos=site_pos;
			    dof_site=site;
			    dof_disappears_across=[];
			    dof_sx_nel_ix_fillcount=1;
			    dof_sx_nel_ix=[|sx_ix;(nr_elem lsl 16)+el_dof_ix|];
			    dof_in_body=site_regions;
			    dof_volume=(-1.0); (* not yet initialized properly *)
			  }
			in
			let () = Hashtbl.add ht_dofs_by_id dof_id the_dof in
			  (the_dof::have_dofs)
		in add_dofs (1+nr_dof) new_have_dofs
	    in add_dofs 0 have_dofs
	in walk_simplices 0 have_dofs
    in
    let _ = walk_sites 0 [] in
    (* Actually, we would get the DOFs here, but we extract them via map_hashtbl_to_array below.
       Why? All this is quite a confusing lot, and I had a 14-day break between writing it and
       debugging it, so for now, I do not do major code changes!
     *)
      map_hashtbl_to_array
	~sorter:(fun d1 d2 -> compare d1.dof_nr d2.dof_nr)
	(fun k v -> v)
	ht_dofs_by_id
  in
  let () = Array.iter dof_sx_nel_ix_truncate dofs in
  let dof_nrs_by_simplex_index =
    (* Actually, we could also have computed this above.
       But the code up there is complicated enough as it is.
       So, for the sake of separation and simplicity, we just analyze
       our dofs in a post-processing step to extract that information.

       ===

       Note that this is quite awkward and hackish code, concerning
       how we ensure that the order of the DOFs for a given simplex
       matches the order of the dofs in the abstract element
    *)
    let arr = Array.make nr_simplices [] in
    let () =
      Array.iter
	(fun dof ->
	   dof_sx_nel_ix_iter simplices dof
	     (fun  sx _ el_dof_ix ->
		let sx_id = sx.ms_id in
		  arr.(sx_id) <- (dof.dof_nr,el_dof_ix) :: arr.(sx_id)
	     )
	) dofs
    in
      Array.map 
	(fun li ->
	   let a = Array.of_list li in
	   let () = Array.sort (fun (_,el_ix_1) (_,el_ix_2) -> compare el_ix_1 el_ix_2) a in
	     Array.map (fun (x,_) -> x) a)
	arr
  in
    (* dofs by site is computed in a completely independent post-processing step. *)
  let ht_dofs_by_site =
    let h1 = Hashtbl.create nr_sites
    and h2 = Hashtbl.create nr_sites
    in
    let () = Array.iter (fun dof -> hashtbl_push h1 dof.dof_site dof) dofs
    in
    let () =
      Hashtbl.iter
	(fun k v -> Hashtbl.add h2 k (Array.of_list (List.rev v)))
	h1
    in h2
  in
  (* We also use an independent post-processing step to find out 
     which DOF disappears across which face.
   *)
  let the_dof_name dof =
    let nel_ix = dof.dof_sx_nel_ix.(1) in
      (* Note: this entry MUST be present, otherwise the dof does not belong to any MWE site! *)
    let ix = nel_ix land 0xffff in
    let nr_el = nel_ix lsr 16 in
    let elem = v_elements.(nr_el) in
      elem.el_dof_names.(ix)
  in
  let () =
    Array.iter
      (fun dof ->
	 let (dof_name_stem,_) as dof_name = the_dof_name dof in
	 let fillcount = dof.dof_sx_nel_ix_fillcount in
	 let rec check_simplices n have =
	   if n = fillcount then have
	   else
	     let sx = simplices.(dof.dof_sx_nel_ix.(2*n)) in
	     let faces = site_face_nrs sx dof.dof_site in
	     let rec check_faces rest_faces have =
	       match rest_faces with
		 | [] -> check_simplices (1+n) have
		 | (face_id::rest_rest_faces) ->
		     match sx.ms_neighbours.(face_id) with
		       | None -> (* Okay, this DOF does vanish across the "End of the World" surface *)
			   check_faces rest_rest_faces ((sx,face_id)::have)
		    | Some sx_n ->
			let elem_n = v_elements.(v_nel_by_sx_index.(sx_n.ms_id)) in
			let pos_in_n =
			  array_position_if
			    (fun (n,_) -> n=dof_name_stem)
			    elem_n.el_subfields 0
			in
			  if -1=pos_in_n	(* Yes, this DOF is not present in the neighbour *)
			  then check_faces rest_rest_faces ((sx,face_id)::have)
			  else check_faces rest_rest_faces have
	     in check_faces faces have
	 in
	 let x = check_simplices 0 [] in
	   dof.dof_disappears_across <- x
      ) dofs
  in
  let mwe_distribution =
    let nr_nodes = Array.length mesh.mm_vertex_distribution in
    let vertex_machine nr_vertex =
      let rec walk nr_node rest =
	if nr_node = nr_nodes then impossible()
	else
	  let new_rest = rest-mesh.mm_vertex_distribution.(nr_node) in
	    if new_rest<0 then nr_node
	    else walk (1+nr_node) new_rest
      in
	walk 0 nr_vertex
    in
    let v_dist = Array.make nr_nodes 0 in
      begin
	for i=0 to Array.length dofs-1 do
	  let this_machine = vertex_machine dofs.(i).dof_site.(0) in
	    v_dist.(this_machine) <- v_dist.(this_machine)+1
	    done;
	v_dist
      end
  in
    (* Generalization/extension idea: it would be interesting to provide means to
       specify an affine/projective transformation so that when we set one matrix entry,
       the others are not set the same way, but subject to that affine transformation.
       This way, we could easily implement all sorts of twisted boundary conditions.
       
       TODO student project TODO
    *)
  let nr_dofs = Array.length dofs in
  let periodic_images =
    let nontrivial_images_of_master_dof = Hashtbl.create 17 in
    let () = Array.iteri
      (fun nr_dof dof ->
	 let site = dof.dof_site in
	 let canon_site = Array.map canonicalize_point site in
	   if canon_site = site then ()
	   else
	     let dof_name = the_dof_name dof in
	     let key = (dof_name,canon_site) in
	     let old_nontrivial_images = 
	       try Hashtbl.find nontrivial_images_of_master_dof key with | Not_found -> []
	     in
	       Hashtbl.replace
		 nontrivial_images_of_master_dof key
		 (dof.dof_nr::old_nontrivial_images))
      dofs
    in
      (* for every master dof, we now know the dof indices of all its other images. 
	 But actually, we do not yet know the master dof's own dof indices...
      *)
    let p_i = Array.make nr_dofs [||] in
    let () = hashtbl_foreach_do nontrivial_images_of_master_dof
      (fun (master_dof_name,master_site) li_indices_images ->
	 try 
	   let master_ix = 
	     let mdofs = Hashtbl.find ht_dofs_by_site master_site in
	     let the_mdof = array_find (fun dof -> the_dof_name dof = master_dof_name) mdofs in
	       the_mdof.dof_nr
	   in
	   let v_indices = array_sorted compare (Array.of_list (master_ix::li_indices_images)) in
	     array_foreach_do v_indices
	       (fun ix -> p_i.(ix) <- v_indices)
	 with | Not_found ->
	   failwith
	     (Printf.sprintf "resolving DOF periodicity failed for dof %s, site=%s"
		(dof_name_to_string master_dof_name) (int_array_to_string master_site)))
    in
    let ddd = 
      (if Array.fold_left (fun sf x -> max sf (Array.length x)) 0 p_i >0
       then
	 Printf.printf "=== MWE '%s' DOF periodic images ===\n%s\n%!" name (string_array_to_string (Array.map int_array_to_string p_i))
       else ())
    in
      p_i
  in
  let mwe_mb = 
    MWEMB_make_mwe
      (name,
       properties_by_region,
       Array.init nr_sites
	 (fun nr_site ->
	    try Hashtbl.find ht_site_outer_region nr_site
	    with | Not_found -> 0),
       fun_simplex_to_element)
  in
    {
      mwe_mesh=mesh;
      mwe_made_by=mwe_mb;
      mwe_name=name;
      mwe_elements=v_elements;
      mwe_is_primary=true;
      mwe_subfields=subfields;
      mwe_properties_by_region=ht_prop_by_region;
      mwe_sites=v_sites;
      mwe_pmids=v_pmid;
      mwe_sites_simplex_info_pmid=the_hibernatable_sites_simplex_info_pmid;
      mwe_dofs=dofs;
      mwe_dof_periodic_images=periodic_images;
      mwe_dofs_by_site=ht_dofs_by_site;
      mwe_nel_by_simplex_index=v_nel_by_sx_index;
      mwe_dof_nrs_by_simplex_index=dof_nrs_by_simplex_index;
      (* -- lazy entries -- *)
      mwe_dof_neighbour_pairs = ref None;
      mwe_dof_funs_g = ref None;
      mwe_dof_funs_g_solver = ref None;
      mwe_dof_funs_g_entries = ref [||];
      mwe_dof_funs_volumes = ref None;
      mwe_distribution=mwe_distribution;
      mwe_dofs_and_distribution_by_dvss = Hashtbl.create 1;
    }
;;

let make_mwe name
             ?(debug=false)
	     ?(retain_simplex_info_pmid=false)
	     (* Do we provide bookkeeping data that needs quite a bit of memory
		and is not really used at present? *)
             ?(fun_outer_region=fun _ _ -> -1)
             ?(properties_by_region=[||])
             fun_simplex_to_element
             mesh =
  let mwe =
    debug_make_mwe
      name
      ~retain_simplex_info_pmid
      ~fun_outer_region
      ~properties_by_region
      fun_simplex_to_element
      mesh
  in
  let () =
    if debug then
      let report = mwe_memstats_str (mwe_memstats mwe) in
        Printf.printf
          ("*** MEMORY STATISTICS FOR '%s' ***\n%s*** END MEMORY STATISTICS FOR '%s'***\n") name report name
    else ()
(*  in
  let ddd = 
    begin
      Printf.printf "DDD CPU %d === mwe \"%s\" properties by region ===\n" (Mpi_petsc.comm_rank (Mpi_petsc.petsc_get_comm_world())) mwe.mwe_name;
      Hashtbl.iter (fun (Body_Nr k) v -> Printf.printf "%d => %s\n" k (string_array_to_string v)) mwe.mwe_properties_by_region
    end
*)
  in
    mwe
;;

let mwe_ensure_has_neighbour_pairs mwe =
  match !(mwe.mwe_dof_neighbour_pairs) with
    | Some _ -> () (* We already have that information *)
    | None ->
	let v_ht_pairs =
	  Array.init
	    (Array.length mwe.mwe_subfields) (fun _ -> Hashtbl.create 1987)
	in
	let dof_is_leading dof_name =
	  let (_,indices) = dof_name in
	    array_all_satisfy (fun x -> x=0) indices
	in
	let ht_sites_by_leading_dof = Hashtbl.create 17 in
	let () =
	  array_foreach_do_n mwe.mwe_nel_by_simplex_index
	    (fun nr_simplex nr_el ->
	       let sx = mwe.mwe_mesh.mm_simplices.(nr_simplex) in
	       let el = mwe.mwe_elements.(nr_el) in
	       let sx_vertices = Array.map (fun p -> p.mp_id) sx.ms_points in
	       let sites = Array.map (fun a -> fold_out_abssite a sx_vertices) el.el_abssites in
		 begin
	           Hashtbl.clear ht_sites_by_leading_dof;
		   Array.iteri
		     (fun ix_dof_orig dof_name ->
			if not(dof_is_leading dof_name)
			then ()
			else
			  hashtbl_push
			    ht_sites_by_leading_dof dof_name
			    sites.(el.el_abssite_index_by_dof_index.(ix_dof_orig)))
		     el.el_dof_names;
		   hashtbl_foreach_do ht_sites_by_leading_dof
		     (fun ((dof_stem,_) as leading_dof) sites ->
			let v_sites = Array.of_list sites in
			let () = Array.sort compare v_sites in
			let nr_sites = Array.length v_sites in
			let ix_subfield =
			  array_position_if (fun (stem,_) -> stem=dof_stem) el.el_subfields 0
			in
			let ht = v_ht_pairs.(ix_subfield) in
			  begin
			    for i=0 to nr_sites-1 do
			      let site_i = v_sites.(i) in
			      let dof_i =
				array_find
				  (fun dof -> the_dof_name mwe dof = leading_dof) 
				  (Hashtbl.find mwe.mwe_dofs_by_site site_i)
			      in
			      let dof_nr_i = dof_i.dof_nr in
				for j=i+1 to nr_sites-1 do
				  let site_j = v_sites.(j) in
				  let dof_j =
				    array_find
				      (fun dof -> the_dof_name mwe dof = leading_dof) 
				      (Hashtbl.find mwe.mwe_dofs_by_site site_j)
				  in
				  let dof_nr_j = dof_j.dof_nr in
				    Hashtbl.replace ht 
				      (if dof_nr_i < dof_nr_j
				       then (dof_nr_i,dof_nr_j)
				       else (dof_nr_j,dof_nr_i))
				      true;
				done
			    done
			  end
		     );
		 end)
	in
	let neighbours =
	  Array.map 
	    (fun ht ->
	       let pairs = hashtbl_keys ~sorter:compare ht in
		 ((Array.map (fun (x,_) -> x) pairs),
		  (Array.map (fun (_,x) -> x) pairs)))
	    v_ht_pairs
	in
	  mwe.mwe_dof_neighbour_pairs := Some neighbours
;;     

(* We may want to obtain "connectivity" information for a given
   dof_stem: find the groups of dofs that have the same name and
   are connected via the transitive closure of overlapping 
   "dof tent functions". What is this for? Answer: finding
   nullspaces for von-Neumann-boundary-condition solvers!
*)

let mwe_dof_connectivity mwe dof_stem =
  let () = mwe_ensure_has_neighbour_pairs mwe in
  let nr_subfield = array_position_if (fun (n,_) -> n = dof_stem) mwe.mwe_subfields 0 in
  let () =
    (if nr_subfield == (-1)
     then failwith (Printf.sprintf "mwe_dof_connectivity: bad dof name stem: '%s'" dof_stem)
     else ())
  in
  let (_,max_indices) = mwe.mwe_subfields.(nr_subfield) in
  let neighbours =
    match !(mwe.mwe_dof_neighbour_pairs) with
      | None -> impossible ()
      | Some pairs_by_subfield_index ->
	  let (pairs_le,pairs_ri) = pairs_by_subfield_index.(nr_subfield) in
	    (graph_components
	       (fun ~produce ->
		  let xproduce x y = produce x y (x,y,1) in
		    for np = 0 to Array.length pairs_le-1 do
		      let ix_le_all_field_indices_zero=pairs_le.(np)
		      and ix_ri_all_field_indices_zero=pairs_ri.(np)
		      in
			multifor max_indices
			  (fun n _ ->
			     xproduce
			       (ix_le_all_field_indices_zero+n)
			       (ix_ri_all_field_indices_zero+n))
		    done
	       ))
  in
    map_hashtbl_to_array ~sorter:compare (fun _ (group,_) -> group) neighbours
;;

(* Note: the Petsc manual says these should be orthonormal! *)
let mwe_matnullspace_vectors ~fun_make_vector ~fun_add_entry ~fun_assemble_vector mwe dof_stems =
  let pieces = 
    array_join (Array.map (fun s -> mwe_dof_connectivity mwe s) dof_stems) in
  let nr_pieces = Array.length pieces in
  let vecs = Array.init nr_pieces (fun n -> fun_make_vector ~nr_vec:n) in
    begin
      for i=0 to nr_pieces-1 do
	let vec = vecs.(i) 
	and piece = pieces.(i)
	in
	  begin
	    let nr_entries = Array.length piece in
	    let z = 1.0/.(sqrt(float_of_int(nr_entries))) in
	    for j=0 to nr_entries-1 do
	      fun_add_entry vec piece.(j) z
	    done;
	    fun_assemble_vector vec
	  end
      done;
      vecs
    end
;;

  

(* NOTE: we provide equalization only for fields, not for co-fields,
   and we are not especially picky about relative weights. After all,
   only numerical drift should have caused them to be non-equal in the
   first place!
*)
let equalize_periodic_field
    (FEM_field (mwe_src,restr_src,v_src))
    (FEM_field (mwe_dst,restr_dst,v_dst))
    opt_dof_stems =
  let () = (if mwe_src != mwe_dst	(* Note: testing for EQ-ness here! *)
	      || restr_src <> restr_dst
	    then failwith "equalize_periodic_field: field types must be PRECISELY equal!"
	    else ())
  in
  let mwe = mwe_src and restr = restr_src in
  let check_if_needs_equalizing =
    match opt_dof_stems with
      | None -> (fun _ -> true)
      | Some dof_stems -> (fun stem -> -1<>array_position stem dof_stems 0)
  in
  match restr_src with
    | Some _ -> failwith "XXX equalize_periodic_field() cannot handle restricted fields (TODO)"
    | None ->
	let periodic_info = mwe.mwe_dof_periodic_images in
	let nr_dofs = Array.length mwe.mwe_dofs in
	  Mpi_petsc.with_petsc_vector_as_bigarray v_src
	    (fun ba_v_src ->
	       Mpi_petsc.with_petsc_vector_as_bigarray v_dst
		 (fun ba_v_dst ->
		    for i=0 to nr_dofs-1 do
		      let p_i = periodic_info.(i) in
			if Array.length p_i > 0 && p_i.(0) = i
			  && (let (stem,_) = the_dof_name mwe mwe.mwe_dofs.(i) in
				check_if_needs_equalizing stem)
			then
			  let average =
			    (Array.fold_left (fun sf ix -> sf+.ba_v_src.{ix}) 0.0 p_i)/.
			      (float_of_int (Array.length p_i))
			  in
			    Array.iter (fun ix -> ba_v_dst.{ix} <- average) p_i
			else ba_v_dst.{i} <- ba_v_src.{i}
		    done
		 ))
;;
	  
(* The definition of diffop_id had to be moved upwards, so that
   mwe_ensure... can use it! *)

let integrate_field_slow ?dof_stem (FEM_field (mwe,_,v) as field) =
  let () = ensure_field_is_unrestricted field in
  let () = mwe_ensure_has_volumes mwe in 
  let vols = match !(mwe.mwe_dof_funs_volumes) with
    | None -> impossible ()
    | Some x -> x
  in
  let filter_this_dof_name =
    match dof_stem with
      | None -> (fun _ -> true)
      | Some stem ->
	  fun (s,_) -> s=stem
  in
  let all_dofs = mwe.mwe_dofs in
  let nr_dofs = Array.length all_dofs in
  let ht_contribs = Hashtbl.create 10 in
  let () =
    Mpi_petsc.with_petsc_vector_as_bigarray v
      (fun v_ba ->
	 for i=0 to nr_dofs-1 do
	   let this_dof = all_dofs.(i) in
	   let dof_name = the_dof_name mwe this_dof in
	     if not(filter_this_dof_name dof_name)
	       || (let a = mwe.mwe_dof_periodic_images.(i) in Array.length a > 1 && a.(0) <> i)
	       (* Do not include mirage dofs in the integral! *)
	     then ()
	     else
	       let contrib_so_far =
		 try Hashtbl.find ht_contribs dof_name
		 with | Not_found -> 0.0
	       in
	       let new_contrib = contrib_so_far+.v_ba.{i}*.vols.(i)
	       in
		 Hashtbl.replace ht_contribs dof_name new_contrib
	 done)
  in
    map_hashtbl_to_array ~sorter:compare (fun k v -> (k,v)) ht_contribs
;;

(* integrate_field requires summing contributions for each separate component
   of a field. Example: the magnetisation M_Py has 3 components, so we should 
   run over all the dofs, consider only those corresponding to M_Py and sum
   the contributions individually for M_Py(0), M_Py(1) and  M_Py(2).
   This could be done by creating an hashtable and using dof_names
   as keys (remember, the dof_name indentifies univocally M_Py(i)).
   However hashtables are evil! They involve hashing and structural
   comparisons, leading to a major performance penalty! This function is a fix
   for that. The idea is the following: we first look at the max-indices
   for the required field stem. In the previous case we get M_Py(3).
   We then know that we will have 3 dinstinct contributions. In the case of
   a tensor we could get T(3, 4, 5), meaning that we should collect 3*4*5=60
   kinds of contributions. We then return an array which has the appropriate
   size to contain all these and functions to map a dof_name to the right
   index and viceversa. These three elements are enough to replace the use
   of hashtable and make the code more performant! 

   mf, 3 Jun 2009 *)
let multi_dim_array_handlers max_indices =
  let num_components =
    Array.fold_left
      (fun accum max_idx -> accum*max_idx) 1 max_indices
  in
  let index_from_indices indices =
    let rec walk i idx =
      if i < Array.length indices then
        walk (i + 1) (idx*max_indices.(i) + indices.(i))
      else
        idx
    in walk 0 0
  in
  let indices_from_index index =
    let n = Array.length max_indices in
    let indices = Array.create n 0 in
    let rec walk i idx =
      if i < 0 then
        ()
      else
        let () = indices.(i) <- idx mod max_indices.(i) in
          walk (i - 1) (idx/max_indices.(i))
    in
    let () = walk (n - 1) index
    in
      indices
  in
    (num_components, index_from_indices, indices_from_index)
;;

let integrate_field_fast dof_stem (FEM_field (mwe,_,v) as field) =
  let () = ensure_field_is_unrestricted field in
  let () = mwe_ensure_has_volumes mwe in 
  let (Some vols) = !(mwe.mwe_dof_funs_volumes) in
  let all_dofs = mwe.mwe_dofs in
  let nr_dofs = Array.length all_dofs in
  try
    let (stem, max_indices) =
      array_find (fun (n, _) -> n = dof_stem) mwe.mwe_subfields in
    let num_components, index_from_indices, indices_from_index =
        multi_dim_array_handlers max_indices in
    let components = Array.create num_components 0.0 in
    let () =
      Mpi_petsc.with_petsc_vector_as_bigarray v
        (fun v_ba ->
	   for i=0 to nr_dofs-1 do
	     let this_dof = all_dofs.(i) in
	     let stem, indices = the_dof_name mwe this_dof in
	       if stem <> dof_stem
	         || (let a = mwe.mwe_dof_periodic_images.(i) in
	               Array.length a > 1 && a.(0) <> i)
	         (* Do not include mirage dofs in the integral! *)
	       then ()
	       else
	         let new_contrib = v_ba.{i}*.vols.(i) in
	         let index = index_from_indices indices in
	           components.(index) <- components.(index) +. new_contrib
	   done)
    in
      Array.mapi
        (fun index value ->
           let dn:dof_name = (dof_stem, indices_from_index index)
           in
             (dn, value))
        components
  with
    Not_found -> [||]
;;


(*let integrate_field = integrate_field_slow;;*)

let timer_start, timer_stop =
  let t = ref 0.0 in
  let num_stops = ref 0 in
  ((fun () -> t := !t -. Unix.gettimeofday ()),
   (fun () -> 
      begin
        num_stops := !num_stops + 1;
        t := !t +. Unix.gettimeofday ();
        if !num_stops mod 10 = 0 then
          Printf.printf "num_stops=%4d time=%5.2f\n%!"
          !num_stops !t
        else ();
      end))
;;
 
let integrate_field ?dof_stem field =
  (*let () = timer_start () in
  let x =*)
    match dof_stem with
      None -> integrate_field_slow ?dof_stem field
    | Some dn -> integrate_field_fast dn field
(*  in
  let () = timer_stop () in
    x*)
;;

let probe_field (FEM_field (mwe,_,v_field) as field) ?dof_stem pos =
  let simplices=mwe.mwe_mesh.mm_simplices in
  let () = ensure_field_is_unrestricted field in
  let filter_this_dof_name =
    match dof_stem with
      | None -> (fun _ -> true)
      | Some stem ->
	  fun (s,_) -> s=stem
  in
  let ht_field = Hashtbl.create 5 in
    try
      let (simplex,coords_L) = mesh_locate_point mwe.mwe_mesh pos in
	(* Note: this raises Not_found if the point could not be located! *)
      let Some inv_ext_point_coords = simplex.ms_inv_ext_point_coords in
	(* ^ Note: this cannot be None! *)
      let sx_id = simplex.ms_id in
      let elem = mwe.mwe_elements.(mwe.mwe_nel_by_simplex_index.(sx_id))
      and dofs = Array.map (fun n -> mwe.mwe_dofs.(n)) mwe.mwe_dof_nrs_by_simplex_index.(sx_id)
      in
      let () = Mpi_petsc.with_petsc_vector_as_bigarray v_field
	(fun ba_field ->
	   Array.iter
	     (fun dof ->
		let dof_name = the_dof_name mwe dof in
		  if not(filter_this_dof_name dof_name)
		  then ()
		  else
		    let (_,nr_elem,ix) = dof_sx_nel_ix_find simplices dof (fun s _ _ -> s==simplex) in
		    let dof_femfun = elem.el_dof_funs.(ix) in
		    let dof_val = ba_field.{dof.dof_nr} *. (femfun_eval inv_ext_point_coords dof_femfun coords_L) in
		      hashtbl_increase (+.) ht_field dof_name dof_val)
	     dofs)
      in
	hashtbl_to_array ~sorter:compare ht_field
    with | Not_found -> [||]
;;



(* XXX NOTE: quite some of the plotting functions have been obsoleted by now.
   Rationale: they could not handle higher-order elements properly.
 *)

let _plot_scalar_field_ps_header="
% Color-Interpolated triangle

/cavg3 % (c0,c1,c2) => cm -- color average of three colors
{
 dup 0 get 0 get exch % c0[0] c012
 dup 1 get 0 get exch % c0[0] c1[0] c012
 dup 2 get 0 get exch % c0[0] c1[0] c2[0] c012
 4 1 roll % c012 c0[0] c1[0] c2[0]
 add add 0.333333333 mul exch % avg c012
 %%
 dup 0 get 1 get exch
 dup 1 get 1 get exch
 dup 2 get 1 get exch
 4 1 roll
 add add 0.333333333 mul exch
 %%
 dup 0 get 2 get exch
 dup 1 get 2 get exch
 dup 2 get 2 get exch
 4 1 roll
 add add 0.333333333 mul exch pop
 3 array astore
} def

/cavg2 % (r0,g0,b0) (r1,g1,b1) => (rm,gm,bm)
{
  dup 2 get 3 1 roll exch dup 2 get 3 1 roll exch
  dup 1 get 3 1 roll exch dup 1 get 3 1 roll exch
  dup 0 get 3 1 roll exch dup 0 get 3 1 roll exch
  pop pop
  add 0.5 mul 5 1 roll add 0.5 mul 3 1 roll add 0.5 mul
  3 array astore
} def


/pavg2 % (x0,y0,c0) (x1,y1,c1) => (xm,ym,cm)
{
  dup 0 get 3 1 roll exch dup 0 get 4 3 roll add 0.5 mul 3 1 roll
  dup 1 get 3 1 roll exch dup 1 get 4 3 roll add 0.5 mul 3 1 roll
  dup 2 get 3 1 roll exch dup 2 get 4 3 roll cavg2 3 1 roll
  pop pop
  3 array astore
} def

% interior of color triangle
/ctri_i % ((x0,y0,c0),(x1,y1,c1),(x2,y2,c2)) level
{
 dup 0 eq
 { % if level=0
   pop
   dup 0 get 2 get exch
   dup 1 get 2 get exch
   dup 2 get 2 get exch
   4 1 roll 3 array astore cavg3 aload pop setrgbcolor
   % Color is set now.
   dup 0 get 0 get exch
   dup 0 get 1 get exch 3 1 roll moveto
   dup 1 get 0 get exch
   dup 1 get 1 get exch 3 1 roll lineto
   dup 2 get 0 get exch
   dup 2 get 1 get exch 3 1 roll lineto
   closepath fill stroke
   pop
 }
 { % else (level>0)
   exch aload pop 4 3 roll
   1 sub % level-1
   dup dup dup
   7 4 roll 3 array astore % 4x level p012
   dup 0 get exch dup 1 get exch % 4x level p0 p1 p012
   3 1 roll pavg2 exch % 4x level p01 p012
   dup 0 get exch dup 2 get exch
   3 1 roll pavg2 exch % 4x level p01 p02 p012
   dup 1 get exch dup 2 get exch
   3 1 roll pavg2 exch % 4x level p01 p02 p12 p012
   aload pop % 4x level p01 p02 p12 p0 p1 p2
   6 array astore % 4x level p-all
   dup 3 get exch dup 0 get exch dup 1 get exch % 4x level p0 p01 p02 p-all
   5 1 roll 3 array astore exch ctri_i
   dup 4 get exch dup 2 get exch dup 0 get exch % p1 p12 p01
   5 1 roll 3 array astore exch ctri_i
   dup 5 get exch dup 1 get exch dup 2 get exch % p2 p02 p12
   5 1 roll 3 array astore exch ctri_i
   dup 0 get exch dup 1 get exch dup 2 get exch % p01 p02 p12
   5 1 roll 3 array astore exch ctri_i
   pop
 }
 ifelse
} def

% boundary of color triangle
/ctri_b % ((x0,y0,c0),(x1,y1,c1),(x2,y2,c2))
{
  0 0 0 setrgbcolor
  dup 0 get 0 get exch
  dup 0 get 1 get exch 3 1 roll moveto
  dup 1 get 0 get exch
  dup 1 get 1 get exch 3 1 roll lineto
  dup 2 get 0 get exch
  dup 2 get 1 get exch 3 1 roll lineto
  closepath stroke
  pop
} def

/ctri % ((x0,y0,c0), (x1,y1,c1), (x2,y2,c2)) level
{
  exch dup 3 2 roll
  ctri_i
  ctri_b
} def

/ictri_i % indexed color triangle - inner: ix0 ix1 ix2 level
{
  points 5 4 roll get
  points 5 4 roll get
  points 5 4 roll get
  3 array astore exch
  ctri_i
} def

/ictri_b
{
 points 4 3 roll get
 points 4 3 roll get
 points 4 3 roll get
 3 array astore ctri_b
} def

/ictri % ix0 ix1 ix2 level
{
  points 5 4 roll get
  points 5 4 roll get
  points 5 4 roll get
  3 array astore dup
  3 2 roll
  ctri_i ctri_b
} def

"
;;


let mesh2d_plot_scalar_field
    ?(scale=(fun pos -> [|50.0*.pos.(0)+.300.0;50.0*.pos.(1)+.500.0|]))
    ?(plot_order=3)
    ?(plot_edges=true)
    plot_dof_name
    (FEM_field(mwe,_,v_petsc) as field)
    color_scheme
    filename
    =
  let () = ensure_field_is_unrestricted field in
  let data = Mpi_petsc.vector_extract v_petsc in
  let nr_simplices = Array.length mwe.mwe_mesh.mm_simplices in
  let ht_site_to_ps_index = Hashtbl.create 100 in
  let v_sites_posns_vals =
    array_mapfilterij
      (fun _ ix_new dof ->
	 let dof_name = the_dof_name mwe dof in
	   if dof_name=plot_dof_name
	   then
	     let () = Hashtbl.replace ht_site_to_ps_index dof.dof_site ix_new in
	       Some (dof.dof_site,dof.dof_pos,data.(ix_new))
	   else None)
      mwe.mwe_dofs
  in
  let print_points fh =
    begin
      Printf.fprintf fh "0.5 setlinewidth\n"; (* Change default line width *)
      Printf.fprintf fh "/points\n";
      Array.iter
	(fun (site,pos,x) ->
	  let spos = scale pos in
	  let color = color_interpolate_piecewise color_scheme x in
	  Printf.fprintf fh
	    "%8.4f %8.4f  %5.3f %5.3f %5.3f 3 array astore 3 array astore\n"
	    spos.(0) spos.(1) color.(0) color.(1) color.(2)
	) v_sites_posns_vals;
      Printf.fprintf fh "%d array astore def\n"
	(Array.length v_sites_posns_vals);
    end
  in
  let out_fh = open_out filename in
  begin
    Printf.fprintf out_fh "%s" _plot_scalar_field_ps_header;
    print_points out_fh;
    (* Plot the triangles *)
    (* XXXXXXXXX DONE UP TO HERE - NEED MORE DEBUGGING! do not have el,v_dofs - look how we did it
       earlier, searching for:

    let elem = mwe.mwe_elements.(mwe.mwe_nel_by_simplex_index.(sx_id))
    and dofs = Array.map (fun n -> mwe.mwe_dofs.(n)) mwe.mwe_dof_nrs_by_simplex_index.(sx_id)

     *)
    for sx_id = 0 to nr_simplices-1 do
      (* let el = mwe.mwe_elements.(mwe.mwe_nel_by_simplex_index.(sx_id)) in *)
      let v_dofs = Array.map (fun n -> mwe.mwe_dofs.(n)) mwe.mwe_dof_nrs_by_simplex_index.(sx_id)
      in
      let relevant_dofs =
	array_filter
	  (fun d -> the_dof_name mwe d = plot_dof_name)
	  v_dofs in
      let nr_relevant = Array.length relevant_dofs in
	if nr_relevant=0 then () (* Special cases that can be handled fast *)
	else if nr_relevant = 3
	then
	  Printf.fprintf out_fh "%d %d %d  %d ictri_i\n"
	    (Hashtbl.find ht_site_to_ps_index relevant_dofs.(0).dof_site)
	    (Hashtbl.find ht_site_to_ps_index relevant_dofs.(1).dof_site)
	    (Hashtbl.find ht_site_to_ps_index relevant_dofs.(2).dof_site)
	    plot_order
	else if nr_relevant <3 then ()
	  (* Actually, this presumably hints at a bug in the
	     DOF allocation. We just ignore that. *)
	else
	  let dof_posns = Array.map (fun d -> d.dof_pos) relevant_dofs in
	  let sub_triangles = Qhull.delaunay dof_posns in
	  let effective_order =
	    max 0
	      (1+plot_order-(Array.fold_left
			       (fun sf x -> max sf (Array.length x.dof_site))
			       0 relevant_dofs))
	  in
	    Array.iter
	      (fun sub_tri ->
		 Printf.fprintf out_fh "%d %d %d  %d ictri_i\n"
		   (Hashtbl.find ht_site_to_ps_index relevant_dofs.(sub_tri.(0)).dof_site)
		   (Hashtbl.find ht_site_to_ps_index relevant_dofs.(sub_tri.(1)).dof_site)
		   (Hashtbl.find ht_site_to_ps_index relevant_dofs.(sub_tri.(2)).dof_site)
		   effective_order)
	      sub_triangles
    done;
    (* Afterwards, optionally also plot the edges *)
    (if plot_edges then
       for sx_id = 0 to nr_simplices-1 do
	 (* let el = mwe.mwe_elements.(mwe.mwe_nel_by_simplex_index.(sx_id)) in *)
	 let v_dofs = Array.map (fun n -> mwe.mwe_dofs.(n)) mwe.mwe_dof_nrs_by_simplex_index.(sx_id)
	 in
	 let relevant_dofs =
	   array_filter
	     (fun d -> (Array.length d.dof_site =1)
		&& (the_dof_name mwe d= plot_dof_name))
	     v_dofs in
	 let nr_relevant = Array.length relevant_dofs in
	   if nr_relevant = 3
	   then
	     Printf.fprintf out_fh "%d %d %d ictri_b\n"
	       (Hashtbl.find ht_site_to_ps_index relevant_dofs.(0).dof_site)
	       (Hashtbl.find ht_site_to_ps_index relevant_dofs.(1).dof_site)
	       (Hashtbl.find ht_site_to_ps_index relevant_dofs.(2).dof_site)
	   else ()
       done
     else ());
    close_out out_fh;
    ()
  end
;;

let debugprint_field (FEM_field (mwe,r,v_petsc) as field) =
  let () = ensure_field_is_unrestricted field in
  let v_ml = Mpi_petsc.vector_extract v_petsc in
  Array.iteri
    (fun nr_dof dof ->
      Printf.printf "%20s @ %s: %f\n"
	(dof_name_to_string (the_dof_name mwe dof))
	(float_array_to_string dof.dof_pos)
	v_ml.(nr_dof)
    ) mwe.mwe_dofs
;;

let debugprint_cofield (FEM_cofield (mwe,r,v_petsc) as cofield) =
  let () = ensure_cofield_is_unrestricted cofield in
  let v_ml = Mpi_petsc.vector_extract v_petsc in
  Array.iteri
    (fun nr_dof dof ->
      Printf.printf "%20s @ %s: %f\n"
	(dof_name_to_string (the_dof_name mwe dof))
	(float_array_to_string dof.dof_pos)
	v_ml.(nr_dof)
    ) mwe.mwe_dofs
;;

(* XXX sample_field has to be changed in such a way that we also can sample restricted/short fields.
   For now, pyfem3 uses its own sampling code.
*)

let sample_field ?(petsc_name=gensym "field") mwe f =
  let () = logdebug (Printf.sprintf "Inside sample field!%!") in
  let nr_dofs = Array.length mwe.mwe_dofs in
  let () = logdebug (Printf.sprintf "Creating petsc vector%!") in
  let v_target = Mpi_petsc.vector_create nr_dofs petsc_name in
  let () = logdebug (Printf.sprintf "Assembling petsc vector%!") in
  let () = Mpi_petsc.vector_assemble v_target in
  let () = logdebug (Printf.sprintf "Sampling petsc vector!%!") in
  let () =
    Mpi_petsc.with_petsc_vector_as_bigarray v_target
      (fun ba_target ->
	for i=0 to nr_dofs-1 do
	  let dof = mwe.mwe_dofs.(i) in
	  let dof_name = the_dof_name mwe dof in
	  let v = f dof_name dof in
	  ba_target.{i} <- v
	done
      )
  in
  FEM_field(mwe,None,v_target)
;;

(* Using a different interface for the function f...
   XXX should allow target argument!
 *)
let sample_field_type2 ?(petsc_name=gensym "field") mwe f =
  let () = logdebug (Printf.sprintf "Inside sample field!%!") in
  let nr_dofs = Array.length mwe.mwe_dofs in
  let () = logdebug (Printf.sprintf "Creating petsc vector%!") in
  let v_target = Mpi_petsc.vector_create nr_dofs petsc_name in
  let () = logdebug (Printf.sprintf "Assembling petsc vector%!") in
  let () = Mpi_petsc.vector_assemble v_target in
  let () = logdebug (Printf.sprintf "Sampling petsc vector!%!") in
  let () =
    Mpi_petsc.with_petsc_vector_as_bigarray v_target
      (fun ba_target ->
	for i=0 to nr_dofs-1 do
	  let dof = mwe.mwe_dofs.(i) in
	  let dof_name = the_dof_name mwe dof in
	  let v = f dof_name dof.dof_pos in
	  ba_target.{i} <- v
	done
      )
  in
  FEM_field(mwe,None,v_target)
;;


(* XXX TODO: need better sample_field which probes the function at more places,
   trying to do a least-squares approximation! *)

let set_field field f =
  let () = ensure_field_is_unrestricted field in
  let FEM_field (mwe,_,v_target ) = field in
  let nr_dofs = Array.length mwe.mwe_dofs in
  let () =
    Mpi_petsc.with_petsc_vector_as_bigarray v_target
      (fun ba_target ->
	for i=0 to nr_dofs-1 do
	  let dof = mwe.mwe_dofs.(i) in
	  let dof_name = the_dof_name mwe dof in
	  let v = f dof_name dof in
	  ba_target.{i} <- v
	done
      )
  in
    ()
;;

(* Set the given field to the same value at every point in space.
   'dof_stem' and 'field' define the subfield to be set, while 'value'
   is a Bigarray.Genarray with the value to be used in the operation.
   The function returns true if some values of the field have been
   set, otherwise it returns false. *)
let set_field_uniformly dof_stem (FEM_field (mwe, _, v) as field) value =
  let () = ensure_field_is_unrestricted field in
  let all_dofs = mwe.mwe_dofs in
  let nr_dofs = Array.length all_dofs in
  let have_set_something = ref false in
  try
    let (stem, max_indices) =
      array_find (fun (n, _) -> n = dof_stem) mwe.mwe_subfields in
    let num_components, index_from_indices, indices_from_index =
        multi_dim_array_handlers max_indices
    in
    let () =
      Mpi_petsc.with_petsc_vector_as_bigarray v
        (fun v_ba ->
	   for i=0 to nr_dofs-1 do
	     let this_dof = all_dofs.(i) in
	     let stem, indices = the_dof_name mwe this_dof in
	       if stem <> dof_stem
	       then ()
	       else
                 let () = have_set_something := true in
                   v_ba.{i} <- Bigarray.Genarray.get value indices
	   done)
    in !have_set_something
  with
    Not_found -> false
;;

(* The computation of the demagnetizing field is quite costly.
   Unfortunately, the fast methods (FMM) have a quite high break-even
   point, so it makes sense to also provide O(N^2) direct summation
   techniques for smaller problem sets.

   Note: Here, we just approximate every site as a point-dipole.
   We may want to use a second "correction" function that subtracts
   the nearest-neighbour dipole-dipole contributions and replaces
   them by better approximations.

   Note also: it definitely does make sense to keep an eye on the
   possibility to do some further pre-reduction/coarsening before
   applying the direct summation method (after all, if we already
   agreed upon concentrating all the magnetization in a "tent
   function" at one place, we may take this just a small step further
   as well, right?)

   Hence, it is important to design the interface in such a way that
   the fast part of the fast summation code just sees that data which
   it really has to see in order to do the summation. (So by not
   having any extra assumptions in the fast part of the code, we may
   re-use it for all kinds of coarsening schemes...)

   It is permissible to spend some setup or O(N) time copying data
   into the appropriate places.


*)

(* internal: *)
external dipole_evaluator_sumpairs_fast_ccode:
  (int * int * float)  ->
  (* Parameters:
     dimension, result_range_offset,
     minimal distance to take a pair into account
  *)
  float array ->
  (* source and result positions, dim*nr_sites entries
     Note: we assume that source and result fields
     use the same positions.
     This is checked/guaranteed in the ML code.
  *)
  float array -> (* Source strengths, same format *)
  float array -> (* result strengths,
		    again, same format,
		    but this is only a chunk of the whole array
		 *)
  unit
  = "caml_dipole_evaluator_sumpairs_fast_ccode"
;;

(* We need means to produce a fine mesh from a coarse mesh, as well as
   means to transport fields from corresponding mwes - that is,
   essentially a wrapper around make_mwe that takes two meshes and
   produces two mwes and data vector translation functions in both 
   directions.

   Note: as this is an operation to be used once on a mesh of reasonable size,
   there is little incentive trying to optimize performance...
*)

(** Given a way to refine simplices, create a finer mesh from a given coarse mesh...
    This returns the finer mesh, as well as fine-mesh-points information.
*)

let finer_mesh_from_coarse_mesh simplex_refinement coarse_mesh =
  (* simplex_refinement is an array of abstract sub-simplices,
     expressed as abssite arrays.
  *)
  let ht_new_point = Hashtbl.create 100 in
  let ht_new_point_some_parent_simplex = Hashtbl.create 100 in
  let instantiate_abssite nr_simplex points abssite =
    let weight_factor = 1.0/.(float_of_int (Array.fold_left (+) 0 abssite)) in
    let dim=Array.length points.(0).mp_coords in
    let abstract_position =
      let arr = Array.mapi (fun n p -> (p.mp_id,abssite.(n))) points in
      let filtered_arr = array_filter (fun (_,b) -> b>0) arr in             (* we keep only the points whose sites weights are > 0 *)
      let () = Array.sort (fun (a,_) (b,_) -> compare a b) filtered_arr
      in
	(Array.map (fun (x,_) -> x) filtered_arr,
	 Array.map (fun (_,x) -> x) filtered_arr)
    in
    let p = Array.make dim 0.0 in
    let () =
      begin
	for i=0 to (Array.length abssite)-1 do
	  for k=0 to dim-1 do
	    p.(k) <- p.(k) +. (float_of_int abssite.(i)*.points.(i).mp_coords.(k));
	  done;
	done;
	for k=0 to dim-1 do
	  p.(k) <- p.(k)*.weight_factor
	done;
      end
    in
      try
	let looked_up = Hashtbl.find ht_new_point abstract_position in
	  looked_up
      with | Not_found ->
	let new_ix = Hashtbl.length ht_new_point in
	let point_desc=(abstract_position,p,new_ix) in
	let () = Hashtbl.add ht_new_point abstract_position point_desc in
	let () = (* We also make sure we know at least one simplex this point belongs to.
		    This will become important later on when we create the mapping functions
		    from the coarse to the fine mesh and vice versa.
		 *)
	  Hashtbl.add ht_new_point_some_parent_simplex new_ix nr_simplex in
	  point_desc
  in
  let coarse_simplices = coarse_mesh.mm_simplices in
  let nr_coarse_simplices = Array.length coarse_simplices in
  let rec walk_coarse_simplices n small_simplices_and_regions =
    if n = nr_coarse_simplices
    then array_join (Array.of_list (List.rev small_simplices_and_regions))
    else
      let sx = coarse_simplices.(n) in
      let points = sx.ms_points in
      let new_simplices_and_regions_and_parent =
	Array.map 
	  (fun refinements ->
	     let new_point_indices =
	       Array.map (fun abssite -> let (_,_,ix) = instantiate_abssite n points abssite in ix)
		 refinements
	     in
	     let Body_Nr region = sx.ms_in_body in
	       (new_point_indices,region,n)
	  )
	  simplex_refinement
      in walk_coarse_simplices (1+n) (new_simplices_and_regions_and_parent :: small_simplices_and_regions)
  in
  let new_simplices_and_regions_and_parent = walk_coarse_simplices 0 [] in
    (* Now, every sub_simplex is known as a vector of new-point indices.
       We have to build an array to actually look them up by index... *)
  let nr_new_points = Hashtbl.length ht_new_point in
  let arr_new_point_by_index =
    let some_element =
      match hashtbl_arbitrary_element ht_new_point with
	| None -> impossible()
	| Some (_,v) -> v
    in
    let a = Array.make nr_new_points some_element in
    let () = Hashtbl.iter (fun _ ((_,pos,ix) as entry) -> a.(ix) <- entry) ht_new_point in
      a
  in
  let fine_mesh =
    match
      read_mesh_from_points_and_simplices
	(Array.map (fun (_,pos,_) -> pos) arr_new_point_by_index)
	(Array.map (fun (points,_,_) -> points) new_simplices_and_regions_and_parent)
	(Array.map (fun (_,region,_) -> region) new_simplices_and_regions_and_parent)
    	[||] (* WARNING - the information about periodic points is lost when we create a finer mesh *)
with
      | None -> failwith "Mesh generation failed!"
      | Some m -> m
  in
    (fine_mesh,
     (Array.map (fun (x,_,ix) -> (x, Hashtbl.find ht_new_point_some_parent_simplex ix))
	arr_new_point_by_index),
     (Array.map (fun (_,_,x) -> x)
	new_simplices_and_regions_and_parent)
    )
;;

(* It is nice to have the refinement point specifications auto-generated.
   A refinement is a vector of simplices, where every simplex is a vector of
   points, where every point is a vector of abssites.

   Note: there is an unresolved issue: I just assume that qhull will
   subdivide the surfaces of the simplices in question in an uniform
   way, so that we cannot get any mismatches when linking one
   fine-simplex-inside-a-coarse-simplex to a neighbour. Have not spend
   too much thought on the question whether this really must always
   hold, for any refimenent and in any dimension.

   Indeed, I suppose that it works in every 2d and 3d case, but
   when refining 4d meshes, touching tetrahedra may be subdivided in
   incompatible ways!

*)

let mesh_simplex_refinement dim order =
  let () = (if dim>3 then failwith "mesh_simplex_refinement - bad things may happen in dimensions >3!\n%!" else ()) in 
  let nr_vertices=1+dim in
  let abssites_not_gcd_reduced = all_distributions_to_buckets nr_vertices order in
  let extended_folded_out_abssites =
    let c = 1.0/.(float_of_int order) in
      (Array.append [|Array.make (1+dim) 0.0|]
	 (Array.map (Array.map (fun n -> (float_of_int n)*.c)) abssites_not_gcd_reduced))
  in
  let cells = Qhull.delaunay extended_folded_out_abssites in
    (* All these cells will contain the origin, which is the point with index 0.
       If we remove it, we get the surfaces.
    *)
    Array.map
      (fun cell ->
	 array_mapfilter
	   (fun n -> if n=0 then None else Some abssites_not_gcd_reduced.(n-1))
	   cell
      )
      cells
;;

let make_coarse_fine_mwe
    ?(petsc_name=gensym "mx_coarse_fine")
    (name_coarse,name_fine)
    fun_simplex_to_element
    (mesh_coarse,mesh_fine) 
    fine_coarse_simplex_info
    =
  let dim = mesh_coarse.mm_dim in
  let mwe_coarse = make_mwe name_coarse fun_simplex_to_element mesh_coarse
  and mwe_fine = make_mwe name_fine fun_simplex_to_element mesh_fine
  in
  let simplices_coarse = mwe_coarse.mwe_mesh.mm_simplices
  and simplices_fine = mwe_fine.mwe_mesh.mm_simplices
  in
    (* What we actually need for the coarse/fine mapping functions is the overlap between
       coarse-mesh shape functions and fine-mesh shape functions, as evaluated
       on the fine mesh.

       So, for every DOF in the fine mesh, we have to go through all its simplices,
       and for every simplex, we have to find the corresponding coarse simplex,

       then we have to take the coarse mesh femfuns for all the sites of that
       coarse simplex and evaluate them on the sites of the fine simplex.

       This gives us the coefficients with which we then multiply the integrals of the 
       femfun_fine*femfun_fine shape function products.

       Once we have these data, we know how to "measure" a coarse-mesh function
       on the fine mesh, hence map coarse_field -> fine_cofield. That is good enough
       for a start.

       How about the other direction? This corresponds to integrating a fine_field over
       the coarse shape functions. To me it seems as if the transpose of the previous
       matrix could do this. And that is reasonable:

       <coarse | M | fine> = <fine | M^T | coarse>
    *)
  let coarse_fine_coeffs =
      Mpi_petsc.matrix_create
	(Array.length mwe_coarse.mwe_dofs)
	(Array.length mwe_fine.mwe_dofs) 
	petsc_name
  in
  let inc_matrix_element = Mpi_petsc.matrix_inc_fast_no_safety_belt coarse_fine_coeffs
  in
  let foreach_do x f = Array.iter f x in
  let foreach_do_n x f = Array.iteri f x in
  let () = foreach_do_n mwe_fine.mwe_dof_nrs_by_simplex_index
    (fun fine_sx_ix fine_dof_nrs ->
       let coarse_sx_ix = fine_coarse_simplex_info.(fine_sx_ix) in
       let coarse_sx = mwe_coarse.mwe_mesh.mm_simplices.(coarse_sx_ix) in
       let fine_sx = mwe_fine.mwe_mesh.mm_simplices.(fine_sx_ix) in
       let coarse_sx_mx_x_to_L =
	 match coarse_sx.ms_inv_ext_point_coords with
	   | None -> impossible()
	   | Some x -> x
       in
       let coarse_dof_nrs = mwe_coarse.mwe_dof_nrs_by_simplex_index.(coarse_sx_ix) in
       let ht_coarse_dofs_by_name = Hashtbl.create 10 in
       let ht_fine_dofs_by_name = Hashtbl.create 10 in
       let () = foreach_do fine_dof_nrs
	 (fun fine_dof_nr -> 
	    let fine_dof = mwe_fine.mwe_dofs.(fine_dof_nr) in
	    let dof_name = the_dof_name mwe_fine fine_dof in
	    let fine_dof_el_ix =
	      let (sx,nel,ix) =
		dof_sx_nel_ix_find simplices_fine fine_dof
		  (fun sx nel ix -> sx.ms_id=fine_sx_ix)
	      in
	      let elem = mwe_fine.mwe_elements.(nel) in
		(elem,ix)
	    in
	      hashtbl_push ht_fine_dofs_by_name dof_name (fine_dof,fine_dof_el_ix))
       in
	 (* Actually, this is practically the same code as above -
	    should introduce a function to do this uniformly.
	    Note: we are presumably doing way too much work building
	    the coarse hash table over and over again, whenever we do
	    a fine simplex. But that is not so much an issue here...
	 *)
       let () = foreach_do coarse_dof_nrs
	 (fun coarse_dof_nr -> 
	    let coarse_dof = mwe_coarse.mwe_dofs.(coarse_dof_nr) in
	    let dof_name = the_dof_name mwe_coarse coarse_dof in
	    let coarse_dof_el_ix =
	      let (_,nel,ix) as coarse_dof_nel_ix =
		dof_sx_nel_ix_find simplices_coarse coarse_dof
		  (fun sx nel ix -> sx.ms_id=coarse_sx_ix)
	      in
	      let elem = mwe_coarse.mwe_elements.(nel) in
		(elem,ix)
	    in
	      hashtbl_push ht_coarse_dofs_by_name dof_name (coarse_dof,coarse_dof_el_ix))
       in
	 (* Now that we have all the dof_names and for every dof_name 
	    know the corresponding fine dofs as well as coarse dofs that are relevant,
	    we can proceed to compute matrix elements. For this, we have to know
	    for every coarse dof the value of its femfun at the sites used by the
	    fine dofs.
	 *)
       let all_dof_names = hashtbl_keys ht_coarse_dofs_by_name in
	 foreach_do all_dof_names
	   (fun dof_name ->
	      let coarse_dofs_and_el_ix =
		Array.of_list (Hashtbl.find ht_coarse_dofs_by_name dof_name)
	      and fine_dofs_and_el_ix =
		Array.of_list (Hashtbl.find ht_fine_dofs_by_name dof_name)
	      in
		(* We have to determine the values of the coarse dof's femfuns at the sites
		   where the fine dofs are located. These then are coefficients
		   for the fine_dof femfun inner products.
		   
		   Actually, we would have to integrate a product of the
		   coarse and fine dof femfuns over the fine simplex.
		   Here, we just assume that these femfuns are structurally the same,
		   and we may just as well replace the coarse-femfun-restricted-to-the-fine-simplex
		   by sampling at all fine-simplex sites and summing the corresponding fine-simplex
		   femfuns.
		*)
	      let coarse_L_coords_of_fine_sites =
		Array.map
		  (fun (fine_dof,(fine_dof_el,fine_dof_ix)) ->
		     let fine_dof_pos = Array.init (1+dim)
		       (fun n -> if n < dim then fine_dof.dof_pos.(n) else 1.0)
		     in
		       mx_x_vec coarse_sx_mx_x_to_L fine_dof_pos)
		  fine_dofs_and_el_ix
	      in
	      (* let () = (if coarse_sx_ix <> 0 then () else Printf.printf "coarse_L_coords_of_fine_sites: %s\n" (String.concat " : " (Array.to_list (Array.map float_array_to_string coarse_L_coords_of_fine_sites)))) in (* This indeed shows that the L-coordinate mapping works! *) *)
	      let coarse_femfuns_values_at_fine_sites =
		Array.map
		  (fun (coarse_dof,(coarse_dof_el,coarse_dof_ix)) ->
		     let coarse_femfun = coarse_dof_el.el_dof_funs.(coarse_dof_ix) in
		       Array.map (femfun_eval coarse_sx_mx_x_to_L coarse_femfun)
			 coarse_L_coords_of_fine_sites)
		  coarse_dofs_and_el_ix
	      in
		foreach_do_n coarse_femfuns_values_at_fine_sites
		  (fun nr_row row ->
		     foreach_do_n row
		       (fun nr_col coeff ->
			  let ix_left_coarse =
			    let (d,_) = coarse_dofs_and_el_ix.(nr_row) in d.dof_nr
			  in
			    (* We now have a coarse femfun and its value at a site
			       corresponding to a fine femfun. We now have to
			       integrate the product of this with all the other
			       fine femfuns...
			    *)
			  let femfun_left = 
			    let (_,(el,ix)) = fine_dofs_and_el_ix.(nr_col) in
			      el.el_dof_funs.(ix)
			  in
			    foreach_do fine_dofs_and_el_ix
			      (fun (fine_dof_right,(fine_el_right,fine_ix_right)) ->
				 let femfun_right = fine_el_right.el_dof_funs.(fine_ix_right)
				 in
				 let contrib = coeff*.(femfun_integrate_over_simplex
							 (femfun_mult
							    femfun_left femfun_right)
							 fine_sx)
				 in
				   inc_matrix_element 
				     ix_left_coarse fine_dof_right.dof_nr
				     contrib)))))
  in
  let () = Mpi_petsc.matrix_assemble coarse_fine_coeffs true in
  let fun_fine_field_to_coarse_cofield
      ?(petsc_name="v_coarse_from_fine")
      ?target
      (FEM_field (mwe_f,_,v_data) as field) =
    let () = ensure_field_is_unrestricted field in
    if mwe_f != mwe_fine
    then failwith "MWE mismatch!"
    else
      let v_target =
	match target with
	  | None -> 
	      let v = 
		Mpi_petsc.vector_create 
		  (Array.length mwe_coarse.mwe_dofs)
		  petsc_name
	      in let () = Mpi_petsc.vector_assemble v in
		   v
	  | Some (FEM_cofield (mwe_t,_,v) as cofield) ->
	      let () = ensure_cofield_is_unrestricted cofield in
	      if mwe_t != mwe_coarse
	      then failwith "coarse field -> fine field: wrong target mwe!" 
	      else v
      in
      let () = Mpi_petsc.matrix_times_vector coarse_fine_coeffs v_data v_target in
	FEM_cofield (mwe_coarse,None,v_target)
  in
  let fun_coarse_field_to_fine_cofield
      ?(petsc_name="v_fine_from_coarse")
      ?target
      (FEM_field (mwe_c,_,v_data) as field) =
    let () = ensure_field_is_unrestricted field in
    if mwe_c != mwe_coarse
    then failwith "MWE mismatch!"
    else
      let v_target =
	match target with
	  | None -> 
	      let v = 
		Mpi_petsc.vector_create 
		  (Array.length mwe_fine.mwe_dofs)
		  petsc_name
	      in let () = Mpi_petsc.vector_assemble v in
		   v
	  | Some (FEM_cofield (mwe_t,_,v) as cofield) ->
	      let () = ensure_cofield_is_unrestricted cofield in
	      if mwe_t != mwe_fine 
	      then failwith "coarse field -> fine field: wrong target mwe!" 
	      else v
      in
      let () = Mpi_petsc.matrix_transpose_times_vector coarse_fine_coeffs v_data v_target in
	FEM_cofield (mwe_fine,None,v_target)

  in
    (mwe_coarse,mwe_fine,fun_fine_field_to_coarse_cofield,fun_coarse_field_to_fine_cofield)
;;

(* ========================================================== *)

(* The functionality below is the essence of what previously has been
   the timestep.ml module.

   What we want to do is to do some manipulations on a bunch of fields
   site-wise, i.e. we take a float fem_field array of fields with
   disjoint dof names, as well as a bunch of C code, and execute that
   C code site-wise in such a way that it can read and write to these
   fields.

*)

(* This is a strictly internal function that is used to do the iteration over all sites! *)

external forall_fem_sites_do:
  Mpi_petsc.vector array ->             (* petsc vectors *)
    (string * (int array) * (float array)) array -> (* site structure: vec-nr + offset + site-position *)
      (float array) ->                    (* array containing time and dt *)
        Fastfields.c_funptr ->            (* user-supplied physics *)
          bool
            = "caml_forall_fem_sites_do";;


(* 
   XXX Note: our old site_wise_execute has been replaced by
   a more flexible mechanism which should also be able 
   to handle distributed execution, and a compatibility
   wrapper. The code documentation does not yet properly 
   respect this major change!

   Note: one may be led to believe that site_wise_execute
   should operate on proper fields only - as the numbers in
   cofields are integrals over certain shape functions.

   However, this is not quite right: we may want to do site-wise
   iteration over cofields in order to perform some integral.
   This is of interest in particular if we want to integrate
   surface jumps over part of a boundary...

   Note that if we do such region-constrained integrals,
   we have to keep parallelization in mind.

   In the end, we will have to specify reductors that accumulate
   the values obtained on individual machines...

   XXX Another conceptual issue: as it stands, site_wise_execute is
   built in such a way that it produces a closure over the fields.

   Should we rather only require mwe arguments and allow the resulting
   function to take any vector of fields, which have signature
   compatible to the mwe vector passed to site_wise_execute?
   While this may seem nice (and has been implemented like this
   earlier on), I strongly suppose it will not be needed in reality.

   How about having multiple pieces of C code operating on the same
   field set? Would also be nice, conceptually, but there is a simple
   hack (dispatching on an aux_arg parameter value) that allows us to
   get the same effect. (If we wanted to do it otherwise, the c_code
   argument should be a string array, and the result a function array.)

   Note: non-strict mwe checks allow us to use an applicator which was
   built to take a field of mwe type X on a field of mwe type Y if
   mwes X and Y are compatible. The main application is to set up a
   multistep timestepper and let the field future_configuration
   appear with the names of the field present_configuration inside the
   C function that computes velocities. All this sounding strange?
   If so, don't worry - you probably do not need that feature anyway.
*)

let _site_wise_buffer_name="__refbuf"
and _site_wise_posbuffer_name="__posbuf"
and _site_wise_position_name="COORDS"
and _site_wise_site_pmid="SITE_PMID"
;;

(* XXX TO GO INTO mesh.ml *)

let mesh_vertex_machine mesh nr_vertex =
  let distrib = mesh.mm_vertex_distribution in
  let rec walk offset nr_machine =
    if nr_vertex < offset then nr_machine
    else walk (offset+distrib.(nr_machine+1)) (1+nr_machine)
  in walk distrib.(0) 0
;;

let site_wise_structure_and_offsets_and_defines
    ?(parallel=false)
    v_field_mwes
    v_cofield_mwes
    aux_arg_names
    =
  let () = loginfo (Printf.sprintf "Assembling site-wise offsets for fields %s" (string_array_to_string (Array.map (fun m -> m.mwe_name) (Array.append v_field_mwes v_cofield_mwes)))) in
  let () = logdebug (Printf.sprintf "site_wise_structure_and_offsets_and_defines #0 nr_field_mwes=%d nr_cofield_mwes=%d" (Array.length v_field_mwes) (Array.length v_cofield_mwes)) in
  let empty = [||] in
    (* For the purpose of everything that we do below,
       we treat the cofields as if they were additional fields,
       so we just brutally wrap up the FEM_cofield data in FEM_field.

       This is a technique which no user of fem.ml should resort to,
       but we may use this hack as we are the designers and hence owners
       of fem.ml - quod licet iovis non licet bovis.
    *)
  let v_mwes = Array.append v_field_mwes v_cofield_mwes in
  let nr_mwes = Array.length v_mwes in
    if nr_mwes=0 then ([||],[||],[])
    else
      let mwe0 = v_mwes.(0) in
      let site_pmid =
	let ht = Hashtbl.create 17 in
	let () =
	  Array.iteri
	    (fun n site -> let pmid = mwe0.mwe_pmids.(n) in Hashtbl.add ht site pmid)
	    mwe0.mwe_sites
	in
	  (fun site -> Hashtbl.find ht site)
      in
      let dim = mwe0.mwe_mesh.mm_dim in
      let all_maxnames_and_field_indices_and_lengths =
	let a =
	  array_join
	    (Array.mapi
	       (fun nr_mwe mwe ->
		  if mwe.mwe_mesh != mwe0.mwe_mesh
		  then failwith "site_wise_execute: incompatible MWEs given!"
		  else
		    Array.map
		      (fun ((dof_name,indices) as dn) ->
			 (dn,nr_mwe,Array.fold_left ( * ) 1 indices))
		      mwe.mwe_subfields)
	       v_mwes)
	in
	let () = Array.sort compare a in
	  a
      in
      let buffer_size =
	Array.fold_left
	  (fun sf (_,_,n) -> sf+n) 0 all_maxnames_and_field_indices_and_lengths
      in
      let str_nr_field_indices =
	let s = String.make buffer_size '\000' in
	let () =
	  let nr_maxnames = Array.length all_maxnames_and_field_indices_and_lengths in
	  let rec fill_str pos_offset nr_maxname_entry =
	    if nr_maxname_entry = nr_maxnames
	    then ()
	    else
	      let (_,nr_field,length) =
		all_maxnames_and_field_indices_and_lengths.(nr_maxname_entry)
	      in
		begin
		  for i=0 to length-1 do
		    s.[i+pos_offset] <- Char.chr nr_field
		  done;
		  fill_str (pos_offset+length) (1+nr_maxname_entry)
		end
	  in fill_str 0 0
	in s
      in
      let () = logdebug (Printf.sprintf "Done str_nr_field_indices") in
      let r_defines = ref [] in
      let (ht_dof_name_to_buffer_offset,ht_sites_posns) =
	let ht_bo = Hashtbl.create 100 in
	let r_offset_count = ref 0 in
	let () =
	  array_foreach_do all_maxnames_and_field_indices_and_lengths
	    (fun (((dof_stem,indices) as dof_name),field_index,length) ->
	       let () = logdebug (Printf.sprintf "Processing dof %s" (dof_name_to_string dof_name)) in
	       let nr_indices = Array.length indices in
	       let weights =
		 ((fun (p,q) -> Array.of_list p)
		    (Array.fold_right
		       (fun range_here (so_far,factor) ->
			  ((factor::so_far),range_here*factor))
		       indices ([],1)))
	       in
	       let c_define_accessor =
		 (dof_stem,
		  (Array.to_list (Array.init nr_indices (fun n -> Printf.sprintf "ix%d" n))),
		  Printf.sprintf
		    "(*%s[%d+%s])"
		    _site_wise_buffer_name
		    (!r_offset_count)
		    (let s=
		       (String.concat "+"
			  (Array.to_list
			     (Array.init nr_indices
				(fun n -> Printf.sprintf "%d*ix%d" weights.(n) n))))
		     in if s="" then "0" else s)
		 )
	       in
	       let c_define_is_present =
		 (Printf.sprintf "have_%s" dof_stem,
		  [],
		  Printf.sprintf "(0!=%s[%d])" _site_wise_buffer_name (!r_offset_count)
                    (* Note: it is an important property that this gives 0 or 1.
		       This way, we can write

		       if(have_M1+have_M2+have_M3 >= 2) ...

		       to find out if we are at any inter-material interface.
		     *)
		 )
	       in
	       let () =
		 r_defines := c_define_accessor::c_define_is_present::(!r_defines)
	       in
		 if nr_indices=0 then
		   begin
		     Hashtbl.replace ht_bo dof_name (!r_offset_count);
		     r_offset_count:= !r_offset_count+1
		   end
		 else
		   multifor indices
		     (fun n v_ix ->
			begin
			  Hashtbl.replace ht_bo
			    (dof_stem,Array.copy v_ix)
			    (!r_offset_count);
			  r_offset_count:= !r_offset_count+1
			end))
	in
	let ht_sp =
	  let ht = Hashtbl.create 100 in
	  let () = array_foreach_do v_mwes
	    (fun mwe ->
	       Hashtbl.iter (fun site dofs ->
			       if Array.length dofs>0 then
				 let pos = dofs.(0).dof_pos in
				 let extended_pos =
				   Array.init (1+dim)
				     (fun n -> if n<dim
				      then pos.(n)
				      else float_of_int (site_pmid site))
				 in
				   Hashtbl.replace ht site extended_pos)
		 mwe.mwe_dofs_by_site)
	  in
	    ht
       	in
	  (ht_bo,ht_sp)
      in
      let () = logdebug (Printf.sprintf "Done buffer offsets and site positions") in
	(* Okay, now we have all the sites and their positions.
	   Next, we need an index buffer for every site.
	   This is string * int array * float array
	           ^              ^         ^
	           |              |         site position in space
	        Nr. petsc vector  vector index
	*)
      let all_sites = hashtbl_keys ~sorter:compare ht_sites_posns in
      let v_site_info =
	Array.map
	  (fun site ->
	     let pos = Hashtbl.find ht_sites_posns site in
	     let v_offsets = Array.make buffer_size (-1) in
	     let () = array_foreach_do v_mwes
	       (fun mwe ->
		  let dofs =
		    try Hashtbl.find mwe.mwe_dofs_by_site site with | Not_found -> empty
		  in
		    array_foreach_do dofs
		      (fun dof ->
			 let dof_buffer_offset =
			   Hashtbl.find ht_dof_name_to_buffer_offset
			     (the_dof_name mwe dof)
			 in
			   v_offsets.(dof_buffer_offset) <- dof.dof_nr))
	     in
	       (str_nr_field_indices,v_offsets,pos))
	  all_sites
      in
      let () = logdebug (Printf.sprintf "Done v_site_info") in
      let field_defines= List.rev !r_defines in
      let nr_aux_args = Array.length aux_arg_names in
      let extra_defines =
	let defs =
	  (_site_wise_position_name,["ix0"],Printf.sprintf "(0+%s[%d+ix0])"
	     (* This little 0+ trick ensures we are not a lvalue,
		hence the user cannot accidentally modify position data.
	     *)
	     _site_wise_posbuffer_name nr_aux_args)
	  ::
	  (_site_wise_site_pmid,[],Printf.sprintf "(0+%s[%d+%d])"
	     (* This little 0+ trick ensures we are not a lvalue,
		hence the user cannot accidentally modify position data.
	     *)
	     _site_wise_posbuffer_name nr_aux_args dim)
	  ::
	    (Array.to_list
               (Array.mapi
		  (fun i arg_name ->
		     (arg_name,[],Printf.sprintf "(%s[%d])" _site_wise_posbuffer_name i))
		  aux_arg_names))
	in
	  List.append defs field_defines
      in
      let par_site_info =
	if not parallel
	then [|v_site_info|]
	else
	  (* we just split v_site_info and adjust offsets... *)
	  let nr_machines = Array.length v_mwes.(0).mwe_distribution in
	  let () = logdebug (Printf.sprintf "nr_machines=%d" nr_machines) in
	  let (slice_lengths,slice_offsets) =
	    let a = Array.make nr_machines 0 in
	    let () =
	      Array.iteri
		(fun nr_site site ->
		   let m = mesh_vertex_machine mwe0.mwe_mesh site.(0) in
		     a.(m) <- 1+a.(m))
		all_sites
	    in 
	    let () = logdebug (Printf.sprintf "site distribution: a=%s" (int_array_to_string a)) in
	    let offsets = Array.make nr_machines 0 in
	      begin
		for i=1 to nr_machines-1 do
		  offsets.(i) <- offsets.(i-1) + a.(i-1)
		done;
		logdebug (Printf.sprintf "site offsets: offsets=%s" (int_array_to_string offsets));
		(a,offsets)
	      end
	  in
	  let mwe_offsets = Array.map
	    (fun mwe ->
	       let distrib = mwe.mwe_distribution in
	       let offsets = Array.make nr_machines 0 in
		 begin
		   for i=1 to nr_machines-1 do
		     offsets.(i) <- offsets.(i-1) + distrib.(i-1)
		   done;
		   logdebug (Printf.sprintf "mwe='%s', distribution=%s, offsets=%s" mwe.mwe_name (int_array_to_string distrib) (int_array_to_string offsets));
		   offsets
		 end)
	    v_mwes
	  in
	    Array.init nr_machines
	      (fun nr_machine ->
		 let sub_slice =
		   let () = logdebug (Printf.sprintf "machine=%d, slice offset=%d, length=%d" nr_machine slice_offsets.(nr_machine) slice_lengths.(nr_machine)) in
		   Array.sub v_site_info slice_offsets.(nr_machine) slice_lengths.(nr_machine)
		 in
		   Array.map
		     (fun (str_nr_field,indices,space_pos) ->
			let modified_indices = 
			  Array.mapi 
			    (fun n ix ->
			       let nr_mwe = Char.code str_nr_field.[n] in
				 ix-mwe_offsets.(nr_mwe).(nr_machine))
			    indices
			in
			  (str_nr_field,modified_indices,space_pos))
		     sub_slice)
      in
      let buffer_dof_names = 
	Array.map (fun (offset,name) -> name)
	  (map_hashtbl_to_array 
	     ~sorter:compare
	     (fun name offset -> (offset,name))
	     ht_dof_name_to_buffer_offset)
      in
	(buffer_dof_names,par_site_info,extra_defines)
;;

(* Note: we originally had a "strict mwe check" preventing us from applying an
   executor to wrong-type vectors. This, however, seems to be too low-level
   a constraint, as it does not interact well with parallelization.

   So, this check has to be performed at a higher level! Same holds
   for the check whether a vector corresponds to a restricted or
   unrestricted field.
*)

let site_wise_executor site_info_this_machine extra_defines c_code =
  let manipulator =
    Fastfields.c_register_field_manipulator
      ~buffer_name:_site_wise_buffer_name
      ~posbuffer_name:_site_wise_posbuffer_name
      ~extra_defines:extra_defines
      c_code
  in
    (fun aux_args v_petsc_fields v_petsc_cofields -> (* aux_args are such as time, dt *)
       let petsc_vectors = Array.append v_petsc_fields v_petsc_cofields in
       let () = Fastfields.do_relink_if_necessary() in
       let result =
	 forall_fem_sites_do petsc_vectors site_info_this_machine aux_args
	   (Fastfields._c_manipulator_funptr manipulator)
       in
	 result)
;;

(* Compatibility variant (new) *)

let site_wise_execute
    ?(strict_mwe_check=true)
    field_mwes
    cofield_mwes
    aux_arg_names c_code
    =
  let (_,par_site_info,extra_defines) =
    site_wise_structure_and_offsets_and_defines field_mwes cofield_mwes aux_arg_names
  in
  let swex_nocheck = 
    let s = site_wise_executor par_site_info.(0) extra_defines c_code in
      fun z f c ->
	s z 
	  (Array.map (fun (FEM_field(_,_,v)) -> v) f)
	  (Array.map (fun (FEM_cofield(_,_,v)) -> v) c)
  in
    if strict_mwe_check then
      fun z v_fields v_cofields ->
	let fail () = failwith "Field/MWE incompatibility encountered!" in
	  if   Array.length v_fields <> Array.length field_mwes
	    || Array.length v_cofields <> Array.length cofield_mwes
	  then fail ()
	  else
	    begin
	      for i=0 to Array.length v_fields-1 do
		let FEM_field(mwe,_,_) = v_fields.(i) in
		  if mwes_are_compatible mwe field_mwes.(i) then () else fail()
	      done;
	      for i=0 to Array.length v_cofields-1 do
		let FEM_cofield(mwe,_,_) = v_cofields.(i) in
		  if mwes_are_compatible mwe field_mwes.(i) then () else fail()
	      done;
	      swex_nocheck z v_fields v_cofields
	    end
    else swex_nocheck
;;


let ccode_for_tensor_norms mwe =
  let subfields = mwe.mwe_subfields in
  let ccode_maxname (stem,max_indices) =
    let rank = Array.length max_indices in
    let cvars = 
      Array.to_list
	(Array.init rank (fun n -> Printf.sprintf "i%d" n))
    in
    let cvar_init =
      Printf.sprintf "int %s;\n" (String.concat ", " cvars)
    in
    let cindexed =
      Printf.sprintf "%s(%s)" stem (String.concat "," cvars)
    in
    let cloop the_body =
      Printf.sprintf "%s\n%s\n%s\n"
	(String.concat ", "
	   (Array.to_list
	      (Array.init rank
		 (fun n -> Printf.sprintf "for(i%d=0;i%d<%d;i%d++){\n" n n max_indices.(n) n))))
	the_body
	(String.make rank '}')
    in
      Printf.sprintf
"if (have_%s) {
 double _sq_norm_here=0.0, _v;
 %s /* loop variables */
 %s /* loop body */
 Norm_L2_%s+=_sq_norm_here;
 if(_sq_norm_here>Norm_max_%s){Norm_max_%s=_sq_norm_here;}
 }

"
 stem
 cvar_init
 (cloop (Printf.sprintf "_v=%s; _sq_norm_here+=_v*_v;" cindexed))
	stem stem stem
  in
    String.concat "" (List.map ccode_maxname (Array.to_list subfields))
;;

let fun_tensor_norms mwe =
  let subfields = mwe.mwe_subfields in
  let varnames_L_max =
    array_join
      (Array.map
	 (fun (n,_) ->
	    [|(Printf.sprintf "Norm_max_%s" n);
	      (Printf.sprintf "Norm_L2_%s" n)|])
	 subfields)
  in
  let varnames_L2 = Array.map (fun (n,_) -> Printf.sprintf "Norm_L2_%s" n) subfields in
  let site_wise_fun =
    let (_,par_site_info,extra_defines) =
      site_wise_structure_and_offsets_and_defines [|mwe|] [||] varnames_L_max 
    in
      site_wise_executor par_site_info.(0) extra_defines (ccode_for_tensor_norms mwe)
  in
  let buffer = Array.make (2*(Array.length subfields)) 0.0 in
    fun field ->
      begin
	for i=0 to Array.length buffer-1 do
	  buffer.(i) <- 0.0;
	done;
	ignore(site_wise_fun buffer [|let FEM_field(_,_,v) = field in v|] [||]);
	Array.mapi
	  (fun n (stem,_) -> (stem,sqrt(buffer.(2*n)),sqrt(buffer.(2*n+1))))
	  subfields
      end
;;
      
(* === NEW DIFFOP->MATRIX CODE === *)

(* This will eventually supersede diffop_prematrix
   and friends - maybe even the whole idea
   of having a separate prematrix...
*)

let mwe_simplex_id_to_elem_dof_nr_to_dof_nr mwe =
  let simplices = mwe.mwe_mesh.mm_simplices in
  let nr_simplices = Array.length simplices in
  let result =
    Array.init nr_simplices
      (fun nr_sx ->
	 let elem = mwe.mwe_elements.(mwe.mwe_nel_by_simplex_index.(nr_sx)) in
	   Array.make (Array.length elem.el_dof_names) 0)
  in
    (* fill this table *)
  let () = array_foreach_do mwe.mwe_dofs 
    (fun dof ->
       dof_sx_nel_ix_iter simplices dof
	 (fun sx nel ix ->
	    result.(sx.ms_id).(ix) <- dof.dof_nr))
  in
    result
;;


let ddiffop_from_string str =
  parse_or_error Ddiffop_lexer.token Ddiffop_parser.parse_ddiffop str
;;

(*
   This is our "swiss army knife" that provides matrix population
  in its most abstract form.

  XXX Q: we perhaps should give wrapped_fun_add_contrib 
   an optional argument whether it should make "zero entries"
   as well, or rather not call that function when the entry is 
   = 0.0.

   Note that ddiffop_vivified will come with the power
   to also automatically create the matrix. 
*)


(* internal 
   XXX Note: this function dramatically changed (simplified) with the introduction of advanced field logic.
   Does it still do the right thing? Check!
*)
let _dof_belongs_to_field mwe dof field =
  let (dof_name,dof_indices) = the_dof_name mwe dof in
    dof_name = field.Ddiffop_parser.f_name
      &&
      dof_indices = field.Ddiffop_parser.f_indices
      &&
      (match field.Ddiffop_parser.f_bspec with
	 | Ddiffop_parser.DLOG_true -> true
	 | dlog -> dof_belongs_to mwe dof dlog)
;;


(* XXX Note: this ad-hoc helper originally was part of ddiffop_vivified,
   but is also needed in mumag.ml, hence was factored out. 
   This explains the somewhat strange convention that passing an empty 
   "fields" array 
*)
let ddiffop_mxdim_index_mapping mwe fields =
  let nr_machines = Array.length mwe.mwe_distribution in
  let distrib = Array.make nr_machines 0 in
  let fun_check =
    if Array.length fields = 0
    then
      fun dof -> true
    else
      fun dof ->
	-1 <> array_position_if (_dof_belongs_to_field mwe dof) fields 0
  in
  let short_to_long =
    array_mapfilter 
      (fun dof -> if fun_check dof then Some dof else None)
      mwe.mwe_dofs
  in
  let long_to_short = Array.make (Array.length mwe.mwe_dofs) (-1) in
  let () =
    Array.iteri
      (fun ix_short dof -> 
	 begin
	   long_to_short.(dof.dof_nr) <- ix_short;
	   let nr_machine = the_dof_machine mwe dof in
	     distrib.(nr_machine) <- distrib.(nr_machine)+1
	 end)
      short_to_long
  in (long_to_short,Array.map (fun dof -> dof.dof_nr) short_to_long,distrib)
;;

type 'matrix vivificator =
    ?fun_make_matrix:(int -> int -> 'matrix) ->
   ?fun_finish_matrix:('matrix -> unit) ->
   ?field_mid:float fem_field ->
   ('matrix option -> int -> int -> float -> unit) -> 'matrix option
;;

let ddiffop_vivified
    ?nr_cpu (* Note: this nr_machine/nr_cpu terminology has to be homogenized! *)
    ddiffop
    ?mwe_mid
    mwe_le
    mwe_ri
    =
  let timing =
    let t0 = Unix.gettimeofday() in
      fun () -> Unix.gettimeofday()-.t0
  in
  let have_mwe_mid = mwe_mid <> None in
  let constantly_1 x = 1.0 in
  let mesh = mwe_le.mwe_mesh in
  let dim = mesh.mm_dim in
  let femfun_one =
    registered_femfun [|{ff_coefficient=1.0;ff_dL_powers=[||]; ff_L_powers=Array.make (1+dim) 0;}|]
  in
  let () =
    (if mesh != mwe_ri.mwe_mesh
     then
       failwith "diffop_vivified: Problem: must provide same mesh!"
	 (* XXX Note: we might have to use a more appropriate exception,
	    if we allow the python interface to go that deep.
	    Should we also check mwe_mid ?
	 *)
     else ())
  in
  let mwe_mid = match mwe_mid with | None -> mwe_ri | Some m -> m in
  (* This hack ensures we do not constantly have to discern between having
     mwe_mid or not for the sake of the type system alone... Simplifies coding.
   *)
  let for_ddiffop_dof_combinations f = Array.iter f ddiffop.Ddiffop_parser.diff_contribs in
  let (ixmap_le,mx_size_le) =
    let (a,b,_) =
      ddiffop_mxdim_index_mapping mwe_le (let (x,_) = ddiffop.Ddiffop_parser.diff_mxdim in x) in
      (a, Array.length b)
  in
  let (ixmap_ri,mx_size_ri) =
    let (a,b,_) =
      ddiffop_mxdim_index_mapping mwe_ri (let (_,x) = ddiffop.Ddiffop_parser.diff_mxdim in x) in
      (a,Array.length b)
  in
  let v_periodicity =
    let p_fields = ddiffop.Ddiffop_parser.diff_periodic in
      Array.mapi
	(fun nr_dof_le dof_le ->
	   if -1=array_position_if (_dof_belongs_to_field mwe_le dof_le) p_fields 0
	   then
	     [|nr_dof_le|]
	   else
	     let pi = mwe_le.mwe_dof_periodic_images.(nr_dof_le) in
	       if Array.length pi = 0 then [|nr_dof_le|] else pi
	) mwe_le.mwe_dofs
  in
  (* let ddd = Printf.printf "v_periodicity: %s\n%!" (int_array2_to_string v_periodicity) in *)
  let nr_vertices=1+dim in
  let simplices = mesh.mm_simplices in
  let nr_simplices = Array.length simplices in
  let simplex_cpu sx =
    let vertices = sx.ms_points in
    let rec lowest_point_index seen pos =
      if pos = Array.length vertices then seen 
      else lowest_point_index (min seen vertices.(pos).mp_id) (1+pos)
    in
    let lpi = lowest_point_index 0 0 in
    let distrib = mesh.mm_vertex_distribution in
      if Array.length distrib=0 then 0
      else
	let rec the_cpu nr_cpu offset =
	  if nr_cpu = Array.length distrib
	  then 
	    let () = Printf.fprintf stderr "Broken mesh vertex distribution!\n%!" in
	      (* ^ make sure this gets printed even when the line below sends us into Nirvana. *)
	      failwith "Broken mesh vertex distribution!"
	  else
	    if lpi < offset then nr_cpu
	    else the_cpu (1+nr_cpu) (offset+distrib.(nr_cpu))
	in the_cpu 0 distrib.(0)
  in
  let iterate_over_simplices_restraining_cpu =
    match nr_cpu with
      | None -> 
	  (fun f ->
	     for i=0 to Array.length simplices-1 do
	       f simplices.(i)
	     done)
      | Some cpu ->
	  (fun f ->
	     for i=0 to Array.length simplices-1 do
	       let sx = simplices.(i) in
		 if simplex_cpu sx <> cpu then ()
		 else f sx
	     done)
  in	  
  let optionally_diff femfun difftype =
    match difftype with
      | Ddiffop_parser.DIFF_none -> femfun
      | Ddiffop_parser.DIFF_vol ix ->
	  femfun_diff_x femfun ix
      | Ddiffop_parser.DIFF_boundary _ -> femfun_zero
  in
    (* When we create the element/element thunks below,
       we also need means to apply them, in particular
       a way to know the mwe_dof_nrs of the dofs of
       an element which was instantiated for a particular
       simplex.
    *)
  let mwe_le_simplex_id_to_elem_dof_nr_to_dof_nr =
    mwe_simplex_id_to_elem_dof_nr_to_dof_nr mwe_le
  in
  let mwe_ri_simplex_id_to_elem_dof_nr_to_dof_nr =
    mwe_simplex_id_to_elem_dof_nr_to_dof_nr mwe_ri
  in
  let mwe_mid_simplex_id_to_elem_dof_nr_to_dof_nr =
    mwe_simplex_id_to_elem_dof_nr_to_dof_nr mwe_mid
  in
  let nr_regions =
    1+(Array.fold_left
	 (fun sf sx -> 
	    let Body_Nr region_here = sx.ms_in_body in
	      max sf region_here)
	 0 simplices)
  in
  let forall_element_dof_nrs_belonging_to_field elem field f =
    let field_name =  field.Ddiffop_parser.f_name in
    let field_indices = field.Ddiffop_parser.f_indices in
      array_foreach_do_n elem.el_dof_names 
	(fun nr_dof (dofname,indices) ->
	   if dofname = field_name && indices = field_indices then f nr_dof else ())
  in
  let region_thunks = Hashtbl.create 10 in
  (* A region thunk maps an array of 
     [|nel_le;nel_ri;nel_mid|]
     (where nel_mid may be -1 if missing) 
     (and nel stands for Number_of_ELement)
     to a (fun sx fun_add_contrib -> ... in ()) function
     that uses fun_add_contrib row col contrib to fill in the simplex
     related matrix contributions.
   *)
  let () = logdebug (Printf.sprintf "Before operator Region Thunk compilation") in
  let () = array_foreach_do simplices
      (fun sx ->
	let nel_le = mwe_le.mwe_nel_by_simplex_index.(sx.ms_id)
	and nel_ri = mwe_ri.mwe_nel_by_simplex_index.(sx.ms_id)
	and nel_mid =
	  if have_mwe_mid
	  then mwe_mid.mwe_nel_by_simplex_index.(sx.ms_id)
	  else -1
	in
	let nel_key = [|nel_le;nel_ri;nel_mid|] in
	if Hashtbl.mem region_thunks nel_key 
	then ()
	else
	  let r_funs_add_contrib_if_region_filter_says_ok = ref [] in
	    (* ^ Here, we collect the pieces of the thunk belonging to nel_key *)
	  let elem_le = mwe_le.mwe_elements.(nel_le)
	  and elem_ri = mwe_ri.mwe_elements.(nel_ri)
	  and elem_mid = 
	    if not have_mwe_mid then empty_element
	    else mwe_mid.mwe_elements.(nel_mid)
	  in
	  let () =
	    array_foreach_do ddiffop.Ddiffop_parser.diff_contribs
	      (fun (fields_lrm,ddiffop_contribs) ->
		 let () = (if Array.length fields_lrm = 3 &&
			     fields_lrm.(2).Ddiffop_parser.f_bspec <> Ddiffop_parser.DLOG_true
			   then failwith "Note: we at present do not allow middle fields to carry region specifiers. This may be changed later on."
			   else ())
		 in
		 forall_element_dof_nrs_belonging_to_field elem_le fields_lrm.(0)
		   (fun nr_edof_le ->
	              let femfun_le = elem_le.el_dof_funs.(nr_edof_le) in
		      forall_element_dof_nrs_belonging_to_field elem_ri fields_lrm.(1)
	              (fun nr_edof_ri ->
	              let femfun_ri = elem_ri.el_dof_funs.(nr_edof_ri) in
			(* Note that mwe_field_mid only enters via another
			   femfun multiplicator, and a DOF number. *)
		      let forall_femfuns_edof_nrs_morally_belonging_to_field_mid el fields_lrm f =
			(* Note that the code below has the effect that if we forget field_mid,
			   then this will always default to "constantly 1" in every tensor
			   component.
			*)
			if have_mwe_mid && Array.length fields_lrm = 3
			then
			  forall_element_dof_nrs_belonging_to_field el fields_lrm.(2)
			    (fun nr_edof_mid ->
			       let femfun_mid = el.el_dof_funs.(nr_edof_mid) in
				 f femfun_mid nr_edof_mid)
			else
			  f femfun_one (-1)
		      in
			forall_femfuns_edof_nrs_morally_belonging_to_field_mid elem_mid  fields_lrm
			  (fun femfun_mid nr_edof_mid ->
			     hashtbl_foreach_do ddiffop_contribs
			       (fun ((diff_le,diff_ri) as diff_le_ri) coeff ->
				  let femfun_total =
				    femfun_mult femfun_mid
				      (femfun_mult
					 (match diff_le with 
					    | Ddiffop_parser.DIFF_none -> femfun_le
					    | Ddiffop_parser.DIFF_vol nr_dx -> femfun_diff_x femfun_le nr_dx
					    | _ -> femfun_le)
					 (match diff_ri with 
					    | Ddiffop_parser.DIFF_none -> femfun_ri
					    | Ddiffop_parser.DIFF_vol nr_dx -> femfun_diff_x femfun_ri nr_dx
					    | _ -> femfun_ri))
				  in
				  let sx_opt_surface_diff_dir = (* None = do full volume integral instead. *)
				    match diff_le_ri with
				      | (Ddiffop_parser.DIFF_boundary _,Ddiffop_parser.DIFF_boundary _) ->
					  failwith "Problem: Differential operator contains boundary/boundary derivative combination. As 'pointwise products of distributions' are not well defined, we cannot handle this case."
				      | (Ddiffop_parser.DIFF_boundary n,_) ->
					  Some n
				      | (_,Ddiffop_parser.DIFF_boundary n) ->
					  Some n
				      | _ -> None
				  in
				  let fun_process_simplex ~fun_entry_field_mid wrapped_fun_add_contrib sx =
				    let nr_dof_le = mwe_le.mwe_dof_nrs_by_simplex_index.(sx.ms_id).(nr_edof_le)
				    and nr_dof_ri = mwe_ri.mwe_dof_nrs_by_simplex_index.(sx.ms_id).(nr_edof_ri)
				    in
				    let dof_le = mwe_le.mwe_dofs.(nr_dof_le)
				    and dof_ri = mwe_ri.mwe_dofs.(nr_dof_ri)
				    in
				    let factor_field_mid = 
				      if have_mwe_mid
				      then fun_entry_field_mid (mwe_mid.mwe_dof_nrs_by_simplex_index.(sx.ms_id).(nr_edof_mid))
				      else 1.0
				    in
				      if not(   dof_belongs_to mwe_le dof_le fields_lrm.(0).Ddiffop_parser.f_bspec
					     && dof_belongs_to mwe_ri dof_ri fields_lrm.(1).Ddiffop_parser.f_bspec)
				      then ()
				      else
					match sx_opt_surface_diff_dir with
					  | None ->
					      wrapped_fun_add_contrib nr_dof_le nr_dof_ri
						(coeff *. factor_field_mid *.(femfun_integrate_over_simplex femfun_total sx))
					  | Some diff_dir ->
					      let disappearing_across =
						match diff_le_ri with
						  | (Ddiffop_parser.DIFF_boundary _,_) ->
						      dof_le.dof_disappears_across
						  | (_,Ddiffop_parser.DIFF_boundary _) ->
						      dof_ri.dof_disappears_across
						  | _ -> impossible()
					      in
						list_foreach_do disappearing_across
						  (fun (sx_jump,nr_face) ->
						     if sx != sx_jump then ()
						     else
						       wrapped_fun_add_contrib nr_dof_le nr_dof_ri
							 (-. coeff *. factor_field_mid *.
							    (* ^ minus because normal is pointing to outside
							       and degree of freedom vanishing towards the 
							       outside, therefore has negative surface 
							       divergence
							    *)
							    (femfun_integrate_over_simplex_face
							       ~bare_integral:true
							       femfun_total
							       sx nr_face)*.
							    (simplex_surface_1form_component sx nr_face diff_dir)
							    (*^ this is "surface area vector" component in the direction
							      we take the derivative. If surface normal and derivative
							      direction are at an angle, this will reduce the effective
							      magnitude of the jump. *)
							 ))
				  in
				    r_funs_add_contrib_if_region_filter_says_ok :=
				      fun_process_simplex :: !r_funs_add_contrib_if_region_filter_says_ok)))))
	  in
	    Hashtbl.add region_thunks nel_key (Array.of_list !r_funs_add_contrib_if_region_filter_says_ok))
  in
  let () = logdebug (Printf.sprintf "After operator Region Thunk compilation") in
  let thunk_key = [|0;0;0|] in
    (* === The vivificator === *)
    fun
      ?fun_make_matrix
      ?fun_finish_matrix (* e.g. PETSc may need MatAssembly() at the end. *)
      ?field_mid
      fun_add_contrib ->
	let timing =
	  let t0 = Unix.gettimeofday() in
	    fun () -> Unix.gettimeofday()-.t0
	in
	let opt_the_matrix = 
	  match fun_make_matrix with
	    | None -> None
	    | Some f -> Some (f mx_size_le mx_size_ri)
	in
	let fun_entry_field_mid =
	  match field_mid with
	    | None -> constantly_1
	    | Some (FEM_field (_,_,v) as field) ->
		let () = ensure_field_is_unrestricted field in
		let data_field_mid = Mpi_petsc.vector_extract v in
		  fun n -> data_field_mid.(n)
	in
	let graph_components_and_then_do f_end ?fun_join_data fun_body =
	  let ht:((int,int array*(int*int*float)) Hashtbl.t) = graph_components ?fun_join_data fun_body in
	    f_end ht
	in
	let diff_gauge_fix = ddiffop.Ddiffop_parser.diff_gauge_fix in
	let lambda_produce_wrapped_fun_add_contrib ~produce =
	  (* 
	     let wrapped_fun_add_contrib = ...
	     
	     Must do:
	     
	     * Periodic BC (later on)
	     * Field index re-mapping for "short matrices"
	     (These two can and presumably should be unified?!)
	     * Proper record keeping for von Neumann BCs / gauge fixing
	     
	     We can handle separately:
	     
	     * "Adding ones on the diagonal".
	  *)
	  if Array.length diff_gauge_fix=0
	  then (* speed optimization - unfortunately this duplicates some code: *)
	    (fun row col coeff ->
	       let v_row = v_periodicity.(row) in
		 (* let ddd = (if Array.length v_row > 1 then Printf.printf "DDD periodic row=%s col=%d\n%!" (int_array_to_string v_row) col else ()) in *)
		 for i=0 to Array.length v_row-1 do
		   let row = v_row.(i) in
		   let mx_row = ixmap_le.(row) in
		   let mx_col = ixmap_ri.(col) in
		     (* let ddd = Printf.printf "DDD mx add %3d %3d: %10.8f\n" mx_row mx_col coeff in *)
		     fun_add_contrib opt_the_matrix mx_row mx_col coeff
		 done
	    )
	  else
	    (fun row col coeff ->
	       let nr_gfix = Array.length diff_gauge_fix in
	       let dof_ri = mwe_ri.mwe_dofs.(col) in
	       let v_row = v_periodicity.(row) in
		 (* let ddd = (if Array.length v_row > 1 then Printf.printf "DDD periodic row=%s col=%d\n%!" (int_array_to_string v_row) col else ()) in *)
		 for i=0 to Array.length v_row-1 do
		   let row = v_row.(i) in
		   let mx_row = ixmap_le.(row) in
		   let mx_col = ixmap_ri.(col) in
		   let () =
		     for i=0 to nr_gfix-1 do
		       let field=diff_gauge_fix.(i) in
			 if _dof_belongs_to_field mwe_ri dof_ri field
			 then
			   produce mx_row mx_col (mx_row,mx_col,coeff)
			 else ()
		     done
		   in
		     fun_add_contrib opt_the_matrix mx_row mx_col coeff
		 done
	    )
	in
	let lambda_produce_wrapped_fun_add_contrib_BUGGY ~produce =
	  (* 
	     let wrapped_fun_add_contrib = ...
	     
	     Must do:
	     
	     * Periodic BC (later on)
	     * Field index re-mapping for "short matrices"
	     (These two can and presumably should be unified?!)
	     * Proper record keeping for von Neumann BCs / gauge fixing
	     
	     We can handle separately:
	     
	     * "Adding ones on the diagonal".
	  *)
	  if Array.length diff_gauge_fix=0
	  then (* speed optimization - unfortunately this duplicates some code: *)
	    (fun row col coeff ->
	       let v_col = v_periodicity.(col) in
		 for i=0 to Array.length v_col-1 do
		   let col = v_col.(i) in
		   let mx_row = ixmap_le.(row) in
		   let mx_col = ixmap_ri.(col) in
		     (* let ddd = Printf.printf "DDD mx add %3d %3d: %10.8f\n" mx_row mx_col coeff in *)
		     fun_add_contrib opt_the_matrix mx_row mx_col coeff
		 done
	    )
	  else
	    (fun row col coeff ->
	       let nr_gfix = Array.length diff_gauge_fix in
	       let dof_ri = mwe_ri.mwe_dofs.(col) in
	       let v_row = v_periodicity.(row) in
		 for i=0 to Array.length v_row-1 do
		   let row = v_row.(i) in
		   let mx_row = ixmap_le.(row) in
		   let mx_col = ixmap_ri.(col) in
		   let () =
		     for i=0 to nr_gfix-1 do
		       let field=diff_gauge_fix.(i) in
			 if _dof_belongs_to_field mwe_ri dof_ri field
			 then
			   produce mx_row mx_col (mx_row,mx_col,coeff)
			 else ()
		     done
		   in
		     fun_add_contrib opt_the_matrix mx_row mx_col coeff
		 done
	    )
	in
	let () =
	  graph_components_and_then_do
	    (* Gauge Fix in the end *)
	    (Hashtbl.iter
	       (fun link_head (link_group,(mx_row_max,mx_col_max,coeff_max)) ->
		  let () = logdebug (Printf.sprintf "GAUGE-FIX: %d %d (%d) %f" mx_row_max mx_row_max mx_col_max coeff_max) in
		    fun_add_contrib opt_the_matrix mx_row_max (* mx_col_max *) mx_row_max (0.5*.coeff_max))) (* XXX Deliberately put this on diagonal! *)
	    (* ^ add 0.5*coeff_max*Phi[j] = 0 to row i to fix gauge
	       (where i=mx_row_max and j=mx_col_max) *)
	    ~fun_join_data:
	    (fun ((_,_,xv) as x) ((_,_,yv) as y) ->
	       if abs_float xv > abs_float yv then x else y)
	    (fun ~produce ->
	       let wrapped_fun_add_contrib = lambda_produce_wrapped_fun_add_contrib ~produce in
		 iterate_over_simplices_restraining_cpu
		   (fun sx ->
		      let nel_le = mwe_le.mwe_nel_by_simplex_index.(sx.ms_id)
		      and nel_ri = mwe_ri.mwe_nel_by_simplex_index.(sx.ms_id)
		      and nel_mid =
			if have_mwe_mid
			then mwe_mid.mwe_nel_by_simplex_index.(sx.ms_id)
			else -1
		      in
			begin
			  thunk_key.(0) <- nel_le;
			  thunk_key.(1) <- nel_ri;
			  thunk_key.(2) <- nel_mid;
			  let thunk = Hashtbl.find region_thunks thunk_key in
			  let len_thunk = Array.length thunk in
			    for i=0 to len_thunk-1 do
			      let f = thunk.(i) in
				f ~fun_entry_field_mid wrapped_fun_add_contrib sx
			    done
			end
		   ))
	in
	let () = 
	  let diag_ones = ddiffop.Ddiffop_parser.diff_diag_ones in
	    if Array.length diag_ones = 0 then () else
	      hashtbl_foreach_do mwe_le.mwe_dofs_by_site
		(fun site dofs_le ->
		   let dofs_ri = try Hashtbl.find mwe_ri.mwe_dofs_by_site site with | Not_found -> [||] in
		     (array_foreach_do diag_ones
			(fun (field_le,field_ri) ->
			   array_foreach_do dofs_le
			     (fun dof_le ->
				if not(_dof_belongs_to_field mwe_le dof_le field_le) then ()
				else 
				  array_foreach_do dofs_ri
				    (fun dof_ri ->
				       if not(_dof_belongs_to_field mwe_ri dof_ri field_ri) then ()
				       else 
					 fun_add_contrib opt_the_matrix dof_le.dof_nr dof_ri.dof_nr 1.0)))))
	in
	let () =
	  match (fun_finish_matrix,opt_the_matrix) with
	    | (Some f, Some m) -> f m
	in
	  opt_the_matrix
;;

(* XXX TODO - dirty ad hoc hack to deal with periodic/nonperiodic fields.
   Good enough for now, but in the longer run we have to improve our
   nsim_grammars and merge periodicity specs with boundary specs!

   -- actually, not so bad at all, but the problem with this variant
   is that sundials uses petsc. Must factor that out! XXX
*)

(* After some toil, I (T.F.) found that the most reasonable approach is 
   to attach the mapping of physical non-redundant cvode-internal dofs
   to "mirage" dofs to the CVODE itself. This sounds maybe somewhat
   strange, but is actually a good idea in many situations.

   So, deep inside a cvode, there will be a "mirage dof mapper". 
   We can make good use of two long and two short buffers here,
   so that we can copy physical_short to buffer1_long and 
   buffer2_long to a new physical_short again.
*)

let make_sundials_mirage_dof_mapping ?(petsc_name=gensym "midof") ?subfield_restriction mwe =
  let pname s = Printf.sprintf "%s_%s" petsc_name s in
  let nr_dofs = Array.length mwe.mwe_dofs in
  let bigarray_create n = Bigarray.Array1.create  Bigarray.float64 Bigarray.c_layout n in
  let is_allowed_dof =
    match subfield_restriction with
      | None -> (fun _ -> true)
      | Some names ->
	  (fun stem -> -1<>array_position stem names 0)
  in
  let pdofs = (* remove periodic shadow copies of dofs *)
    array_filter
      (fun dof ->
	 let p_i = mwe.mwe_dof_periodic_images.(dof.dof_nr) in
	   (Array.length p_i <= 1)
	   ||
	     (let (stem,_) = the_dof_name mwe dof in
		p_i.(0) = dof.dof_nr && is_allowed_dof stem)
      )
      mwe.mwe_dofs
  in
  let nr_pdofs = Array.length pdofs in
    if nr_dofs = nr_pdofs
    then
      None
    else
      let ix_stl = Array.map (fun dof -> dof.dof_nr) pdofs in
      let ix_lts =
	let v = Array.make nr_dofs (-1) in
	let () = Array.iteri (fun ix_s ix_l -> v.(ix_l) <- ix_s) ix_stl in
	  v
      in
      let buffer_long1 = bigarray_create nr_dofs in
      let buffer_long2 = bigarray_create nr_dofs in
      let buffer_short1 = bigarray_create nr_pdofs in
      let buffer_short2 = bigarray_create nr_pdofs in
      let fun_expand ~ba_short ~ba_long () =
	for i=0 to nr_pdofs-1 do
	  let pdof = pdofs.(i) in
	  let dof_nr = pdof.dof_nr in
	  let images = mwe.mwe_dof_periodic_images.(dof_nr) in
	    if Array.length images = 0
	    then
	      ba_long.{dof_nr} <- ba_short.{i}
	    else
	      Array.iter (fun dof_nr -> ba_long.{dof_nr} <- ba_short.{i}) images
	done;
      in
      let fun_reduce ~ba_long ~ba_short () =
	for i=0 to nr_pdofs-1 do
	  let pdof = pdofs.(i) in
	  let dof_nr = pdof.dof_nr in
	  let images = mwe.mwe_dof_periodic_images.(dof_nr) in
	    if Array.length images = 0
	    then
	      ba_short.{i} <- ba_long.{dof_nr}
	    else
	      let avg =
		(Array.fold_left (fun sf ix -> sf+.ba_long.{ix}) 0.0 images)
		/. (float_of_int (Array.length images))
	      in
		ba_short.{i} <- avg
	done;
      in
	Some
	  {Sundials_sp.mdm_expand=fun_expand;
	   Sundials_sp.mdm_reduce=fun_reduce;
	   Sundials_sp.mdm_longbuffer_lts=buffer_long1;
	   Sundials_sp.mdm_longbuffer_stl=buffer_long2;
	   Sundials_sp.mdm_shortbuffer_lts=buffer_short1;
	   Sundials_sp.mdm_shortbuffer_stl=buffer_short2;
	  }
;;
