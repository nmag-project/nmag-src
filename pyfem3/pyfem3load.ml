(* (C) 2005 Dr. Thomas Fischbacher 

   Just a small wrapper to pyfem3.ml into an interactive toplevel...
 *)


#use "topfind";;
#require "snippets";;
#require "qhull";;
#require "mt19937";;
#require "nlog";;
#require "gsl";;
#require "mesh";;
#require "pycaml";;
#require "fem";;
#require "mumag2";;

#use "pyfem3.ml";;

(* let readmesh = 
  let some_mesh = Mesh.read_mesh "/tmp/ellipsoid_array3d.nmesh" in
    match some_mesh with
      | Some mesh -> mesh
      | _ -> failwith "Reading mesh failed!";;


let m = readmesh;;

let b= Mesh.mesh_plotinfo_surfaces_and_surfacesregions m;;
*)
