
#use "topfind";;
#require "fem";;

open Snippets;;
open Mesh;;
open Fem;;

#install_printer Mesh.print_point;;
#install_printer Mesh.print_simplex;;
#install_printer Mesh.print_mesh;;


let the_mesh =
  (*

   A           B             C          
                                        
                                        
          D           E                   
                                        
               G                         
      F                  H              
                                        
                                        
               I                        
                                        
    J                        K          
  *)
  mesh_from_known_delaunay
    [| [|0.0;0.0|];
       [|2.0;0.0|];
       [|4.0;0.0|];
       [|1.0;1.0|];
       [|3.0;1.0|];
       [|0.5;1.4|];
       [|2.0;1.2|];
       [|3.5;1.4|];
       [|2.0;3.0|];
       [|0.0;4.0|];
       [|4.0;4.0|];
    |]
    [| (Body_Nr 0,[|0;1;3|]);
       (Body_Nr 0,[|1;2;4|]);
       (Body_Nr 0,[|0;3;5|]);
       (Body_Nr 0,[|1;3;4|]);
       (Body_Nr 0,[|2;4;7|]);
       (Body_Nr 0,[|0;5;9|]);
       (Body_Nr 0,[|2;7;10|]);
       (Body_Nr 0,[|8;9;10|]);
       (Body_Nr 0,[|5;8;9|]);
       (Body_Nr 0,[|7;8;10|]);
       (Body_Nr 2,[|6;5;3|]);
       (Body_Nr 2,[|6;3;4|]);
       (Body_Nr 2,[|6;4;7|]);
       (Body_Nr 1,[|6;7;8|]);
       (Body_Nr 1,[|6;8;5|]);
    |]
;;

let () = mesh2d_ps the_mesh "/tmp/mesh.ps";;

let elem_M1 = make_element 2 ("M1",[|3|]) 1;;
let elem_M2 = make_element 2 ("M2",[|3|]) 1;;
let elem_scalar = make_element 2 ("S",[||]) 1;;

let elem_M12 = fuse_elements elem_M1 elem_M2;;

let mwe_M = make_mwe "M" (fun sx -> match sx.ms_in_body with
			    | Body_Nr 0 -> empty_element
			    | Body_Nr 1 -> elem_M1
			    | Body_Nr 2 -> elem_M12)
  the_mesh
;;

let mwe_scalar = make_mwe "scalar" (fun sx -> elem_scalar) the_mesh;;

let mwe_rho = mwe_sibling "rho" "rho/S" [|("S","rho")|] mwe_scalar;;
let mwe_phi = mwe_sibling "phi" "phi/S" [|("S","phi")|] mwe_scalar;;

let ddiffop_vivified_from_string s = ddiffop_vivified (ddiffop_from_string s);;


let s1 = "0.8*<rho||d/dxj M1(j)>+0.6*<rho||d/dxj M2(j)>; j:3";;
let lexbuf1 = Lexing.from_string s1 (* "0.8*<rho||D/Dxj M1(j)>+0.6*<rho||D/Dxj M2(j)>; (L||R)=(rho[boundary]=0/1||*), j:3";; *)

let token_stream eof_token lexer lexbuf  =
  let rec walk have =
    let next = lexer lexbuf in
      if next = eof_token then Array.of_list (List.rev have)
      else walk (next::have)
  in walk [];;

let tok1 = token_stream Ddiffop_parser.EOF Ddiffop_lexer.token lexbuf1;;

let dstring_tokens s = token_stream Ddiffop_parser.EOF Ddiffop_lexer.token (Lexing.from_string s);;

let viv_rho_bulk = ddiffop_vivified_from_string "0.8*<rho||d/dxj M1(j)>+0.6*<rho||d/dxj M2(j)>, j:3";;

let simple_vivify_to_ml_matrix ?(debug=false) viv =
  fun mwe_le mwe_ri ->
    let mx = Array.make_matrix (Array.length mwe_le.mwe_dofs) (Array.length mwe_ri.mwe_dofs) 0.0 in
    let fun_add_contrib row col x =
      let () = (if debug then Printf.printf "ADD %3d %3d: %8.4f\n" row col x else ()) in
	mx.(row).(col) <- mx.(row).(col)+.x
    in
    let () = viv ?mwe_mid:None mwe_le mwe_ri ?field_mid:None fun_add_contrib in
      mx
;;

let mx_dims mx = (Array.length mx,Array.length mx.(0));;

(* Simplest case: *)

let dstring_to_ml_matrix ?debug s = simple_vivify_to_ml_matrix ?debug (ddiffop_vivified_from_string s);;

let mx_rho_bulk = dstring_to_ml_matrix ~debug:true "0.8*<rho||d/dxj M1(j)>+0.6*<rho||d/dxj M2(j)>, j:3" mwe_rho mwe_M;;

let mx_rho_surface = dstring_to_ml_matrix ~debug:true "0.8*<rho||D/Dxj M1(j)>+0.6*<rho||D/Dxj M2(j)>, j:3" mwe_rho mwe_M;;

let mx_rho_surface_2 = dstring_to_ml_matrix ~debug:true "0.8*<rho||D/Dxj M1(j)>+0.6*<rho||D/Dxj M2(j)>; (L||R)=(rho[boundary]||*), j:3" mwe_rho mwe_M;;

let mx_rho_surface_3 = dstring_to_ml_matrix ~debug:true "0.8*<rho[boundary=0/*]||D/Dxj M1(j)>+0.6*<rho[boundary=0/*]||D/Dxj M2(j)>; (L||R)=(rho[boundary=0/*]||*), j:3" mwe_rho mwe_M;;

let mx_rho_surface_4 = dstring_to_ml_matrix ~debug:true "0.8*<rho[boundary=0/1]||D/Dxj M1(j)>; (L||R)=(rho[boundary=0/1]||*), j:3" mwe_rho mwe_M;;

(*
# mx_dims mx_rho_surface;;
- : int * int = (11, 33)
# mx_dims mx_rho_surface_2;;
- : int * int = (11, 33)
# mx_dims mx_rho_surface_3;;
- : int * int = (11, 33)
# mx_dims mx_rho_surface_4;;
- : int * int = (11, 33)

This maybe looks a bit wrong, but actually, the matrix size was pre-determined from the number of DOFs,
so the question rather is: "Do we populate just a sub-matrix here?"

Note: this looks quite a bit suspicious:

let mx_rho_surface_3 = dstring_to_ml_matrix ~debug:true "0.8*<rho[boundary=0/*]||D/Dxj M1(j)>+0.6*<rho[boundary=0/*]||D/Dxj M2(j)>; (L||R)=(rho[boundary=0/*]||*), j:3" mwe_rho mwe_M;;

...but actually, it seems to be right, compare with:

let mx_rho_surface_3 = dstring_to_ml_matrix ~debug:true "1000.8*<rho[boundary=0/*]||D/Dxj M1(j)>+0.6*<rho[boundary=0/*]||D/Dxj M2(j)>; (L||R)=(rho[boundary=0/1,0/2]||*), j:3" mwe_rho mwe_M;;

Note that the outermost rho dofs also are on a boundary (0/-1)!

So this may indeed be okay. But it shows us to be very careful with such issues...

*)


dstring_tokens "0.8*<rho||D/Dxj M1(j)>+0.6*<rho||D/Dxj M2(j)>; (L||R)=(rho[boundary=0/1,0/2]||*), j:3";;
dstring_tokens "0.8*<rho||D/Dxj M1(j)>+0.6*<rho||D/Dxj M2(j)>; (L||R)=(rho[boundary=0/*]||*), j:3";;

(*
BUG:

# mx_dims mx_rho_surface;;
- : int * int = (11, 33)

# mx_dims mx_rho_surface_2;;
- : int * int = (11, 33)
*)
