#use "topfind";;
#require "fastfields";;

open Bigarray;;

at_exit Fastfields.finish;;

let () = Fastfields.register_include ~library_file:true "math.h";; 

(* Plotting a function using an intermediate bigarray to hold values *)

let plot_ascii_2d (x0,y0) (x1,y1) c_string =
  let (x_max,y_max)= (60,40) in
  let chars = " .,oO0@#" in
  let minmax the_min the_max x = max the_min (min the_max x) in
  let the_char x =
    chars.[minmax 0 ((String.length chars)-1)
	     (int_of_float (x*.(float_of_int (String.length chars))))]
  in
  let c_fun = Fastfields.c_register_field c_string in
  let pos = Array1.create float64 c_layout 2 in
  let result = Array1.create float64 c_layout 1 in
  let f = Fastfields.c_field_evaluator_modifying_bigarray c_fun in
  let dx = (x1-.x0)/. (float_of_int (x_max-1)) in
  let dy = (y1-.y0)/. (float_of_int (y_max-1)) in
  for y = 0 to y_max-1 do
    begin
      pos.{1} <- y1-.(float_of_int y)*.dy;
      for x=0 to x_max-1 do
        pos.{0} <- x0+.(float_of_int x)*.dx;
	begin
	  f pos result;
	  Printf.printf "%c%!" (the_char result.{0}) (* Flushing is for debugging! *)
	end;
      done;
      Printf.printf "\n%!";
    end;
  done
;;
    
let () =
  plot_ascii_2d (-1.5,-1.5) (1.5,1.5)
"*result = position[0]*position[0]+position[1]*position[1];
if(0)
 fprintf(stderr,\"POS: %8.4f %8.4f VAL: %f\\n\",
                position[0],position[1],*result);
"
;;

