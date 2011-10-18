

#use "topfind";;
#require "Qhull";;


let make_random_points n =
  let () = Random.init 42 in
    Array.init n
      (fun _ -> [|Random.float 1.0;Random.float 1.0|])
    ;;

let points = make_random_points 10;;

let print_points p =
  let print_point point = 
    begin
      Printf.printf "[";
      Array.iter (fun b -> Printf.printf "%f " b) point;
      Printf.printf "]\n";
    end
    in
    Array.iter (print_point) p 
;;


print_points points;;


let simplicies = Qhull.delaunay points;;

let print_simplicies s =
  let print_simplex simplex = 
    begin
      Printf.printf "[";
      Array.iter (fun b -> Printf.printf "%d " b) simplex;
      Printf.printf "]\n";
    end
    in
    Array.iter (print_simplex) s 
;;

print_simplicies simplicies;;


    



