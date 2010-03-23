#use "topfind";;
#require "Snippets";;

let b =[|"dog";"cat";"mouse";"elephant";"tiger"|] in 
  let a=[|1;3;5;7;11|] in 
  Snippets.array_map2 (fun a b -> (a,b)) a b;;

(* Output from command prompt is 

# - (int * string) array =
[|(1, "dog"); (3, "cat"); (5, "mouse"); (7, "elephant"); (11, "tiger")||]

*)

