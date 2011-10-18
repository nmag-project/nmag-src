
let ctx = Mt19937.make 97;;

let nr = 1000000 in
let t0= Unix.gettimeofday () in
  for i=0 to nr-1 do
    ignore(Mt19937.int ctx 1024)
  done;
  let t1= Unix.gettimeofday () in
    Printf.printf "Time for %d random ints: %f\n" nr (t1-.t0)
;;

(* 0.213 seconds *)

let nr = 1000000 in
let t0= Unix.gettimeofday () in
  for i=0 to nr-1 do
    ignore(Random.int 1024)
  done;
  let t1= Unix.gettimeofday () in
    Printf.printf "Time for %d random ints: %f\n" nr (t1-.t0)
;;

(* 0.893 seconds *)

