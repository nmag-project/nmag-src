(*
  (C) 2005 Dr. Thomas Fischbacher

*)

open Qhull;;

let make_random_points n =
  let () = Random.init 77 in
    Array.init n
      (fun _ -> [|Random.float 1.0;Random.float 1.0;Random.float 1.0|])
    ;;

let points = make_random_points 1000;;

let delaunay = Qhull.delaunay points;;

let timing n f =
  let t0 = Unix.gettimeofday () in
    begin
      for i = 0 to n-1 do
	f ()
      done;
      let t1 = Unix.gettimeofday () in
	Printf.printf "%d calls took %f secs. (Avg. %f s/call)\n" n (t1-.t0) ((t1-.t0)/.(float_of_int n))
    end
;;


let time_qhull nr_points nr_runs =
  let points = make_random_points nr_points in
    timing nr_runs (fun _ -> Qhull.delaunay points)
;;
   
(*
# timing 10 (fun () -> Qhull.delaunay points);;
10 calls took 1.110780 secs. (Avg. 0.111078 s/call)
- : unit = ()
# timing 10 (fun () -> Qhull.delaunay points);;
10 calls took 1.107465 secs. (Avg. 0.110747 s/call)
- : unit = ()
# timing 10 (fun () -> Qhull.delaunay points);;
10 calls took 1.110206 secs. (Avg. 0.111021 s/call)
- : unit = ()
# timing 10 (fun () -> Qhull.delaunay points);;
10 calls took 1.115600 secs. (Avg. 0.111560 s/call)
- : unit = ()
# timing 10 (fun () -> Qhull.delaunay points);;
10 calls took 1.106483 secs. (Avg. 0.110648 s/call)
- : unit = ()


# timing 10 (fun () -> Qhull.delaunay_nocheck points);;
10 calls took 1.113590 secs. (Avg. 0.111359 s/call)
- : unit = ()
# timing 10 (fun () -> Qhull.delaunay_nocheck points);;
10 calls took 1.106470 secs. (Avg. 0.110647 s/call)
- : unit = ()
# timing 10 (fun () -> Qhull.delaunay_nocheck points);;
10 calls took 1.109073 secs. (Avg. 0.110907 s/call)
- : unit = ()
# timing 10 (fun () -> Qhull.delaunay_nocheck points);;
10 calls took 1.111047 secs. (Avg. 0.111105 s/call)
- : unit = ()
# 
*)

(*
# for i = 1 to 100 do Printf.printf "%!%d  " (i*100); time_qhull (i*100) 5 done;;
100  5 calls took 0.041786 secs. (Avg. 0.008357 s/call)
200  5 calls took 0.132044 secs. (Avg. 0.026409 s/call)
300  5 calls took 0.238606 secs. (Avg. 0.047721 s/call)
400  5 calls took 0.366058 secs. (Avg. 0.073212 s/call)
500  5 calls took 0.210022 secs. (Avg. 0.042004 s/call)
600  5 calls took 0.272713 secs. (Avg. 0.054543 s/call)
700  5 calls took 0.347424 secs. (Avg. 0.069485 s/call)
800  5 calls took 0.407012 secs. (Avg. 0.081402 s/call)
900  5 calls took 0.477999 secs. (Avg. 0.095600 s/call)
1000  5 calls took 0.554238 secs. (Avg. 0.110848 s/call)
1100  5 calls took 0.615684 secs. (Avg. 0.123137 s/call)
1200  5 calls took 0.685309 secs. (Avg. 0.137062 s/call)
1300  5 calls took 0.754261 secs. (Avg. 0.150852 s/call)
1400  5 calls took 0.830525 secs. (Avg. 0.166105 s/call)
1500  5 calls took 0.922802 secs. (Avg. 0.184560 s/call)
1600  5 calls took 0.992398 secs. (Avg. 0.198480 s/call)
1700  5 calls took 1.088147 secs. (Avg. 0.217629 s/call)
1800  5 calls took 1.130268 secs. (Avg. 0.226054 s/call)
1900  5 calls took 1.220311 secs. (Avg. 0.244062 s/call)
2000  5 calls took 1.267182 secs. (Avg. 0.253436 s/call)
2100  5 calls took 1.355090 secs. (Avg. 0.271018 s/call)
2200  5 calls took 1.428523 secs. (Avg. 0.285705 s/call)
2300  5 calls took 1.514652 secs. (Avg. 0.302930 s/call)
2400  5 calls took 1.593622 secs. (Avg. 0.318724 s/call)
2500  5 calls took 1.653400 secs. (Avg. 0.330680 s/call)
2600  5 calls took 1.742925 secs. (Avg. 0.348585 s/call)
2700  5 calls took 1.821213 secs. (Avg. 0.364243 s/call)
2800  5 calls took 1.896398 secs. (Avg. 0.379280 s/call)
2900  5 calls took 1.997532 secs. (Avg. 0.399506 s/call)
3000  5 calls took 2.062556 secs. (Avg. 0.412511 s/call)
3100  5 calls took 2.171805 secs. (Avg. 0.434361 s/call)
3200  5 calls took 2.247912 secs. (Avg. 0.449582 s/call)
3300  5 calls took 2.330161 secs. (Avg. 0.466032 s/call)
3400  5 calls took 2.405603 secs. (Avg. 0.481121 s/call)
3500  5 calls took 2.495270 secs. (Avg. 0.499054 s/call)
3600  5 calls took 2.517925 secs. (Avg. 0.503585 s/call)
3700  5 calls took 2.615276 secs. (Avg. 0.523055 s/call)
3800  5 calls took 2.697861 secs. (Avg. 0.539572 s/call)
3900  5 calls took 2.778224 secs. (Avg. 0.555645 s/call)
4000  5 calls took 2.869286 secs. (Avg. 0.573857 s/call)
4100  5 calls took 2.926488 secs. (Avg. 0.585298 s/call)
4200  5 calls took 3.061139 secs. (Avg. 0.612228 s/call)
4300  5 calls took 3.096841 secs. (Avg. 0.619368 s/call)
4400  5 calls took 3.166893 secs. (Avg. 0.633379 s/call)
4500  5 calls took 3.316770 secs. (Avg. 0.663354 s/call)
4600  5 calls took 3.399297 secs. (Avg. 0.679859 s/call)
4700  5 calls took 3.415273 secs. (Avg. 0.683055 s/call)
4800  5 calls took 3.508918 secs. (Avg. 0.701784 s/call)
4900  5 calls took 3.579075 secs. (Avg. 0.715815 s/call)
5000  5 calls took 3.707426 secs. (Avg. 0.741485 s/call)
5100  5 calls took 3.771925 secs. (Avg. 0.754385 s/call)
5200  5 calls took 3.851552 secs. (Avg. 0.770310 s/call)
5300  5 calls took 3.887984 secs. (Avg. 0.777597 s/call)
5400  5 calls took 3.981823 secs. (Avg. 0.796365 s/call)
5500  5 calls took 4.084733 secs. (Avg. 0.816947 s/call)
5600  5 calls took 4.150311 secs. (Avg. 0.830062 s/call)
5700  5 calls took 4.230781 secs. (Avg. 0.846156 s/call)
5800  5 calls took 4.379907 secs. (Avg. 0.875981 s/call)
5900  5 calls took 4.477257 secs. (Avg. 0.895451 s/call)
6000  5 calls took 4.527730 secs. (Avg. 0.905546 s/call)
6100  5 calls took 4.582802 secs. (Avg. 0.916560 s/call)
6200  5 calls took 4.677191 secs. (Avg. 0.935438 s/call)
6300  5 calls took 4.741981 secs. (Avg. 0.948396 s/call)
6400  5 calls took 4.824778 secs. (Avg. 0.964956 s/call)
6500  5 calls took 4.916822 secs. (Avg. 0.983364 s/call)
6600  5 calls took 4.973748 secs. (Avg. 0.994750 s/call)
6700  5 calls took 5.092910 secs. (Avg. 1.018582 s/call)
6800  5 calls took 5.193419 secs. (Avg. 1.038684 s/call)
6900  5 calls took 5.205387 secs. (Avg. 1.041077 s/call)
7000  5 calls took 5.333302 secs. (Avg. 1.066660 s/call)

Looks pretty much like 1.7e-5*x*log(x).

Also notice that experiments furthermore indicate that we do not have
a memory leak.


TODO NEXT:

 - Meshing Module

 - Find out how to place individual modules into individual directories with OCamlMakefile

 - Brush up to make all this a C library

 - Python interface to this

 - Python Visualization

*)
