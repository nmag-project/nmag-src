(* 
   (C) 2005 Dr. Thomas Fischbacher
*)

(** Mersenne Twister Random Number Generator *)

(**
{2 The Problem}

   XXX Note: does qhull contain an own, independent RNG? That would be a problem!

   Good scientific practice demands that all simulations be fully
   reproducible, hence we should not only publish results, but also
   the code with which they were obtained, plus the random number
   generator seeds. (Furthermore, it helps a lot for debugging if it
   is always possible to reproduce any spooky situation that came up
   by chance.)

   Evidently, there is the danger that by switching from one version
   of a compiler to the next, the implementation of random number
   generators changes.

   Hence, we use {e one} random number generator for all our simulations,
   which is furthermore known to have good properties: the MT19937
   mersenne twister.

{3 A Problem within this problem}
   
   When program flow depends on the random numbers which we generate
   in not too simple ways (like doing simulations of collisions on 
   a pool billiard table until only a single ball is left), one
   particularly ufly problem is that even with the very same source
   on the very same hardware, results of a run may depend on the
   version of the compiler. One reason for this is that many FPUs
   support a series of different floatingpoint rounding modes,
   and compilers sometimes consider it as as up to their discretion
   when to switch into what mode and when not to switch back.

   One has to be aware of such behaviour. There hardly is a solution
   to this (short of doing all calculations in integer arithmetics),
   besides clearly specifying the total hardware and software
   environment (= code, processor, kernel, libraries, compiler).
   Reconstruction of identical situations is simplifyed with the
   Debian GNU/Linux distribution via the package snapshot archive at
   snapshot.debian.net.

*)

val version : unit -> string
(** This function returns an unique code version string *)

(** {2 Data types} *)

(** A rng contains all the internal state of a random number generator.
    
    In order to have reproducible results, modifications of this state
    must occur in a predictable fashion, i.e. in a multi-threaded program,
    every thread should use its own random number generator.
*)
type rng

(** {2 Functions} *)

(** Create a new random number generator from a seed *)
val make: int -> rng

(** Re-initialize a random number generator with a given seed *)
val init: rng -> int -> unit


(** Generate a floatingpoint random number in the range
    0 (inclusive) .. {i max} (exclusive)
 *)
val float: rng -> float -> float


(** Fill an array of floats with random numbers in the range
    0 (inclusive) .. {i max} (exclusive).
    One reason to have this function is to alleviate the problem
    of number consing: Whenever Mt19937.float is used to generate
    a number, the result is stack allocated. This can lead to
    undesirable GC activity.
*)
val float_fillarray: rng -> float -> float array -> unit

(** Generate an integer random number in the range
    0 (inclusive) .. {i max} (exclusive).

    Note: {i max} must be smaller than 2^30-1 (even on 64-bit machines,
    as this library's behaviour has to be the same on all architectures
    for the sake of reproducibility.

    Note: when {i max} is of the order of magnitude of the maximum allowed
    value, the distribution will become somewhat uneven, as we just
    take the max-modulus of a 30-bit number to generate our random
    numbers.
*)
val int: rng -> int -> int

(** {2 Future Improvements} *)

(** It may be desirable to have means to serialize the state of the RNG
    to a string and re-read it. This is not supported yet.
*)

(** {2 Speed test - comparison to OCaml's built-in RNG} *)

(** 

The following test has been performed on a 2533 MHz Pentium-IV stepping-7 CPU
with the OCaml 3.08.3 bytecode system from the Debian package.

{[
let ctx = Mt19937.make 97;;

let nr = 1000000 in
let t0= Unix.gettimeofday () in
  for i=0 to nr-1 do
    ignore(Mt19937.int ctx 1024)
  done;
  let t1= Unix.gettimeofday () in
    Printf.printf "Time for %d random ints: %f\n" nr (t1-.t0)
;;

(* Average: 0.213 seconds *)

let nr = 1000000 in
let t0= Unix.gettimeofday () in
  for i=0 to nr-1 do
    ignore(Random.int 1024)
  done;
  let t1= Unix.gettimeofday () in
    Printf.printf "Time for %d random ints: %f\n" nr (t1-.t0)
;;

(* Average: 0.893 seconds *)
]}
*)
