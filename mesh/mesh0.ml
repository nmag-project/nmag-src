(* Nmag micromagnetic simulator
 * Copyright (C) 2010 University of Southampton
 * Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
 *
 * WEB:     http://nmag.soton.ac.uk
 * CONTACT: nmag@soton.ac.uk
 *
 * AUTHOR(S) OF THIS FILE: Matteo Franchin
 * LICENSE: GNU General Public License 2.0
 *          (see <http://www.gnu.org/licenses/>)
 *
 * This module defines a type which stores just the minimal amount of
 * information necessary to identify univocally a mesh (points, simplices
 * and - optionally - surfaces).
 *)

open Base.Ba

type t =
  { m0_dim: int;
    m0_points: F.array2;
    m0_simplices: I32.array2;
    m0_regions: I32.array1;}

let dummy =
  { m0_dim = 2;
    m0_points = F.create2 0 2;
    m0_simplices = I32.create2 0 3;
    m0_regions = I32.create1 0; }

let init_from_arrays point_array region_and_simplex_array =
  let d =
    if Array.length point_array >= 1
    then Array.length point_array.(0)
    else failwith "init_from_arrays: cannot handle empty mesh."
  in
  let n = Array.length region_and_simplex_array in
  let simplices = I32.create2 n (d + 1) in
  let regions = I32.create1 n in
  let () =
    Array.iteri
      (fun nr_spx (region, simplex) ->
         begin
           I32.set1 regions nr_spx region;
           Array.iteri
             (fun i v -> I32.set2 simplices nr_spx i v)
             simplex;
         end)
      region_and_simplex_array
  in
    { m0_dim = d;
      m0_points = F.from_ml2 point_array;
      m0_simplices = simplices;
      m0_regions = regions; }

let get_nr_dims m0 = m0.m0_dim

let get_nr_points m0 =
  let n, _ = F.dim2 m0.m0_points in n

let get_nr_simplices m0 =
  let n, _ = I32.dim2 m0.m0_simplices in n

let get_points m0 = m0.m0_points

let get_simplices m0 = m0.m0_simplices

let get_regions m0 = m0.m0_regions

let get_simplex_point m0 nr_simplex nr_point =
  let point_id = I32.get2 m0.m0_simplices nr_simplex nr_point in
    Bigarray.Array2.slice_left m0.m0_points point_id

let get_simplex_points m0 nr_simplex =
  let points = m0.m0_points in
  let simplices = m0.m0_simplices in
  let sn, sd = I32.dim2 simplices in
  let pn, pd = F.dim2 points in
    Array.init sd
      (fun i ->
         let p_id = I32.get2 simplices nr_simplex i in
           Array.init pd (fun j -> F.get2 points p_id j))
