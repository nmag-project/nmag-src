val nr_true : bool array -> int 
val all_combinations : int -> bool array array 
val reduce_array : 'a array -> 'a array 
val bc_box : float array -> float array -> float array -> float
val components_from_directions : bool array -> bool array list 
val all_directions_but : bool array -> bool array
val periodicity_from_direction : bool array -> bool array array
val periodic_directions : bool array -> bool array array
val mask_coords : bool array -> float array -> float array -> float array -> float array
val maskarr :
  bool array ->
  float array -> float array -> float array array -> float array array
val corner_points : float array * float array -> float array array
val unmask_coords :
  bool array ->
  float array -> float array -> float array -> float array array 
val merge_periodic_pts :
  (int, 'a array) Hashtbl.t -> (int, 'a array) Hashtbl.t 
