val find_interface_points_to_add_between_mesh_pieces :
  float array array array array ->
  (float array array * (float array array * (int * int))) array
val find_simplices_sharing_edge :
  float array array array -> float array array array -> (float array array * int list) array
val remove_surfaces_with_edge :
  'a array -> 'a array array -> 'a array list
val extract_surfaces_with_edge :
  'a array -> 'a array array -> 'a array array
val list_filteri : (int -> 'a -> bool) -> 'a list -> 'a array
val find_external_edges : float array array array -> float array array array
val difference_elements_arrays : 'a array -> 'a array -> 'a array 
val extract_couples : 'a array -> 'a array array
val find_simplices_sharing_vertices :
  float array array array ->
  float array array array -> (float array array * int list) array
