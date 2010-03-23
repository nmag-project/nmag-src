
type llg_material =
  {
    llg_name: string;
    llg_Ms: float; (* M_sat *)
    llg_J: float; (* J_exch *)
    llg_c1: float;
    llg_c2: float;
    llg_c3: float;
    llg_anisotropy: (float array -> float) option;
    llg_anisotropy_order: int;
  }
;;

val make_mumag_linalg_machine :
  ?ccpla:(Nsim.nsim_ccpla_opcode, 'a) Ccpla.ccpla ->
  prefix:string ->
  ?pass_on_prematrices_by_name:(string, (int array, float) Hashtbl.t -> unit)
                               Hashtbl.t ->
  mwe_m:float Fem.mesh_with_elements ->
  mwe_dm_dt:float Fem.mesh_with_elements ->
  mwe_H_total:float Fem.mesh_with_elements ->
  mwe_H_demag:float Fem.mesh_with_elements ->
  mwe_H_exch:float Fem.mesh_with_elements ->
  mwe_H_anis:float Fem.mesh_with_elements ->
  mwe_H_ext:float Fem.mesh_with_elements ->
  inside_regions:Mesh.simplex_region array ->
  ?fun_make_and_register_mwe_field:(float Fem.mesh_with_elements -> 'b) ->
  llg_material array -> Nsim.linalg_machine
