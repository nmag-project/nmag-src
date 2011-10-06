
type tensor_index = IX_int of int | IX_name of string 

type parsed_difftype = PDIFF_none | PDIFF_vol of tensor_index | PDIFF_boundary of tensor_index

(* === OBSOLETE ===
type doftype =
  | DOFTYPE_all
  | DOFTYPE_vol of int option list
      (* list of region numbers. Option None = all regions *)
  | DOFTYPE_boundary of ((int option) * (int option)) list
      (* List of selected boundaries *)
*)

type dof_logic =
  | DLOG_true
      (* Would be logically equivalent to DLOG_and [],
	 but it makes sense to have a specific extra
	 notion for this. *)
  | DLOG_and  of dof_logic list
  | DLOG_or   of dof_logic list
  | DLOG_not  of dof_logic
  | DLOG_all of string
  | DLOG_some of string
  | DLOG_nregions of int
      
type nsi_field = (* numerically or symbolically indexed field *)
    {fnsi_name: string;
     fnsi_indices: tensor_index array;
     fnsi_bspec: dof_logic;
    }

type field =
    {f_name: string;
     f_indices: int array;
     f_bspec: dof_logic;
    }


type amendment =
  | AMDMT_MXDIM of nsi_field list option * nsi_field list option
  | AMDMT_DIAGONAL_ONES of nsi_field * nsi_field 
  | AMDMT_GAUGE_FIX of nsi_field
  | AMDMT_PERIODIC of nsi_field


type diff_field_spec = field * parsed_difftype

type diff_mxdim_spec = string list option


type difftype = DIFF_none | DIFF_vol of int | DIFF_boundary of int

type ddiffop =
    (* This is what our friends who use the parser actually
       want to work with in the end:

       We need a "hook", which is:

       ((dof_name_left,surface_handling_left),
        (dof_name_right,surface_handling_right),
        opt_dof_name_middle)

       For every such hook, we have an array of contribs,
       where every contrib is:

       (coeff,difftype_le,difftype_ri)
       
    *)
    {diff_contribs:
       (field array * (* length-2 (left,right) or length-3 (left,right,mid) *)
	  (difftype * difftype, float) Hashtbl.t) array;
     diff_mxdim: field array * field array; (* MatriXDIMension *)
     diff_diag_ones: (field * field) array;
     diff_gauge_fix: field array;
     diff_periodic: field array;
    }
