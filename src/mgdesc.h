/* Nmag micromagnetic simulator
 * Copyright (C) 2011 University of Southampton
 * Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
 *
 * WEB:     http://nmag.soton.ac.uk
 * CONTACT: nmag@soton.ac.uk
 *
 * AUTHOR(S) OF THIS FILE: Matteo Franchin
 * LICENSE: GNU General Public License 2.0
 *          (see <http://www.gnu.org/licenses/>)
 *
 * Build a C descriptor for the specification of the macro-geometry lattice.
 * NOTE: MG stands for (Macro Geometry)
 */

#ifndef _NSIM_HLIB_MGDESC_H
#  define _NSIM_HLIB_MGDESC_H

#  include <stdlib.h>

typedef double Real;

typedef struct {
  Real *matrix,
       grey_factor;
  Real translation[3];
} MGCopy;

typedef struct {
  Real   *matrices;
  size_t num_copies;
  MGCopy *copies;
} MGDesc;


/** Translate the OCaml description of the MG lattice to a MGDesc
 * datastructure.
 * The OCaml value has the following form:
 *
 *   ([|otrans1, otrans2, ...|],
 *    [|(nr_otrans1, greyfactor1, translation1), ...|])
 *
 * and the following type:
 *
 *   float array array array * (int * float * float array) array;
 *
 * where:
 *
 * - [|otrans1, otrans2, ...|] is an array of matrices (transformations)
 * - [|(nr_otrans1, greyfactor1, translation1), ...|] is an array containing
 *   one element for each of the periodic copies. nr_otrans1 is an integer
 *   which selects the matrix for the copy among otrans1, otrans2, etc.
 *   greyfactor1 is a factor used as a weight for the copy.
 *   translation1 is a 3D vector used to displace the copy with respect to
 *   the position of the original cell.
 */
MGDesc *mgdesc_create(value ml_mg_desc);

/** Destroy an MGDesc datastructure created with mgdesc_create. */
void mgdesc_destroy(MGDesc *mg_desc);

#endif
