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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include "mgdesc.h"

#define DIM 3

static char *my_except ="ocaml_exn_hlib_mgdesc";

void *my_malloc(size_t size) {
  void *ptr = malloc(size);
  if (ptr == NULL) {
    fprintf(stderr, "mgdesc.c: malloc failed. Aborting...\n");
    abort();
  }
  return ptr;
}

void my_free(void *ptr) {
  if (ptr != NULL)
    free(ptr);
  /* The check should be done anyway in free... */
}

int matrix_is_one(Real *matrix) {
  size_t i, j;
  for (i = 0; i < DIM; i++)
    for (j = 0; j < DIM; j++) {
      Real expected_entry = (i == j) ? 1.0 : 0.0;
      if (matrix[i*DIM + j] != expected_entry)
        return 0;
    }

  return 1.0;
}

void mgdesc_destroy(MGDesc *mg_desc) {
  if (mg_desc != NULL) {
    my_free(mg_desc->matrices);
    my_free(mg_desc->copies);
    my_free(mg_desc);
  }
}

MGDesc *mgdesc_create(value ml_mg_desc) {
  value ml_otrans = Field(ml_mg_desc, 0),
        ml_copies_info = Field(ml_mg_desc, 1);

  size_t nr_copies = Wosize_val(ml_copies_info),
         nr_matrices = Wosize_val(ml_otrans),
         copy_idx, matrix_idx,
         matrix_nr_entries = DIM*DIM,
         matrix_size = sizeof(Real)*matrix_nr_entries;

  MGDesc *mg_desc = my_malloc(sizeof(MGDesc));

  mg_desc->matrices = my_malloc(matrix_size*nr_matrices);
  mg_desc->num_copies = nr_copies;
  mg_desc->copies = my_malloc(sizeof(MGCopy)*nr_copies);

  /* Initialise the matrices */
  for (matrix_idx = 0; matrix_idx < nr_matrices; matrix_idx++) {
    Real (*matrices)[3][3] = (Real (*)[3][3]) mg_desc->matrices,
         (*matrix)[3] = matrices[matrix_idx];
    value ml_matrix = Field(ml_otrans, matrix_idx);

    size_t i, j;
    if (Wosize_val(ml_matrix) == DIM) {
      for (i = 0; i < DIM; i++) {
        value ml_matrix_row = Field(ml_matrix, i);

        if (Wosize_val(ml_matrix_row)/Double_wosize == DIM) {
          for (j = 0; j < DIM; j++)
            matrix[i][j] = Double_field(ml_matrix_row, j);

        } else {
          /* NOTE: bound checks are done only for array-s which are not
                   guaranteed to have the right number of entry by the type
                   system. */
          mgdesc_destroy(mg_desc);
          raise_with_string(*caml_named_value(my_except),
                            "Matrix row has wrong number of entries.");
          assert(0);
        }
      }

    } else {
      mgdesc_destroy(mg_desc);
      raise_with_string(*caml_named_value(my_except),
                        "Matrix has wrong number of rows.");
      assert(0);
    }
  }

  /* Initialise the copies */
  for (copy_idx = 0; copy_idx < nr_copies; copy_idx++) {
    MGCopy *mg_copy = & mg_desc->copies[copy_idx];

    value ml_copy = Field(ml_copies_info, copy_idx);
    size_t nr_otrans = Int_val(Field(ml_copy, 0));
    value ml_translation = Field(ml_copy, 2);

    /* Set the greyfactor */
    mg_copy->grey_factor = Double_val(Field(ml_copy, 1));

    /* Set the pointer to the transformation matrix */
    if (nr_otrans < nr_matrices) {
      Real *the_matrix =
        (Real *) ((Real (*)[3][3]) mg_desc->matrices)[nr_otrans];
      mg_copy->matrix = matrix_is_one(the_matrix) ? NULL : the_matrix;

    } else {
      mgdesc_destroy(mg_desc);
      raise_with_string(*caml_named_value(my_except),
                        "Transformation index is out of bounds.");
      assert(0);
    }

    /* Set the translation vector */
    if (Wosize_val(ml_translation)/Double_wosize == DIM) {
      size_t i;
      for (i = 0; i < DIM; i++)
        mg_copy->translation[i] = Double_field(ml_translation, i);

    } else {
      mgdesc_destroy(mg_desc);
      raise_with_string(*caml_named_value(my_except),
                        "Translation vector should have dimension 3.");
      assert(0);
    }
  }

  return mg_desc;
}

void mgdesc_fprint(MGDesc *mg_desc, FILE *f) {
  size_t copy_idx;

  fprintf(f, "Macro-Geometry descriptor:\n");
  fprintf(f, "  num_copies=%d\n", (int) mg_desc->num_copies);

  for (copy_idx = 0; copy_idx < mg_desc->num_copies; copy_idx++) {
    MGCopy *c = & mg_desc->copies[copy_idx];
    size_t i, j;

    fprintf(f, "  Copy n. %d\n", (int) copy_idx);
    fprintf(f, "    grey_factor=%g\n", c->grey_factor);
    fprintf(f, "    translation=[%g, %g, %g]\n",
            c->translation[0], c->translation[1], c->translation[2]);
    if (c->matrix != NULL) {
      fprintf(f, "    matrix=%p\n", c->matrix);
      for (i = 0; i < DIM; i++) {
        char *sep = "      ";
        for (j = 0; j < DIM; j++) {
          fprintf(f, "%s%g", sep, c->matrix[j + i*DIM]);
          sep = ",\t";
        }
        fprintf(f, "\n");
      }

    } else
      fprintf(f, "    matrix not given, assuming identity\n");
  }

  fprintf(f, "END of Macro-Geometry descriptor\n");
}

CAMLprim value caml_mgdesc_print_debug(value ml_mg_desc) {
  CAMLparam1(ml_mg_desc);
  MGDesc *mgdesc = mgdesc_create(ml_mg_desc);
  mgdesc_fprint(mgdesc, stderr);
  mgdesc_destroy(mgdesc);
  CAMLreturn(Val_unit);
}
