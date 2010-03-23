/* cubicmesh -- simple program to generate cubic unstructured meshes
 * Written by Matteo Franchin, 8 Jan 2008
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <assert.h>

#define DIM 3

typedef int Int;
typedef double Real;
typedef Real Point[DIM];
typedef Int IPoint[DIM];

typedef Int Simplex[DIM+1];
typedef Int Surface[DIM];

typedef struct {
  Int num_points, num_simplices, num_surfaces;
  Int alc_num_points, alc_num_simplices, alc_num_surfaces;
  Point *points;
  Simplex *simplices;
  Surface *surfaces;
} Mesh;

typedef struct _filter_s {
  Int dim, region;
  Real less_than;
  struct _filter_s *next;
} Filter;

static void *xmalloc(size_t size) {
  void *p = malloc(size);
  if ( p == NULL ) {
    /* This shouldn't anyway happen on linux, see malloc documentation. */
    fprintf(stderr, "Fatal error: malloc failed!\n");
    exit(EXIT_FAILURE);
  }
  return p;
}

static char *xstrdup(const char *s) {
  char *x = strdup(s);
  if (x != NULL)
    return x;

  else {
    fprintf(stderr, "Fatal error: strdup failed!\n");
    exit(EXIT_FAILURE);
  }
  return NULL;
}

#if DIM == 3
#define cube_num_points 8
static IPoint cube_ipoints[cube_num_points] = {
  {0, 0, 0},
  {1, 0, 0},
  {1, 1, 1},
  {1, 0, 1},
  {0, 1, 1},
  {0, 1, 0},
  {0, 0, 1},
  {1, 1, 0}
};

#define cube_num_simplices 6
static Simplex cube_simplices[cube_num_simplices] = {
  {3, 5, 1, 2},
  {3, 5, 4, 0},
  {3, 5, 2, 4},
  {4, 3, 0, 6},
  {3, 5, 0, 1},
  {1, 5, 7, 2}
};

#define cube_num_surfaces 12
static Surface cube_surfaces[cube_num_surfaces] = {
  {2, 3, 4},
  {0, 4, 6},
  {0, 4, 5},
  {0, 1, 3},
  {0, 1, 5},
  {2, 4, 5},
  {1, 5, 7},
  {3, 4, 6},
  {1, 2, 7},
  {2, 5, 7},
  {1, 2, 3},
  {0, 3, 6}
};

#else
#  error Mesh generation for dimension different from 3 is not supported yet.
#endif

static Int this_nn[DIM];
static Real this_sz[DIM], this_dd[DIM];

Filter *Filter_Add(Filter *f, int dim, Real less_than) {
  Filter *new = xmalloc(sizeof(Filter));
  new->dim = dim;
  new->less_than = less_than;
  new->next = f;
  new->region = (f != NULL) ? f->region + 1 : 2;

  if (f != NULL) {
    if (f->less_than > less_than) {
      fprintf(stderr, "Error: coordinates given for region filter should be "
                      "ordered from smaller to greater.\n");
      exit(EXIT_FAILURE);
    }
  }

  return new;
}

void Filter_Destroy(Filter *f) {
  while (f != NULL) {
    Filter *this = f;
    f = f->next;
    free(this);
  };
}

void Filter_Print(FILE *out, Filter *f) {
  if (f == NULL)
    return;

  else {
    int dim = -1;
    char *sep = "";

    for(; f != NULL; f = f->next) {
      assert(f->dim >= 0);
      if (f->dim != dim) {
        fprintf(out, "%s%d:%f", sep, f->dim, f->less_than);
        sep = " + ";
        dim = f->dim;

      } else
        fprintf(out, ",%f", f->less_than);
    }
  }
}

static Filter *Filter_From_String_Ret(char *my_s, int err, Filter *f) {
  free(my_s);
  switch(err) {
  case 1:
    fprintf(stderr, "Filter specification should begin with 'x', 'y', 'z' "
                    "or an integer number followed by ':'\n");
    break;
  case 2:
    fprintf(stderr, "Filter specification should begin with \"x:\" "
                    "followed by real numbers separated by a comma.\n");
    break;
  }
  return f;
}

Filter *Filter_From_String(const char *s) {
  Filter *f = NULL;
  char *save_ptr, *token, *my_s;
  int dim = -1;
  Real coord;

  my_s = xstrdup(s);
  token = strtok_r(my_s, ":", & save_ptr);
  if (token == NULL)
    return Filter_From_String_Ret(my_s, 1, NULL);

  if (strlen(token) == 1) {
    switch(token[0]) {
    case 'x': dim = 0; break;
    case 'y': dim = 1; break;
    case 'z': dim = 2; break;
    }
  }
  if (dim == -1) {
    if (sscanf(token, "%d", & dim) != 1)
      return Filter_From_String_Ret(my_s, 1, NULL);
  }

  while(1) {
    token = strtok_r(NULL, ",", & save_ptr);
    if (token == NULL)
      return Filter_From_String_Ret(my_s, 0, f);
    if (sscanf(token, "%lf", & coord) != 1)
      return Filter_From_String_Ret(my_s, 2, f);
    f = Filter_Add(f, dim, coord);
  }

  assert(0);
  return NULL;
}

Int Filter_Apply_To_Point(Filter *f, Point *p) {
  for (; f != NULL; f = f->next) {
    Int dim = f->dim;
    Real d = f->less_than - (*p)[dim];
    /*printf("%f < %f?\n", f->less_than, (*p)[dim]);*/
    if (fabs(d) <= 1e-10)
      return -1;
    else if (d < 0.0)
      return f->region;
  }
  return 1;
}

Int Filter_Apply_To_Simplex(Filter *f, Mesh *m, Int simplex) {
  if (f == NULL)
    return 1;

  else {
    Int region = -1;
    int i;

    for (i = 0; i <= DIM; i++) {
      Int p_idx = m->simplices[simplex][i];
      Point *p = & m->points[p_idx];
      Int this_region = Filter_Apply_To_Point(f, p);
      if (this_region >= 0 && region >= 0) {
        if (this_region != region) {
          int j;
          fprintf(stderr, "Simplex appears to be in the middle of two regions!\n");
          for(j = 0; j <= DIM; j++) {
            Point *pp = & m->points[m->simplices[simplex][j]];
            printf("%d -> (%g, %g, %g)\n", j, (*pp)[0], (*pp)[1], (*pp)[2]);
          }
        }
      }

      if (this_region >= 0)
        region = this_region;
    }
    return region;
  }
}

/* These routines are thought to work (in principle) with arbitrary space
 * dimensions.
 */
Int get_index(Int *ipos) {
  Int i, index=0;
  for(i=0; i<DIM; i++) index = index*(this_nn[i]+1) + ipos[i];
  return index;
}

void get_ipos(Int *ipos, Int index) {
  Int i;
  for(i=DIM-1; i>=0; i--) {
    Int nn = this_nn[i]+1;
    ipos[i] = index % nn;
    index /= nn;
  }
}

Int get_index2(Int *ipos2) {
  Int i, index2=0;
  for(i=0; i<DIM; i++) index2 = index2*this_nn[i] + ipos2[i];
  return index2;
}

void get_ipos2(Int *ipos2, Int index2) {
  Int i;
  for(i=DIM-1; i>=0; i--) {
    Int nn = this_nn[i];
    ipos2[i] = index2 % nn;
    index2 /= nn;
  }
}

void get_pos(Real *pos, Int *ipos) {
  Int i;
  for(i=0; i<DIM; i++) pos[i] = ipos[i]*this_dd[i];
}

int generate_cubic_mesh(Mesh *out) {
  Int npt=1, ncb=1, nsx;
  Int i, j, k, l;

  for(i=0; i<DIM; i++) {
    if (this_nn[i] < 1) {
      fprintf(stderr, "Cube number in dimension %d must be positive!\n", i);
      return 0;
    }
    if (this_sz[i] <= 0.0) {
      fprintf(stderr, "Sides of cube must have positive lengths!\n");
      return 0;
    }
    npt *= (this_nn[i] + 1); /* num points */
    ncb *= this_nn[i]; /* num cubes */
    this_dd[i] = this_sz[i]/this_nn[i];
  }

  nsx = ncb*cube_num_simplices;

  out->num_points = 0;
  out->num_simplices = 0;
  out->num_surfaces = 0;
  out->alc_num_points = npt;
  out->alc_num_simplices = nsx;
  out->alc_num_surfaces = 0;
  out->points = (Point *) xmalloc(npt*sizeof(Point));
  out->simplices = (Simplex *) xmalloc(nsx*sizeof(Simplex));
  out->surfaces = (Surface *) NULL;

  /* First we fill the point array */
  out->num_points = npt;
  for(i = 0; i < npt; i++) {
    Int ipos[DIM];
    get_ipos(ipos, i);
    if (0) {
      Int dd;
      dd = get_index(ipos);
      printf("i=%d --> ipos={%d, %d, %d} --> i=%d\n", i, ipos[0], ipos[1], ipos[2], dd);
    }
    get_pos(out->points[i], ipos);
  }

  for(i=0; i < ncb; i++) {
    Int ipos2[DIM];
    get_ipos2(ipos2, i);
    for(j=0; j<cube_num_simplices; j++) {
      for(k=0; k<=DIM; k++) {
        Int point_ipos[DIM], point_index;
        Int sx_k = cube_simplices[j][k];
        for(l=0; l<DIM; l++) {
          Int cube_ipos = cube_ipoints[sx_k][l];
          point_ipos[l] = ipos2[l] +
                          ((ipos2[l] % 2 == 0) ? cube_ipos : 1 - cube_ipos);
        }
        point_index = get_index(point_ipos);
        out->simplices[out->num_simplices][k] = point_index;
      }
      ++out->num_simplices;
    }
  }

  return 1;
}

int write_mesh_nmesh(Mesh *m, const char *file_name, Filter *filter) {
  FILE *f;
  Int i, j;
  f = fopen(file_name, "w");
  if (f == (FILE *) NULL) {
    fprintf(stderr, "Cannot open file '%s' for writing!\n", file_name);
    return 0;
  }

  fprintf(f, "# PYFEM mesh file version 1.0\n");
  fprintf(f, "# dim = %d \tnodes = %d \tsimplices = %d \t"
             "surfaces = %d \tperiodic = 0\n",
             DIM, m->num_points, m->num_simplices,
             m->num_surfaces);

  fprintf(f, "%d\n", m->num_points);
  for(i=0; i<m->num_points; i++) {
    for(j=0; j<DIM; j++)
      fprintf(f, " %f", m->points[i][j]);
    fprintf(f, "\n");
  }
  fprintf(f, "%d\n", m->num_simplices);
  for(i=0; i<m->num_simplices; i++) {
    Int region = Filter_Apply_To_Simplex(filter, m, i);
    fprintf(f, " %d", region);
    for(j=0; j<=DIM; j++)
      fprintf(f, " %d", m->simplices[i][j]);
    fprintf(f, "\n");
  }
  fprintf(f, "0\n");
  for(i=0; i<m->num_surfaces; i++) {
    fprintf(f, " 1 -1");
    for(j=0; j<DIM; j++)
      fprintf(f, " %d", m->surfaces[i][j]);
    fprintf(f, "\n");
  }
  fprintf(f, "0\n");
  (void) fclose(f);
  return 1;
}

void usage(void) {
  fprintf(stderr,
    "Cubicmesh is a simple program to create regular cubic "
    "unstructured meshes\n\n"
    "USAGE: cubicmesh file.out,10.0:5,20.0:10,30.0:15\n\n"
    "where there are no spaces between numbers and separators.\n"
    "we first specify the file name of the output mesh.\n"
    "we then specify the side length and subdivision as length:subdivision "
    "for the first side of the cubic mesh (x). With a comma we separate "
    "an analogous specification for the second side (y) and so on.\n"
  );
}

int main(int argc, char **argv) {
  Mesh m;
  char file_name[1024];
  Filter *filter = NULL;

  if (argc == 2 || argc == 3) {
    int n;
    n = sscanf(argv[1], "%1000[^,],%lf:%d,%lf:%d,%lf:%d",
               file_name,
               & this_sz[0], & this_nn[0],
               & this_sz[1], & this_nn[1],
               & this_sz[2], & this_nn[2]);
    fprintf(stderr, "Writing mesh to '%s'\n", file_name);
    fprintf(stderr, "Size is %lfx%lfx%lf\n",
            this_sz[0], this_sz[1], this_sz[2]);
    fprintf(stderr, "Subdivided into %dx%dx%d\n",
            this_nn[0], this_nn[1], this_nn[2]);
    if (n != 7) {
      fprintf(stderr, "Error parsing the command line: %d parsed\n---\n", n);
      usage();
      exit(EXIT_FAILURE);
    }

    if (argc == 3) {
      filter = Filter_From_String(argv[2]);
      printf("Adding filter: ");
      Filter_Print(stdout, filter);
      printf("\n");
    }

  } else {
    usage();
    exit(EXIT_FAILURE);
  }

  if (generate_cubic_mesh(& m))
    (void) write_mesh_nmesh(& m, file_name, filter);

  Filter_Destroy(filter);
  return 0;
}
