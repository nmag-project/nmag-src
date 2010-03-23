/* cubicmesh -- simple program to generate cubic unstructured meshes
 * Written by Matteo Franchin, 8 Jan 2008
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#define DIM 3

typedef int Int;
typedef double Real;

static const Real small_real = 1e-10;

typedef Real Point[DIM];
typedef Int IPoint[DIM];

typedef Int Simplex[DIM+1];
typedef Int Surface[DIM];

typedef struct {
  Real min_corner[DIM], max_corner[DIM];
} BBox;

typedef struct {
  Int num_points, num_simplices, num_surfaces;
  Int alc_num_points, alc_num_simplices, alc_num_surfaces;
  Point *points;
  Simplex *simplices;
  Surface *surfaces;
} Mesh;

static void *xmalloc(size_t size) {
  void *p = malloc(size);
  if ( p == NULL ) {
    /* This shouldn't anyway happen on linux, see malloc documentation. */
    fprintf(stderr, "Fatal error: malloc failed!\n");
    exit(EXIT_FAILURE);
  }
  return p;
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
static Point translation = {0.0};
static Real slant[DIM][DIM];

void slant_clear(void) {
  Int i, j;
  for(i=0; i<DIM; i++)
    for(j=0; j<DIM; j++)
      slant[i][j] = 0.0;
}

/* Function which returns 0 if |x - y|<small_real,
 * otherwise returns 1 if x > y or -1 if x < y.
 */
int compare(Real x, Real y) {
  if (fabs(x - y) < small_real) return 0;
  return (x > y) ? 1 : -1;
}

void translation_clear(void) {
  int i;
  for(i=0; i<DIM; i++) translation[i] = 0.0;
}

void translation_set(Real *t) {
  int i;
  for(i=0; i<DIM; i++) translation[i] = t[i];
}

void bbox_set(BBox *bbox, Real *corner1, Real *corner2) {
  int i;
  for(i=0; i<DIM; i++)
    if (corner1[i] > corner2[i]) {
      bbox->max_corner[i] = corner1[i];
      bbox->min_corner[i] = corner2[i];

    } else {
      bbox->max_corner[i] = corner2[i];
      bbox->min_corner[i] = corner1[i];
    }
}

void bbox_enlarge(BBox *bbox, Real delta) {
  int i;
  for(i=0; i<DIM; i++) {
    bbox->max_corner[i] += delta;
    bbox->min_corner[i] -= delta;
  }
}

/** Returns 1 if the point p lies inside the bounding box, 0 otherwise.
 * Returns -1 if p lies on the boundary (with respect to the function
 * almost_equal()).
 */
int bbox_is_inside(BBox *bbox, Real *p) {
  int i, is_inside = 1, is_on_boundary = 0;
  for(i=0; i<DIM; i++) {
    Real xi = p[i];
    int c1 = compare(xi, bbox->min_corner[i]),
        c2 = compare(xi, bbox->max_corner[i]);
    is_on_boundary |= (c1 == 0 || c2 == 0);
    if (c1 < 0 || c2 > 0) is_inside = 0;
  }
  if (is_inside && is_on_boundary) return 0;
  return is_inside ? 1 : -1;
}

void bbox_project(BBox *bbox, Real *p) {
  int i;
  for(i=0; i<DIM; i++) {
    Real xi = p[i];
    if (xi < bbox->min_corner[i])
      p[i] = bbox->min_corner[i];
    else if (xi > bbox->max_corner[i])
      p[i] = bbox->max_corner[i];
  }
}

void simplex_copy(Simplex *dest, Simplex *src) {
  int i;
  for(i=0; i<=DIM; i++) (*dest)[i] = (*src)[i];
}

void point_copy(Point *dest, Point *src) {
  int i;
  for(i=0; i<DIM; i++) (*dest)[i] = (*src)[i];
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
  Int i, j;
  Real s;
  for(i=0; i<DIM; i++) {
    s = 0.0;
    for(j=0; j<DIM; j++) s += slant[i][j]*ipos[j];
    pos[i] = ipos[i]*this_dd[i] + s + translation[i];
  }
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
          point_ipos[l] = ipos2[l] + cube_ipos;
        }
        point_index = get_index(point_ipos);
        out->simplices[out->num_simplices][k] = point_index;
      }
      ++out->num_simplices;
    }
  }

  return 1;
}

/** Removes simplices from a mesh. 'list' is a list of simplices indices
 * to be removed. 'list_size' is the size of such a list.
 */
void mesh_simplices_remove(Mesh *m, int *list, int list_size) {
  int i;
  int *marked_for_removal = xmalloc(sizeof(int)*m->num_simplices);
  int dest_idx=0;

  /* Find an array which contain for each simplex, the removal status. */
  for(i=0; i<m->num_simplices; i++) marked_for_removal[i] = 0;
  for(i=0; i<list_size; i++) marked_for_removal[list[i]] = 1;

  for(i=0; i<m->num_simplices; i++) {
    if (!marked_for_removal[i])
      simplex_copy(& (m->simplices[dest_idx++]), & (m->simplices[i]));
  }
  m->num_simplices = dest_idx;
}

/** Removes unused points from a mesh. */
void mesh_points_clean(Mesh *m) {
  int *marked_for_removal = xmalloc(sizeof(int)*m->num_points), *map;
  int i, j;
  /* Find the unused points */
  for(i=0; i<m->num_points; i++) marked_for_removal[i] = 1;
  for(i=0; i<m->num_simplices; i++) {
    for(j=0; j<=DIM; j++)
      marked_for_removal[m->simplices[i][j]] = 0;
  }

  /* Build the point index translation map */
  map = marked_for_removal;
  j = 0;
  for(i=0; i<m->num_points; i++)
    if (!marked_for_removal[i]) {
      point_copy(& (m->points[j]), & (m->points[i]));
      map[i] = j++;
    }

  m->num_points = j;

  /* Run over the simplices to change the point indices */
  for(i=0; i<m->num_simplices; i++)
    for(j=0; j<=DIM; j++)
      m->simplices[i][j] = map[m->simplices[i][j]];

  /* Run over the surfaces to change the point indices */
  for(i=0; i<m->num_surfaces; i++)
    for(j=0; j<DIM; j++)
      m->surfaces[i][j] = map[m->surfaces[i][j]];
}

/** Returns 0 if the simplex with ID 'simplex_id', belonging to the mesh 'm'
 * lies completely outside the bounding box 'bbox'.
 * Returns 1 if the simplex is completely inside, otherwise returns -1.
 */
int bbox_simplex_is_inside(BBox *bbox, Mesh *m, Int simplex_id) {
  int i, j, is_between = 0;
  for (i=0; i<DIM; i++) {
    int num_points_up=0, num_points_down=0;
    for(j=0; j<=DIM; j++) {
      Int p_idx = m->simplices[simplex_id][j];
      Real *p = m->points[p_idx];
      num_points_up += (compare(p[i], bbox->max_corner[i]) >= 0);
      num_points_down += (compare(p[i], bbox->min_corner[i]) <= 0);
    }
    if (num_points_up == DIM+1 || num_points_down == DIM+1) return -1;
    is_between |= (num_points_up != 0) || (num_points_down != 0);
  }
  return is_between ? 0: 1;
}

/** Adjusts a simplex if it goes outside the bounding box 'bbox'.
 * This is done by projecting the points outside the b.box on its surfaces.
 */
void bbox_simplex_project(BBox *bbox, Mesh *m, Int simplex_id) {
  int i;
  for(i=0; i<=DIM; i++) {
    Int p_idx = m->simplices[simplex_id][i];
    Real *p = m->points[p_idx];
    bbox_project(bbox, p);
  }
}

/** Run over the simplices of a mesh and remove all those which lie
 * outside the provided bounding box (bbox1 and bbox2 are the two
 * opposite corners of the bounding box.
 * If a simplex lies between the faces of the bounding box, then its is
 * treated as follows: the vertex of the simplex which lie outside
 * the bounding box are projected on its surfaces.
 */
void mesh_cut_bbox(Mesh *m, BBox *bbox) {
  Int i, list_idx = 0, list_size = m->num_simplices;
  int *list = xmalloc(sizeof(int)*list_size);

  /* First we delete those simplices which lie completely outside bbox */
  for(i=0; i<m->num_simplices; i++)
    if (bbox_simplex_is_inside(bbox, m, i) == -1) {
      assert(list_idx < list_size);
      list[list_idx++] = i;
    }
  mesh_simplices_remove(m, list, list_idx);
  mesh_points_clean(m);

  for(i=0; i<m->num_simplices; i++) {
    int sii = bbox_simplex_is_inside(bbox, m, i);
    assert(sii != -1);
    if (sii == 0)
      bbox_simplex_project(bbox, m, i);
  }
}

int write_mesh_nmesh(Mesh *m, const char *file_name) {
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
    fprintf(f, " 1");
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
  int exagonal = 1;
  BBox *bbox = (BBox *) NULL;

  if (argc != 2) {
    usage();
    exit(1);

  } else {
    int n;
    n = sscanf(argv[1], "%1000[^,],%lf:%d,%lf:%d,%lf:%d",
               file_name,
               & this_sz[0], & this_nn[0],
               & this_sz[1], & this_nn[1],
               & this_sz[2], & this_nn[2]
              );
    fprintf(stderr, "Writing mesh to '%s'\n", file_name);
    fprintf(stderr, "Size is %lfx%lfx%lf\n",
            this_sz[0], this_sz[1], this_sz[2]);
    fprintf(stderr, "Subdivided into %dx%dx%d\n",
            this_nn[0], this_nn[1], this_nn[2]);
    if (n != 7) {
      fprintf(stderr, "Error parsing the command line: %d parsed\n---\n", n);
      usage();
      exit(1);
    }
  }

  slant_clear();
  translation_clear();
  if (exagonal) {
    static BBox static_bbox;
    Real corner1[DIM], corner2[DIM], t[DIM];
    Real dx = this_sz[0]/this_nn[0], Dx;
    Int nn0 = (this_nn[1] + 1)/2;
    slant[0][1] = 0.5*dx;
    Dx = 0.5*dx*this_nn[1];
    t[0] = -Dx; t[1] = 0.0; t[2] = 0.0;
    translation_set(t);
    corner1[0] = 0.0; corner1[1] = 0.0; corner1[2] = 0.0;
    corner2[0] = this_sz[0]; corner2[1] = this_sz[1]; corner2[2] = this_sz[2];
    bbox_set(& static_bbox, corner1, corner2);
    bbox = & static_bbox;
    this_nn[0] += nn0;
    this_sz[0] += nn0*dx;
  }

  if (generate_cubic_mesh(& m)) {
    if (bbox != (BBox *) NULL) mesh_cut_bbox(& m, bbox);
    (void) write_mesh_nmesh(& m, file_name);
  }
  return 0;
}
