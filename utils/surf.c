/* (C) 2006 Matteo Franchin
 * Converter from our mesh-format to VRML format (v. 2.0)
 * plus other cool effects :-)
 */

#define SURF_VERSION "surf 0.1"
#define SURF_AUTHORS "Matteo Franchin <fnch@libero.it>"

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <math.h>
#include <ctype.h>

#ifdef HASHTABLE
#include "hashtable.h"
#endif

typedef double Real;
#define SREAL "%lf"

/* #define DEBUG */
#define SHOWS_VOLUME0_WARNING

typedef struct {Real x, y, z;} Point;

typedef struct {int body, p[4], left_handed;} Simplex;

typedef struct {
  int body, p[3], rs1;
  Simplex *parent;
  int wrong_order;
} Surface;

typedef struct {
  int min_body, max_body;
  int num_nodes, num_simplices, num_surfaces;
  Point *point_arr;
  Simplex *simplex_arr;
  Surface *surface_arr;
} Mesh;

typedef struct sn {
  struct sn *next;
  int id;
} SelectedNodes;

typedef int IsInside(Point *);

/*****************************************************************************
 *                             Useful functions                              *
 *****************************************************************************/

void *xmalloc(size_t size) {
  void *p = malloc(size);
  if ( p == NULL ) {
    fprintf(stderr, "Fatal error: malloc failed!\n");
    exit(EXIT_FAILURE);
  }
  return p;
}

/* For n = 3 this trivial algorithm is fast as the others.
 * NOTE: returns the number of permutations performed.
 */
int order(int *array, int n) {
  register int i, this, last, j;
  int np = 0;
  for(j = n; j > 1; j--) {
    last = array[0];
    for(i = 1; i < j; i++) {
      this = array[i];
      if ( last < this ) {
        last = this;
      } else {
        array[i] = last;
        array[i-1] = this;
        ++np;
      }
    }
  }
  return np;
}

/* This function returns 1 if the simplex is left-handed, 0 if it is
 * right-handed
 */
int simplex_left_handed(Point *v) {
  Point v01 = {v[1].x - v[0].x, v[1].y - v[0].y, v[1].z - v[0].z};
  Point v02 = {v[2].x - v[0].x, v[2].y - v[0].y, v[2].z - v[0].z};
  Point v03 = {v[3].x - v[0].x, v[3].y - v[0].y, v[3].z - v[0].z};
  Real volume = 
     v01.x * (v02.y * v03.z - v02.z * v03.y)
   + v01.y * (v02.z * v03.x - v02.x * v03.z)
   + v01.z * (v02.x * v03.y - v02.y * v03.x);
#ifdef SHOWS_VOLUME0_WARNING
  if ( volume == 0.0 )
    fprintf(stderr, "simplex_left_handed: this simplex has zero volume!\n");
#endif
  return volume > 0;
}

/*****************************************************************************
 *             Functions to read and write meshes to file                    *
 *****************************************************************************/
int nl;
char *err_msg;

int mesh_read(FILE *f, Mesh *m) {
  char line1[] = "# PYFEM mesh file version 1.0";
  char *line2[4] = {"dim", "nodes", "simplices", "surfaces"};
  char line[4][128];
  int r, _n[4], *n = _n;
  Real x, y, z;

  /* We begin parsing the header */
  nl = 1;
  if ( fscanf(f, "%128[^\n]\n", line[0]) != 1 ) {
    err_msg = "This file has a wrong header: is it a PYFEM mesh file?";
    return 0;
  }

  if ( strcmp(line[0], line1) != 0 ) {
    err_msg = "This file has a wrong header: is it a PYFEM mesh file?";
    return 0;
  }

  ++nl;

  r = fscanf(f, "# %128s = %d %128s = %d %128s = %d %128s = %d\n",
   line[0], n, line[1], n+1, line[2], n+2, line[3], n+3);
  if ( r != 8 ) { err_msg = "Parse error."; return 0; }

  {
    int i;
    for(i = 0; i < 3; i++) {
      if ( strcmp(line[i], line2[i]) != 0 ) {
        err_msg = "This seems to be a broken PYFEM mesh file.";
        return 0;
      }
    }
  }

  /* Some more checks */
  {
    int i;
    if ( n[0] != 3 ) {
      err_msg = "This mesh has dimension different to 3: not supported!";
      return 0;
    }
    for(i = 1; i < 3; i++) {
      if ( n[i] < 1 ) {
        if ( n[i] < 0 ) {
          err_msg = "Found negative value for nodes/simplices/surfaces.";
          return 0;
        }
        n[i] = 0;
        fprintf(stderr, "WARNING: The mesh has no %s!\n", line2[i]);
      }
    }
  }

  /* Here we can allocate the arrays */
  m->point_arr = (Point *) xmalloc( sizeof(Point) * n[1] );
  m->simplex_arr = (Simplex *) xmalloc( sizeof(Simplex) * n[2] );
  m->surface_arr = (Surface *) xmalloc( sizeof(Surface) * n[3] );

  /* Here we read the nodes */
  ++nl;
  if ( fscanf(f, "%d\n", & m->num_nodes) != 1 ) {
    err_msg = "Parse error.";
    return 0;
  }
  if ( m->num_nodes != n[1] ) {
    err_msg = "Number of nodes is different to the one specified in header.";
    return 0;
  }
  {
    int i;
    for(i = 0; i < m->num_nodes; i++) {
      ++nl;
      if ( fscanf(f, SREAL" "SREAL" "SREAL"\n", & x, & y, & z) != 3 ) {
        err_msg = "Parse error.";
        return 0;
      }
      m->point_arr[i].x = x;
      m->point_arr[i].y = y;
      m->point_arr[i].z = z;
    }
  }

  /* Here we read the nodes */
  ++nl;
  if ( fscanf(f, "%d\n", & m->num_simplices) != 1 ) {
    err_msg = "Parse error.";
    return 0;
  }
  if ( m->num_simplices != n[2] ) {
    err_msg = "Number of nodes is different to the one specified in header.";
    return 0;
  }
  {
    int i, first = 1;
    int body, p0, p1, p2, p3;
    Point v[4];
    for(i = 0; i < m->num_simplices; i++) {
      ++nl;
      if (fscanf(f, "%d %d %d %d %d\n", & body, & p0, & p1, & p2, & p3) != 5) {
        err_msg = "Parse error.";
        return 0;
      }
      m->simplex_arr[i].body = body;
      m->simplex_arr[i].p[0] = p0;
      m->simplex_arr[i].p[1] = p1;
      m->simplex_arr[i].p[2] = p2;
      m->simplex_arr[i].p[3] = p3;
      v[0] = m->point_arr[p0];
      v[1] = m->point_arr[p1];
      v[2] = m->point_arr[p2];
      v[3] = m->point_arr[p3];
      m->simplex_arr[i].left_handed = simplex_left_handed(v);

      if ( first ) {
        first = 0;
        m->max_body = m->min_body = body;
      } else {
        if ( body < m->min_body ) m->min_body = body;
        if ( body > m->max_body ) m->max_body = body;
      }
    }
  }

  /* Here we read the surfaces */
  ++nl;
  if ( fscanf(f, "%d\n", & m->num_surfaces) != 1 ) {
    err_msg = "Parse error.";
    return 0;
  }
  if ( m->num_surfaces != n[3] ) {
    err_msg = "Number of nodes is different to the one specified in header.";
    return 0;
  }
  {
    int i;
    int body, p0, p1, p2;
    for(i = 0; i < m->num_surfaces; i++) {
      ++nl;
      if (fscanf(f, "%d %d %d %d\n", & body, & p0, & p1, & p2) != 4) {
        err_msg = "Parse error.";
        return 0;
      }
      m->surface_arr[i].body = body;
      m->surface_arr[i].p[0] = p0;
      m->surface_arr[i].p[1] = p1;
      m->surface_arr[i].p[2] = p2;
      m->surface_arr[i].parent = NULL;
      m->surface_arr[i].wrong_order = 0;
    }
  }
  return 1;
}

int vrml_write(FILE *f, int num_points, Point *points,
 Surface *triangles, int num_triangles) {
  int i;
  char *part0 = {
    "#VRML V2.0 utf8\n\n"
    "Transform{\n"
    " children[\n"
    "  Shape{\n"
    "   appearance Appearance{\n"
    "    material Material{}\n"
    "   }\n\n"
    "   geometry IndexedFaceSet{\n"
  };

  char *part1 = {
    "    coord  Coordinate{\n"
    "     point[\n"
  };

  char *part2 = {
    "     ]\n"
    "    }\n"
    "    coordIndex[\n"
  };

  char *part3 = {
    "    ]\n"
    "   }\n"
    "  }\n"
    " ]\n"
    "}\n"
  };

  fprintf(f, part0);
  fprintf(f, "    # %d vertices %d triangles\n\n", num_points, num_triangles);

  fprintf(f, part1);
  for(i = 0; i < num_points; i++)
    fprintf(f, "      %f %f %f\n", points[i].x, points[i].y, points[i].z);

  fprintf(f, part2);
  for(i = 0; i < num_triangles; i++) {
    if ( triangles[i].wrong_order ) {
      fprintf(f, "     %d, %d, %d, -1,\n",
       triangles[i].p[1], triangles[i].p[0], triangles[i].p[2]);
    } else {
      fprintf(f, "     %d, %d, %d, -1,\n",
       triangles[i].p[0], triangles[i].p[1], triangles[i].p[2]);
    }
  }

  fprintf(f, part3);
  return 1;
}

vrml_sphere(FILE *f, Real r, Real x, Real y, Real z) {
  fprintf(f,
   "Transform{\n  children Shape{\n    appearance Appearance{\n"
   "        material Material{diffuseColor 0 0.800000 0}\n    }\n"
   "    geometry Sphere {radius %f}\n  }\n  translation %f %f %f\n}",
   r, x, y, z
  );
}

#ifdef HASHTABLE
/*****************************************************************************
 *                 Function which cleans the list of nodes                   *
 *****************************************************************************/

int mesh_cleanup(Mesh *m) {
  int i, distinct_points = 0, total_iter = 0;
  Hashtable *ht;

  fprintf(stderr, "Removing duplicate points..");
  ht = hashtable(1000);
  for(i = 0; i < m->num_nodes; i++) {
    Point *p = m->point_arr + i;
    HashItem *hi;
    ++total_iter;
    if ( ! hashtable_find(ht, p, sizeof(Point), & hi) ) {
      ++distinct_points;
      hashtable_insert(ht, p, sizeof(Point));
    }
  }
  for(i = 0; i < m->num_nodes; i++) {
    Point *p = m->point_arr + i;
    HashItem *hi;
    ++total_iter;
    if ( ! hashtable_find(ht, p, sizeof(Point), & hi) ) {
      ++distinct_points;
      hashtable_insert(ht, p, sizeof(Point));
    }
  }

  hashtable_statistics(ht, stderr);
  fprintf(stderr, "Found %d distinct point (total is %d - %d)\n",
   distinct_points, m->num_nodes, total_iter);
  hashtable_destroy(ht);

  return 1;
}
#endif

/*****************************************************************************
 *                           Handling of surfaces                            *
 *****************************************************************************/
static int triangle_num = 0, triangle_max;
static Surface *triangle_buffer = NULL;

/* Reset the list of triangles */
void triangle_reset(void) {
  free( triangle_buffer );
  triangle_buffer = NULL;
  triangle_num = 0;
  triangle_max = 0;
}

/* Initializes the list of triangles */
int triangle_list(int num_triangles) {
  triangle_reset();

  if ( num_triangles > 0 ) {
    triangle_buffer = (Surface *) xmalloc(sizeof(Surface) * num_triangles);
  } else {
    err_msg = "Non-positive number of triangles specified.";
    return 0;
  }
  triangle_max = num_triangles;
  return 1;
}

/* Compares two triangles */
int triangle_are_equal(Surface *t1, Surface *t2) {
  return
      (t1->p[0] == t2->p[0])
   && (t1->p[1] == t2->p[1])
   && (t1->p[2] == t2->p[2]);
}

int triangle_contain_vertex(Surface *s, int v) {
  if ( v < 0 ) return 1;
  return (s->p[0] == v) || (s->p[1] == v) || (s->p[2] == v);
}

/* Print a surface only if it contains the vertices v1, v2 and/or v3.
 * If v1 (or v2, v3) is negative this vertex is not searched.
 */
int triangle_print_surface_if(Surface *s, int v1, int v2, int v3) {
  if (
          triangle_contain_vertex(s, v1)
       && triangle_contain_vertex(s, v2)
       && triangle_contain_vertex(s, v3)
     ) {
    fprintf(stderr, "Triangle {%d, %d, %d}", s->p[0], s->p[1], s->p[2]);
    return 1;
  }
  return 0;
}

/* Add a triangle to the list of the triangle surfaces */
int triangle_add(Surface *s) {
  int i, wrong_order;
  Surface os;

  os = *s;
  wrong_order = order(os.p, 3) & 1;
  os.wrong_order ^= wrong_order;

#ifdef DEBUG
  fprintf(stderr, "triangle_add: ");
  triangle_print_surface_if(s, -1, -1, -1);
  fprintf(stderr, " --> trasformed:  ");
  triangle_print_surface_if(& os, -1, -1, -1);
  fprintf(stderr, " with wrong_order = %d\n", os.wrong_order);
  fflush(stderr);
#endif

  for(i = 0; i < triangle_num; i++) {
    Surface *tr = triangle_buffer + i;
    if ( triangle_are_equal(tr, & os) ) {
#ifdef DEBUG
      if ( triangle_print_surface_if(& os, 38, -1, -1) ) fprintf(stderr, " (ADDED)\n");
#endif
      ++tr->rs1;
      return 1;
    }
  }

  if ( triangle_num < triangle_max ) {
    Surface *tr = triangle_buffer + triangle_num;
#ifdef DEBUG
    if ( triangle_print_surface_if(& os, 38, -1, -1) ) fprintf(stderr, " (NEW)\n");
#endif
    *tr = os;
    tr->rs1 = 1;
    ++triangle_num;
    return 1;
  } else {
    err_msg = "Maximum number of triangles exceeded.";
    return 0;
  }
}

/* Adds all the surfaces of a tethraedra to the list of triangle surfaces */
int triangle_of_simplex_add(Simplex *s) {
  int b = s->body;

  Surface f[4] = {
    {b, {s->p[3], s->p[2], s->p[1]}, 0, s, s->left_handed},
    {b, {s->p[0], s->p[2], s->p[3]}, 0, s, s->left_handed},
    {b, {s->p[0], s->p[3], s->p[1]}, 0, s, s->left_handed},
    {b, {s->p[0], s->p[1], s->p[2]}, 0, s, s->left_handed}
  };
  return
      triangle_add(f)
   && triangle_add(f+1)
   && triangle_add(f+2)
   && triangle_add(f+3);
}

/* Returns the surface of the object whose tetrahedra were specified
 * with the function triangle_of_simplex_add.
 * EXAMPLE:
 *   int num_triangles;
 *   Surface *s;
 *   (void) triangle_surface(& s, & num_triangles);
 *  Here s is a pointer to an array of Surface, has been allocated with malloc
 *  and should be freed by the user with free!
 *  num_triangles is the size of this array.
 */
int triangle_surface(Surface **surface, int *num_triangles) {
  int i, j, nt = 0;
  Surface *s = NULL;

  *surface = s;
  *num_triangles = nt;

  for(i = 0; i < triangle_num; i++)
    nt += ( triangle_buffer[i].rs1 == 1 ) ? 1 : 0;

  s = (Surface *) xmalloc(sizeof(Surface) * nt);

  *surface = s;
  *num_triangles = nt;

  j = 0;
  for(i = 0; i < triangle_num; i++) {
    if ( triangle_buffer[i].rs1 == 1 ) {
      assert(j < nt);
      s[j++] = triangle_buffer[i];
    }
  }

  return 1;
}

/* Remove the surface simplices which were specified with the function
 * 'triangle_of_simplex_add'. After this function has been executed
 * the removed tetrahedra will belong to another object, the one with
 * number 'surface_body'.
 */
int triangle_surface_split(int surface_body) {
  int i, j;

  j = 0;
  for(i = 0; i < triangle_num; i++) {
    if ( triangle_buffer[i].rs1 == 1 ) {
      Simplex *parent_simplex = triangle_buffer[i].parent;
      if ( parent_simplex != NULL )
        parent_simplex->body = surface_body;
    }
  }
  return 1;
}

/*****************************************************************************
 *                        Reconstruction of surfaces                         *
 *****************************************************************************/

/* Extracts the surface for the given body:
 *  - sx is an array of simplices;
 *  - num_sx is the size of this array;
 *  - body is the body which will be considered;
 *  - surface, num_triangles contain the surface as described
 *     for the function triangle_surface (you should use free...)
 */
int body_surface(Simplex *sx, int num_sx, int body,
 Surface **surface, int *num_triangles) {
  int i, body_num_sx;

  body_num_sx = 0;
  for(i = 0; i < num_sx; i++)
    if ( sx[i].body == body ) ++body_num_sx;

  if ( ! triangle_list( 4*body_num_sx ) ) return 0;

  for(i = 0; i < num_sx; i++) {
    Simplex *cur = sx + i;
    if ( cur->body == body ) {
      if ( ! triangle_of_simplex_add(cur) ) return 0;
    }
  }

  return triangle_surface(surface, num_triangles);
}

/* Removes the superficial tetrahedra from 'body'.
 * Removed tetrahedra will become part of the body 'surface_body'.
 *  - sx is an array of simplices;
 *  - num_sx is the size of this array;
 *  - body is the body which will be "undressed";
 *  - surface_body is the body to which the removed tetrahedra will be added;
 */
int body_undress(Simplex *sx, int num_sx, int body, int surface_body) {
  int i, body_num_sx;

  body_num_sx = 0;
  for(i = 0; i < num_sx; i++)
    if ( sx[i].body == body ) ++body_num_sx;

  if ( ! triangle_list( 4*body_num_sx ) ) return 0;

  for(i = 0; i < num_sx; i++) {
    Simplex *cur = sx + i;
    if ( cur->body == body ) {
      if ( ! triangle_of_simplex_add(cur) ) return 0;
    }
  }

  return triangle_surface_split(surface_body);
}

int simplex_is_inside(Point *pnt, Simplex *s, IsInside cut_function) {
  return
      cut_function(pnt + s->p[0])
   && cut_function(pnt + s->p[1])
   && cut_function(pnt + s->p[2])
   && cut_function(pnt + s->p[3]);
}

/* This function cut the body 'body' and put the cutted simplices
 * inside the body 'cutted_body'.
 *  - pnt is the array of points to which the indices of simplices refer;
 *  - sx is an array of simplices;
 *  - num_sx is the size of this array;
 *  - body is the body which will be cutted;
 *  - surface_body is the cutted part of 'body';
 *  - cut_function is the function used to cut.
 */
int body_cut(Point *pnt, Simplex *sx, int num_sx, int body, int cutted_body,
 IsInside cut_function) {
  int i;
  for(i = 0; i < num_sx; i++)
    if ( sx[i].body == body ) {
      if ( ! simplex_is_inside(pnt, sx + i, cut_function) )
        sx[i].body = cutted_body;
    }
  return 1;
}

/*****************************************************************************
 *                                   Main                                    *
 *****************************************************************************/
int cut_positive_z(Point *p) {return (p->z >= 0.0);}

#define body_cut_positive_z(pnt, sx, num_sx, body, cutted_body) \
  body_cut(pnt, sx, num_sx, body, cutted_body, cut_positive_z)

/** This function converts a string into a signed integer value, checking
 * that it is greater than min.
 * @param str the source string
 * @param min the minimum value accepted
 * @param integer the converted result will be put into *integer
 */
int read_int(char *str, int min, int *integer) {
  char *other;
  int number;

  number = strtol(str, & other, 10);
  if (  errno == ERANGE || *other != '\0' ) {
    fprintf(stderr, "Error: '%s' <-- error reading the number\n", str);
    return 0;
  }

  if ( number <  min ) {
    fprintf(stderr, "Error: '%s' <-- invalid value\n", str);
    return 0;
  }
  *integer = number;
  return 1;
}

/** This function converts a string into a floating-point value (Real).
 * @param str the source string
 * @param real_num the converted result will be put into *real_num
 */
int read_real(char *str, Real *real_num) {
  char *other;
  Real number;

  number = strtod(str, & other);
  if (  errno == ERANGE || *other != '\0' ) {
    fprintf(stderr, "Error: '%s' <-- error reading the number\n", str);
    return 0;
  }
  *real_num = number;
  return 1;
}

int show_usage_and_exit(int exit_status) {
  fprintf(stderr,
   "\n%s, written by %s\n\n", SURF_VERSION, SURF_AUTHORS);

  fprintf(stderr,
   "surf loads a mesh written in the nmesh file format, manipulate it in various\n"
   "ways and finally writes its surface in VRML 2.0 format. Most VRML 97 reader\n"
   "should be able to read the obtained file without problem.\n\n"
   "BASIC USAGE: surf -i input.nmesh -o output.wrl\n\n"
   "ADVANCED USAGE: This is the list of options:\n"
   " -i file    input file containing the mesh (nmesh format)\n"
   " -o file    output VRML 2.0 file containing the surface\n"
   " -b number  selects the body 'number' (default: the \"last\")\n"
   " -s number  body whose surface will be saved\n"
   " -u number  Undress the mesh 'number' times\n"
   " -p number  Draw a sphere at node 'number'\n"
   );
  exit(exit_status);
}

int main (int argc, char **argv) {
  int success;
  Mesh m;
  int sf_size;
  Surface *sf;
  SelectedNodes *sn = NULL;

  int skin_body, save_body;
  int undress = 0, body, num_bodies, err;
  int body_selected = 0, save_selected = 0;
  char *file_i = NULL, *file_o = NULL;
  FILE *fi, *fo;

  int c, index, i;

  opterr = 0;

  while ((c = getopt(argc, argv, "i:o:u:b:p:s:?:h:")) != -1)
    switch (c) {
      case 'i':
        file_i = optarg;
        break;
      case 'o':
        file_o = optarg;
        break;
      case 'u':
        if ( ! read_int(optarg, 0, & undress) ) exit(EXIT_FAILURE);
        break;
      case 'b':
        if ( ! read_int(optarg, 0, & body) ) exit(EXIT_FAILURE);
        body_selected = 1;
        break;
      case 's':
        if ( ! read_int(optarg, 0, & save_body) ) exit(EXIT_FAILURE);
        save_selected = 1;
        break;
      case 'p':
        {
          int point_id;
          SelectedNodes *new = xmalloc(sizeof(SelectedNodes));
          if ( ! read_int(optarg, 0, & new->id) ) exit(EXIT_FAILURE);
          new->next = sn;
          sn = new;
        }
        break;
      case '?':
        if (optopt == '?' || optopt == 'h')
          show_usage_and_exit(EXIT_SUCCESS);
        /* here we proceed to the case 'default' */
      default:
        fprintf(stderr, "Option not recognized!\n");
        exit(EXIT_FAILURE);
    }

  for (index = optind; index < argc; index++)
    printf ("Warning: ignored non-option argument: '%s'\n", argv[index]);

  err = 0;
  if ( file_i == NULL ) {
    fprintf(stderr, "Error: output file not specified\n");
    err = 1;
  }
  if ( file_o == NULL ) {
    fprintf(stderr, "Error: input file not specified\n");
    err = 1;
  }
  if ( err ) show_usage_and_exit(EXIT_FAILURE);

  fi = fopen(file_i, "r");
  fo = fopen(file_o, "w");
  if (fi == NULL) {
    fprintf(stderr, "Error: Cannot open the input file for reading.\n");
    exit(EXIT_FAILURE);
  }
  if (fo == NULL) {
    fprintf(stderr, "Error: Cannot open the output file for writing.\n");
    exit(EXIT_FAILURE);
  }

  /* Here we read the mesh */
  if ( ! mesh_read(fi, & m) ) {
    fprintf(stderr, "Error (line %d): %s\n", nl, err_msg);
    exit(EXIT_FAILURE);
  }
  (void) fclose(fi);

#ifdef HASHTABLE
  /* Clean the mesh: remove duplicate points */
  (void) mesh_cleanup(& m);
#endif

  num_bodies = m.max_body - m.min_body + 1;
  printf("Number of bodies (max - min) = %d\n", num_bodies);

  if ( ! body_selected )
    body = m.max_body; /* Operate on the "last body" */
  if ( ! save_selected )
    save_body = body; /* Operate on the "last body" */

  skin_body = body + 1;

  for(i = 0; i < undress; i++) {
    printf("Undressing body %d: skin --> body %d, interior --> body %d\n",
     body, skin_body, body);
    if ( ! body_undress(m.simplex_arr, m.num_simplices, body, skin_body) ) {
      fprintf(stderr, "Error during surface undressing: %s\n", err_msg);
      exit(EXIT_FAILURE);
    }
  }

  printf("Saving body %d\n", save_body);
  success = body_surface(m.simplex_arr, m.num_simplices,
   save_body, & sf, & sf_size);
  if ( ! success ) {
    fprintf(stderr, "Error during surface reconstruction: %s\n", err_msg);
    exit(EXIT_FAILURE);
  }

  vrml_write(fo, m.num_nodes, m.point_arr, sf, sf_size);
  free(sf);

  /* Draw spheres at specified points */
  {
    SelectedNodes *run;
    for(run = sn; run != NULL; run = run->next) {
      if ( run->id >= 0 && run->id < m.num_nodes ) {
        Point *p = & m.point_arr[run->id];
        vrml_sphere(fo, 0.1, p->x, p->y, p->z);
      } else {
        fprintf(stderr, "Warning: the point id (%d) is out of bounds (0..%d)\n",
         run->id, m.num_nodes-1);
      }
    }
  }

  (void) fclose(fo);
  exit(EXIT_SUCCESS);
}

#if 0
  /*
  (void) body_cut_positive_z(m.point_arr, m.simplex_arr, m.num_simplices, 0, 2);
  */

  success = body_surface(m.simplex_arr, m.num_simplices, 1, & sf, & sf_size);
  if ( ! success ) {
    fprintf(stderr, "Error during surface reconstruction: %s\n", err_msg);
    return EXIT_FAILURE;
  }

/* vrml_write(stdout, m.num_nodes, m.point_arr, m.surface_arr, m.num_surfaces); */
#endif
