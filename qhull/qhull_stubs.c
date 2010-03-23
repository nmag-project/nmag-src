/* 
   (C) 2005 Dr. Thomas Fischbacher

   Do Delaunay triangulations from OCaml via an interface to qhull.

   Note: Qhull was never written with the intention of providing a
   shared object library as a primary interface to this functionality
   (and one can tell this from the library's interface design!)
   Rather, the preferred way to use qhull is to exec the programs
   provided as Unix processes. This, however, is horrendously wasteful
   in terms of the overhead involved.
   
   Notes about the interface:

   - The examples use the function qh_new_qhull().
     The implementation of that function is given in
     src/user.c.htm. One problem is that it uses
     functions like qh_meminit which take as an argument
     a FILE* filehandle where they report errors to - 
     a very unwieldy way of doing error handling...
     (By the way, qh_meminit will just crash in case of an error.
     Very nice behaviour, thank you.)

   - Qhull design uses global variables throughout, and provides
     qh_save_qhull() and similar functions to save away the global
     state.  Anyway, this means there is pretty much no point in trying
     to use more than one convex hull at a time.

   - Qhull checks for sizeof(int)==sizeof(void*). If this test fails,
     it just refuses to work.

   - Unfortunately, the whole thing also is quite big. So,
     re-implementing all of its functionality is not that easy.

   THAT BASTARD LIBRARY HAS TO BE SUNK INTO A DEEP WET GRAVE AT SOME FULL MOON!

   Ok, enough the furious comments.
   Let us see how to make the best out of it with reasonable effort.

 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/alloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <qhull/qhull.h>
#include <qhull/qset.h>
#include <qhull/user.h>

/* #define QHULL_FLAGS "qhull s d Tcv" */

/* Note: we use joggled, not triangulated output to deal with
   cocircularity issues.
*/
/* #define QHULL_FLAGS "qhull QJ s d Tcv" */

/* Note: jsut for now, we try Qt instead... */
#define QHULL_FLAGS "qhull Qt s d Tcv"

static FILE *eat_this=0;

CAMLprim value caml_qhull_delaunay(value points)
{
  CAMLparam1(points);

  coordT *qhull_points;
  int nr_points,dim;
  
  CAMLlocal3(the_point,ret,simplex);
  /* 
     If I read byterun/memory.h correctly, then the purpose of
     CAMLparam/CAMLlocal macros is to make GC roots in C code known.
     I do not quite know what triggers GC (timer signals, or consing a
     certain amount of memory, or both?), and this may be subject to
     change, so even if things "just worked right" ignoring CAMLlocal,
     this may not stay so with future versions of the OCaml compiler.
     
     Hence, I think I have to register any C-local "value" variable
     via CAMLlocal, even if we do not cons them ourselves!
  */

  if(eat_this==0)
    {
      if(0==(eat_this=fopen("/dev/null", "w")))
	{
	  perror("Failure to open /dev/null for writing");
	  exit(1);
	}
      
    }

  nr_points=Wosize_val(points);
  /* We should check that nr_points >0,
     but it is a bit awkward to do here
     and raise an exception in C (even though we could).

     Furthermore, there is little point in trying to be cleaner and
     better than libqhull here. So, we leave all consistency checking
     to a Caml wrapper of this stub function, and just exit on problems.
   */
  
  if(nr_points==0)exit(1);

  the_point=Field(points,0);

  /* points are double-float arrays, for which OCaml provides a direct
     representation. See byterun/mlvalues.h
  */

  dim=Wosize_val(the_point)/Double_wosize;

  /* fprintf(stderr,"NR POINTS=%d DIM=%d\n",nr_points,dim); / * DDD */
  
  /* As qhull projects our points onto a parabola, we have to go to
     dimension dim+1.

     It is a bit hard to make sense of the example programs
     user_eg.c/user_eg2.c provided with qhull, my impression being
     that, maybe, user_eg.c may reserve more memory for points than
     strictly necessary. Indeed, replacing DIM+1 by DIM and enclosing
     the array in question between canary1=0xdeadbeef/canary2=0xdeadbeef
     shows no signs of corruption in the canary words.
  */
  
  if(0==(qhull_points=malloc(sizeof(coordT)*dim*nr_points)))
    {
      fprintf(stderr,"malloc() failed!\n");
      exit(1);
    }

  {
    int p,c;
    int ix=0;
    for(p=0;p<nr_points;p++)
      {
	the_point=Field(points,p);
	for(c=0;c<dim;c++)
	  {
	    qhull_points[ix++]=Double_field(the_point,c);
	    /* fprintf(stderr,"P %3d.%d: %f\n", p+1,c+1,Double_field(the_point,c+1)); / * DDD */
	  }
      }
  }

  {
    int qhull_exitcode;
    qhull_exitcode=
      qh_new_qhull(dim,nr_points,qhull_points,0,
		   QHULL_FLAGS,
		   /* stdout,stderr */
		   eat_this,eat_this
		   );

    if(qhull_exitcode)
      {
	fprintf(stderr,"Qhull failure: nr_points=%d exitcode=%d!\n", nr_points, qhull_exitcode);
	exit(1);
      }
  }
  /* By now, we have the facets. First problem: we do not know how many of them there are. */
  {				
    facetT * facet;
    /* Note 1: the FORALLfacets macro is dirty and relies on the name "facet".

       Note 2: "dirtiness" of macros is a technical term from Lisp parlance.
       But what's REALLY dirty about this dirty macro is that it is unnecessarily dirty:

       #define FORALLfacets for (facet=qh facet_list;facet && facet->next;facet=facet->next)

       More shame:

       #if qh_QHpointer
       #define qh qh_qh->
       extern qhT *qh_qh;     / * allocated in global.c * /
       #else
       #define qh qh_qh.
         extern qhT qh_qh;
       #endif
     */
	   
    vertexT *vertex, **vertexp;

    int nr_facets=0,nr_this_facet=0;
    int nr_vertices;

    /* ...and the documentation says:

===
	Here some unchecked code to print the point indices of each Delaunay  
	triangle. Use option 'QJ' if you want to avoid non-simplicial facets.
	Note that upper Delaunay regions are skipped. These facets correspond
	to the furthest-site Delaunay triangulation.

 facetT *facet;
  vertexT *vertex, **vertexp;

  FORALLfacets {
    if (!facet->upperdelaunay) {
      printf ("%d", qh_setsize (facet->vertices);
      FOREACHvertex_(facet->vertices)
        printf (" %d", qh_pointid (vertex->point));
      printf ("\n");
    }
  }
===

        Note that it does not even compile, due to a syntax error...

    */

    FORALLfacets
      {
	if(!facet->upperdelaunay)nr_facets++;
      }

    ret=alloc(nr_facets,0);

    FORALLfacets
      {
	if(!facet->upperdelaunay)
	  {
	    int ix;
	    int point_id;
	    /* qh_setsize does not set a size - it should rather
	       be called qh_sizeof_set, I think... */
	    nr_vertices=qh_setsize(facet->vertices);

	    ix=0;
	    simplex=alloc(nr_vertices,0);

	    FOREACHvertex_(facet->vertices)
	      {
		point_id=qh_pointid(vertex->point);
		Store_field(simplex,ix++,Val_int(point_id));
	      }
	    Store_field(ret,nr_this_facet++,simplex);
	  }
      }
  }

  {
    int curlong, totlong;
    
    qh_freeqhull (!qh_ALL);
    qh_memfreeshort (&curlong, &totlong);
    if(curlong || totlong)
      {
	fprintf(stderr, "Qhull memory leak: Did not free %d bytes (%d pieces)\n",
		totlong,curlong);
      }
  }
  
  free(qhull_points);
  
  CAMLreturn(ret);
}

