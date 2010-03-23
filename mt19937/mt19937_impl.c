/* Very minor modifications have been made by Thomas Fischbacher:

   (1) Syntactic cosmetics to achieve ANSI C Compliance
   (2) Some internal functions are declared "static"
   (3) removed main(), so this now is a library.

   (4) put global variables into a context structure and modified
       functions to take such a random number generator context.
       (Thus, we can use more than one RNG in a program, e.g. to get
       well-defined behaviour in a multithreaded application)

 */


/* A C-program for MT19937: Real number version([0,1)-interval) */
/* (1999/10/28)                                                 */
/*   genrand() generates one pseudorandom real number (double)  */
/* which is uniformly distributed on [0,1)-interval, for each   */
/* call. sgenrand(seed) sets initial values to the working area */
/* of 624 words. Before genrand(), sgenrand(seed) must be       */
/* called once. (seed is any 32-bit integer.)                   */
/* Integer generator is obtained by modifying two lines.        */
/*   Coded by Takuji Nishimura, considering the suggestions by  */
/* Topher Cooper and Marc Rieffel in July-Aug. 1997.            */

/* This library is free software; you can redistribute it and/or   */
/* modify it under the terms of the GNU Library General Public     */
/* License as published by the Free Software Foundation; either    */
/* version 2 of the License, or (at your option) any later         */
/* version.                                                        */
/* This library is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of  */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            */
/* See the GNU Library General Public License for more details.    */
/* You should have received a copy of the GNU Library General      */
/* Public License along with this library; if not, write to the    */
/* Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA   */ 
/* 02111-1307  USA                                                 */

/* Copyright (C) 1997, 1999 Makoto Matsumoto and Takuji Nishimura. */
/* Any feedback is very welcome. For any question, comments,       */
/* see http://www.math.keio.ac.jp/matumoto/emt.html or email       */
/* matumoto@math.keio.ac.jp                                        */

/* REFERENCE                                                       */
/* M. Matsumoto and T. Nishimura,                                  */
/* "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform  */
/* Pseudo-Random Number Generator",                                */
/* ACM Transactions on Modeling and Computer Simulation,           */
/* Vol. 8, No. 1, January 1998, pp 3--30.                          */

#include <stdio.h>
#include "mt19937.h"

/* Period parameters */  
#define N 624
#define M 397
#define MATRIX_A 0x9908b0df   /* constant vector a */
#define UPPER_MASK 0x80000000 /* most significant w-r bits */
#define LOWER_MASK 0x7fffffff /* least significant r bits */

/* Tempering parameters */   
#define TEMPERING_MASK_B 0x9d2c5680
#define TEMPERING_MASK_C 0xefc60000
#define TEMPERING_SHIFT_U(y)  (y >> 11)
#define TEMPERING_SHIFT_S(y)  (y << 7)
#define TEMPERING_SHIFT_T(y)  (y << 15)
#define TEMPERING_SHIFT_L(y)  (y >> 18)

mt19937_rng *mt19937_new_rng(mt19937_malloc_fun_t *allocator)
{
  mt19937_rng *ctx;
  if(0==(ctx=(mt19937_rng *)((*allocator)(sizeof(mt19937_rng)))))
    {
      fprintf(stderr,"memory allocator failure!");
      exit(1);
    }

  ctx->mti=N+1; /* means: RNG is not initialized */
}


/* Initializing the array with a seed */
void mt19937_sgenrand(mt19937_rng *ctx, unsigned int seed)
{
    int i;

    for (i=0;i<N;i++) {
         ctx->mt[i] = seed & 0xffff0000;
         seed = 69069 * seed + 1;
         ctx->mt[i] |= (seed & 0xffff0000) >> 16;
         seed = 69069 * seed + 1;
    }
    ctx->mti = N;
}

/* Initialization by "sgenrand()" is an example. Theoretically,      */
/* there are 2^19937-1 possible states as an intial state.           */
/* This function allows to choose any of 2^19937-1 ones.             */
/* Essential bits in "seed_array[]" is following 19937 bits:         */
/*  (seed_array[0]&UPPER_MASK), seed_array[1], ..., seed_array[N-1]. */
/* (seed_array[0]&LOWER_MASK) is discarded.                          */ 
/* Theoretically,                                                    */
/*  (seed_array[0]&UPPER_MASK), seed_array[1], ..., seed_array[N-1]  */
/* can take any values except all zeros.                             */

void mt19937_lsgenrand(mt19937_rng *ctx,unsigned int *seed_array)
/* the length of seed_array[] must be at least N */
{
    int i;

    for (i=0;i<N;i++) 
      ctx->mt[i] = seed_array[i];
    ctx->mti=N;
}

double mt19937_genrand_double(mt19937_rng *ctx)
{
  unsigned int y;
  unsigned int *mt=ctx->mt;

  static unsigned int mag01[2]={0x0, MATRIX_A};
  /* mag01[x] = x * MATRIX_A  for x=0,1 */
  
  if (ctx->mti >= N) { /* generate N words at one time */
    int kk;

    if (ctx->mti == N+1)   /* if sgenrand() has not been called, */
      mt19937_sgenrand(ctx,4357); /* a default initial seed is used   */

    for (kk=0;kk<N-M;kk++) {
      y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

        ctx->mti = 0;
  }
  
  y = mt[ctx->mti++];
  y ^= TEMPERING_SHIFT_U(y);
  y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
  y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
  y ^= TEMPERING_SHIFT_L(y);
  
  return ( (double)y * 2.3283064365386963e-10 ); /* reals: [0,1)-interval */
  /* return y; */ /* for integer generation */
}

unsigned int mt19937_genrand_uint(mt19937_rng *ctx)
{
    unsigned int y;
    unsigned int *mt=ctx->mt;

    static unsigned int mag01[2]={0x0, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (ctx->mti >= N) { /* generate N words at one time */
        int kk;

        if (ctx->mti == N+1)   /* if sgenrand() has not been called, */
	  mt19937_sgenrand(ctx,4357); /* a default initial seed is used   */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

        ctx->mti = 0;
    }
  
    y = mt[ctx->mti++];
    y ^= TEMPERING_SHIFT_U(y);
    y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
    y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
    y ^= TEMPERING_SHIFT_L(y);

    return y;
}

