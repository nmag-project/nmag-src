#ifndef _MT19937_H
#define _MT19937_H

#include <stdlib.h>

typedef struct {
  /* Note: the value 624 is the #define parameter N in mt19937.c.
     624 32-bit words make just enough words to store 19937 bits.
     
     Note 2: we changed this to unsigned int (was: unsigned long).
     Rationale: on 64-bit machines, "long" is 64 bits (LC64).
     But here, we actually need 32-bit words.
     (We just assume int is 32 bits - it's true for all
     practical situations...)
   */
  unsigned int mt[624];
  int mti;
} mt19937_rng;


typedef void *mt19937_malloc_fun_t(size_t);

extern mt19937_rng *mt19937_new_rng(mt19937_malloc_fun_t *);

extern void mt19937_sgenrand(mt19937_rng *, unsigned int);
extern void mt19937_lsgenrand(mt19937_rng *,unsigned int *);
double mt19937_genrand_double(mt19937_rng *);
unsigned int mt19937_genrand_uint(mt19937_rng *);

#endif
