/* This code is an adaption of Dmitry Grebeniuk's "objsize" extension
   with very minor modifications by Thomas Fischbacher (i.e. basically 
   turning non-static symbols into static ones and merging all C code
   into a single file.)
 */

#define WITH_MEMORY_FOOTPRINT 0

#if WITH_MEMORY_FOOTPRINT == 1

/*
#define DBG(x) do { x; fflush(stdout); } while(0)
*/
#define DBG(x) do{}while(0)

#define DUMP 0

#define PRF(x) bitarray##x

#include <caml/config.h>
#include "ocamlsrc/byterun/misc.h"


#include <stdlib.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include "ocamlsrc/byterun/minor_gc.h"
#include "ocamlsrc/byterun/major_gc.h"
#include "ocamlsrc/byterun/gc.h"

#define ABORT(x) do { \
  fprintf(stderr, "aborted at %s:%i: %s\n", __FILE__, __LINE__, x); \
  exit(1); } while(0)

#define ASSERT(b, err) do { \
 if (!(b)) \
  { ABORT("assert_failed: " err); \
  }; \
 } while(0)


#define Col_white (Caml_white >> 8)
#define Col_gray  (Caml_gray >> 8)
#define Col_blue  (Caml_blue >> 8)
#define Col_black (Caml_black >> 8)


#define COLORS_INIT_COUNT 256

#define BITS_OF_CHAR 8

#define ALPRF(x) bitarrayalloc##x

#define ALLOC_TYPE unsigned char
#define ALLOC_PRF(x) ALPRF(x)

static ALLOC_TYPE* ALLOC_PRF(_alloc)(size_t count)
{
  return malloc(count * sizeof(ALLOC_TYPE));
}

static void ALLOC_PRF(_free)(ALLOC_TYPE* arr)
{
  free(arr);
}

static ALLOC_TYPE* ALLOC_PRF(_realloc)(ALLOC_TYPE* arr, size_t count)
{
  size_t newsz = count * sizeof(ALLOC_TYPE);
  ALLOC_TYPE* newarr = realloc(arr, newsz);
  if (count != 0 && newarr == NULL)
    {
      static char msg[128];
      sprintf
	( msg
	  , "realloc(%p, %u*%u=%u) failed: to provide an alternative behaviour."
	  , arr, (unsigned int) count, (unsigned int) sizeof(ALLOC_TYPE)
	  , (unsigned int) newsz
	  );
      ABORT(msg);
    };
  return newarr;
}


#undef ALLOC_PRF
#undef ALLOC_TYPE

static size_t wordalign(size_t n)
{
  size_t al = sizeof(int);
  size_t m = al % n;
  if (m == 0)
    {
      return n;
    }
  else
    {
      return n + al - m;
    }
}


static size_t PRF(_sizebytes)(size_t n)
{
  return wordalign(n/BITS_OF_CHAR);
}


static unsigned char* PRF(_alloc)(size_t count)
{
  return ALPRF(_alloc)(PRF(_sizebytes)(count));
}


static void PRF(_free)(unsigned char* arr)
{
  ALPRF(_free)(arr);
}


static unsigned char* PRF(_realloc)(unsigned char* arr, size_t newcount)
{
  return ALPRF(_realloc)(arr, PRF(_sizebytes)(newcount));
}


#define LVAL(arr, i) ((arr)[(i)/BITS_OF_CHAR])
#define MASK(i) (1<<((i)%BITS_OF_CHAR))

static int PRF(_get)(unsigned char arr[], size_t i)
{
  return ((LVAL(arr,i) & MASK(i)) ? 1 : 0);
}

static void PRF(_set)(unsigned char arr[], size_t i, int val)
{
  unsigned char mask = MASK(i);
  if (val)
    {
      LVAL(arr,i) |= mask;
    }
  else
    {
      LVAL(arr,i) &= ~mask;
    }
  return;
}

static void PRF(_init)(unsigned char arr[], size_t sz, int val)
{
  size_t bytesize = sz/BITS_OF_CHAR;
  size_t i;
  unsigned char valbyte = val ? (-1) : 0;
  for (i=0; i<bytesize; ++i)
    {
      arr[i] = valbyte;
    };
  i *= BITS_OF_CHAR;
  while(i < sz)
    {
      PRF(_set)(arr, i, val);
      ++i;
    };
  return;
}


#undef MASK
#undef LVAL
#undef PRF


unsigned char* colors = NULL;
size_t colors_bitcap = 0;
size_t colors_writeindex = 0;
size_t colors_readindex = 0;


static void colors_init(void)
{
  ASSERT(colors==NULL, "colors_init");
  colors_bitcap = COLORS_INIT_COUNT*2;
  colors = bitarray_alloc(colors_bitcap);
  colors_writeindex = 0;
  colors_readindex = 0;
  return;
}


static void colors_deinit(void)
{
  bitarray_free(colors);
  colors = NULL;
  return;
}


static void writebit(int bit)
{
  if (colors_writeindex == colors_bitcap)
    {
      size_t colors_new_bitcap = colors_bitcap * 2;
      unsigned char* newarr = bitarray_realloc(colors, colors_new_bitcap);
      ASSERT(newarr != NULL, "realloc");
      colors = newarr;
      colors_bitcap = colors_new_bitcap;
    };
  ASSERT(colors_writeindex < colors_bitcap, "bound on write");
  bitarray_set(colors, colors_writeindex++, bit);
  return;
}


static int readbit(void)
{
  int res;
  ASSERT(colors_readindex < colors_writeindex, "bound on read");
  res = bitarray_get(colors, colors_readindex++);
  ASSERT(res == 0 || res == 1, "bitarray_get");
  return res;
}


static void writeint(unsigned int arg, unsigned int width)
{
  while(width-- > 0)
    {
      writebit(arg&1);
      arg >>= 1;
    };
  ASSERT(arg == 0, "writeint");
  return;
}


static unsigned int readint(unsigned int width)
{
  unsigned int acc = 0;
  unsigned int hibit = 1 << (width-1);
  ASSERT(width > 0, "readint width");
  while(width-- > 0)
    {
      int bit = readbit();
      acc >>= 1;
      if (bit) acc |= hibit;
    };
  return acc;
}


static int prev_color = 0;
static int repeat_count = 0;

#define BITS_FOR_COUNT 5
#define BITS_FOR_ORDER 4

#define MAX_REPEAT_COUNT (1<<BITS_FOR_COUNT)
#define MAX_REPEAT_ORDER (1<<BITS_FOR_ORDER)

static void rle_write_repeats(void)
{
  while(repeat_count >= MAX_REPEAT_COUNT)
    {
      unsigned int ord = 0;
      
      while(ord < MAX_REPEAT_ORDER-1 && (1<<ord) <= repeat_count/2)
	{
	  ++ord;
	};
      
      writeint(Col_blue, 2);
      writeint(1, 1);
      ASSERT((1<<ord) != 0, "write_repeats#2");
      writeint(ord, BITS_FOR_ORDER);
      repeat_count -= (1 << ord);
    };
  
  ASSERT(repeat_count < MAX_REPEAT_COUNT, "write_repeats");
  
  if (repeat_count > 0)
    {
      writeint(Col_blue, 2);
      writeint(0, 1);
      writeint(repeat_count, BITS_FOR_COUNT);
      repeat_count = 0;
    };
  
  return;
}


static void rle_write_flush(void)
{
  if (repeat_count > 0)
    {
      rle_write_repeats();
    };
  ASSERT(repeat_count == 0, "rle_write_flush");
  return;
}


static void rle_read_flush(void)
{
  DBG(printf("rle_read_flush: repeat_count=%i, ri=%i, wi=%i\n",
	     repeat_count, colors_readindex, colors_writeindex)
      );
  
  ASSERT
    ( repeat_count == 0
      && colors_readindex == colors_writeindex
      , "rle_reader_flush"
      );
  return;
}


static void rle_write(int color)
{
  if (prev_color == color)
    {
      ++repeat_count;
    }
  else
    {
      rle_write_flush();
      ASSERT(color != Col_blue, "rle_write");
      writeint(color, 2);
      prev_color = color;
    };
}


static int rle_read(void)
{
  if (repeat_count > 0)
    {
      --repeat_count;
      return prev_color;
    }
  else
    {
      int c = readint(2);
      if (c == Col_blue)
	{
	  int rk = readint(1);
	  if (rk == 0)
	    { repeat_count = readint(BITS_FOR_COUNT); }
	  else
	    { repeat_count = 1 << readint(BITS_FOR_ORDER); };
	  ASSERT(repeat_count > 0, "rle_read");
	  return rle_read();
	}
      else
	{
	  prev_color = c;
	  return c;
	};
    };
}


static void rle_init(void)
{
  prev_color = 0;
  repeat_count = 0;
  return;
}



static void writecolor(int col)
{
  ASSERT(col >= 0 && col <= 3 && col != Col_blue, "writecolor");
  rle_write(col);
  return;
}


static int readcolor(void)
{
  int res = rle_read();
  ASSERT(res >= 0 && res <= 3 && res != Col_blue, "readcolor");
  return res;
}


static size_t acc_hdrs;
static size_t acc_data;
static size_t acc_depth;


static void c_rec_objsize(value v, size_t depth)
{
  DBG(printf("c_rec_objsize: v=%p b=%i y=%i h=%i\n"
	     , (void*)v, Is_block(v), Is_young(v), Is_in_heap(v))
      );
  
  if (Is_block(v)
      && (Is_young(v) || Is_in_heap(v))
      && Colornum_hd(Hd_val(v)) != Col_blue
      )
    {
      size_t sz = Wosize_val(v);
      int col;
      header_t hd;
      
      acc_data += sz;
      ++acc_hdrs;
      if (depth > acc_depth) { acc_depth = depth; };
      
      hd = Hd_val(v);
      col = Colornum_hd(hd);
      writecolor(col);
      
      DBG(printf("COL: w %08lx %i\n", v, col));
      
      Hd_val(v) = Coloredhd_hd(hd, Col_blue);
      
      if (Tag_val(v) < No_scan_tag)
	{
	  size_t i;
	  for (i=0; i<sz; ++i)
	    {
	      /*acc +=*/ c_rec_objsize(Field(v,i), (depth+1));
	    };
	}; /* (Tag_val(v) < No_scan_tag) */
    };
  
  return /*acc*/ ;
}


static void restore_colors(value v)
{
  if (Is_block(v)
      && (Is_young(v) || Is_in_heap(v))
      && Colornum_hd(Hd_val(v)) == Col_blue
      )
    {
      int col = readcolor();
      DBG(printf("COL: r %08lx %i\n", v, col));
      Hd_val(v) = Coloredhd_hd(Hd_val(v), col);
      
      if (Tag_val(v) < No_scan_tag)
	{
	  size_t sz = Wosize_val(v);
	  size_t i;
	  for (i=0; i<sz; ++i)
	    {
	      restore_colors(Field(v,i));
	    };
	};
    };
  return;
}


static void c_objsize(value v, size_t* headers, size_t* data, size_t* depth)
{
  colors_init();
  rle_init();
  DBG(printf("young heap from %p to %p\n", caml_young_start, caml_young_end));
  DBG(printf("old heap from %p to %p\n", caml_heap_start, caml_heap_end));
  DBG(printf("COL writing\n"));
  
  acc_data = 0;
  acc_hdrs = 0;
  acc_depth = 0;
  c_rec_objsize(v, 0);
  *headers = acc_hdrs;
  *data = acc_data;
  *depth = acc_depth;
  
  rle_write_flush();
  DBG(printf("COL reading\n"));
  rle_init();
  restore_colors(v);
  rle_read_flush();
  
#if DUMP
  printf("objsize: bytes for rle data = %i\n", colors_readindex/8);
  fflush(stdout);
  
  {
    FILE* f = fopen("colors-dump", "w");
    fwrite(colors, 1, colors_readindex/8, f);
    fclose(f);
  };
#endif
  
  colors_deinit();
  DBG(printf("c_objsize done.\n"));
  
  return;
}


CAMLprim value caml_memory_footprint(value start)
{
  CAMLparam1(start);
  CAMLlocal1(res);
  size_t hdrs, data, depth;
  
  c_objsize(start, &hdrs, &data, &depth);
  
  res = caml_alloc_tuple(3);
  Store_field(res, 0, copy_double(sizeof(value)*(double)data));
  Store_field(res, 1, copy_double(sizeof(value)*(double)hdrs));
  Store_field(res, 2, copy_double((double)depth));
  CAMLreturn(res);
}

#else /* if WITH_MEMORY_FOOTPRINT == 1 */

#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value caml_memory_footprint(value start) {
      CAMLparam1(start);
      CAMLlocal1(res);
      size_t s = 0;

      res = caml_alloc_tuple(3);
      Store_field(res, 0, copy_double(s));
      Store_field(res, 1, copy_double(s));
      Store_field(res, 2, copy_double(s));
      CAMLreturn(res);
}

#endif /* if WITH_MEMORY_FOOTPRINT == 1 */

