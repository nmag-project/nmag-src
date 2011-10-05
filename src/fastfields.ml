(* (C) 2006 Dr. Thomas Fischbacher *)

(* Main problems discovered while working with the present fastfields module:

   - auto-expiring C code did encounter some GC problems. These
     presumably were related to broken memory management in pycaml and
     should be resolved now.

   - What does not work very well at all is how we deal with broken C
     code.  Actually, it would be much more reasonable if every C
     function went into an own compilation unit (file), and if we
     compiled the whole bunch of C files into one shared object.

     This way, we would have the option to parse the compiler's error messages,
     and, should an error occur in module X, replace the corresponding function
     by one that raises a Failure "C Compiler error: <compiler message>",
     then re-compile.

     In addition, such a scheme would get rid of all the nasty #undefs, and
     cross-influencing #defines inside functions, and we may also allow the user
     to register his own prelude (e.g. additional helper functions/definitions
     declared file-static) to this module. Next step then would be to think about
     function groups (but this would bring us back the #undefs!), i.e. a bunch of
     functions can be declared to go into the same compilation unit - if one fails,
     all fail, but we have an opportunity to re-use static helper functions.


   So, in principle, what we have here is a nice and very useful module, but
   its error handling and flexibility could be improved greatly.
*)

open Bigarray;;

exception Fastfields_eval_problem;;

type c_funptr;;
type dl_handle;;

type c_field = string * string * (c_funptr array);;
(* symbol, code, c pointer *)
(* Exported as an opaque type. Note that the array has length 1,
   and will just store a single pointer.
 *)

type c_manipulator = string * string * (c_funptr array);;
(* Basically, this is just the same, but with double** arguments that
   are supposed to be used ONLY through special #define macros!
 *)

let debug=false;;

let _cc_debug_handle = ref (None:(out_channel option));;

let opt_directory=ref "/tmp/ocaml-fastfields.XXXXXX";;

let the_directory=ref None;;
 (* Also serves to check if we are initialized *)

let the_lib_handle = ref None;;

let need_relink = ref false;;
 (* Will be set once we define our first function *)

let opt_cc = ref "gcc";;
let opt_cc_opts = ref "-O3 -fomit-frame-pointer";;
let opt_includes = ref ["#include <stdio.h>\n";"#include <math.h>\n#include <stdlib.h>\n"];;

let own_header = ref 
  [
    "

#include <math.h>

/* MD5C.C - RSA Data Security, Inc., MD5 message-digest algorithm
 */

/* Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.

License to copy and use this software is granted provided that it
is identified as the \"RSA Data Security, Inc. MD5 Message-Digest
Algorithm\" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as \"derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm\" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided \"as is\"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software.
 */

/* GLOBAL.H - RSAREF types and constants
 */

/* PROTOTYPES should be set to one if and only if the compiler supports
  function argument prototyping.
The following makes PROTOTYPES default to 0 if it has not already

  been defined with C compiler flags.
 */
#ifndef PROTOTYPES
#define PROTOTYPES 0
#endif

/* POINTER defines a generic pointer type */
typedef unsigned char *POINTER;

/* UINT2 defines a two byte word */
typedef unsigned short int UINT2;

/* UINT4 defines a four byte word */
typedef unsigned int UINT4;

/* PROTO_LIST is defined depending on how PROTOTYPES is defined above.
If using PROTOTYPES, then PROTO_LIST returns the list, otherwise it
  returns an empty list.
 */
#if PROTOTYPES
#define PROTO_LIST(list) list
#else
#define PROTO_LIST(list) ()
#endif


/* MD5.H - header file for MD5C.C
 */

/* Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.

License to copy and use this software is granted provided that it
is identified as the \"RSA Data Security, Inc. MD5 Message-Digest
Algorithm\" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as \"derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm\" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided \"as is\"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software.
 */

/* MD5 context. */
typedef struct {
  UINT4 state[4];                                   /* state (ABCD) */
  UINT4 count[2];        /* number of bits, modulo 2^64 (lsb first) */
  unsigned char buffer[64];                         /* input buffer */
} MD5_CTX;

void MD5Init PROTO_LIST ((MD5_CTX *));
void MD5Update PROTO_LIST
  ((MD5_CTX *, unsigned char *, unsigned int));
void MD5Final PROTO_LIST ((unsigned char [16], MD5_CTX *));


/* Constants for MD5Transform routine.
 */

#define S11 7
#define S12 12
#define S13 17
#define S14 22
#define S21 5
#define S22 9
#define S23 14
#define S24 20
#define S31 4
#define S32 11
#define S33 16
#define S34 23
#define S41 6
#define S42 10
#define S43 15
#define S44 21

static void MD5Transform PROTO_LIST ((UINT4 [4], unsigned char [64]));
static void Encode PROTO_LIST
  ((unsigned char *, UINT4 *, unsigned int));
static void Decode PROTO_LIST
  ((UINT4 *, unsigned char *, unsigned int));
static void MD5_memcpy PROTO_LIST ((POINTER, POINTER, unsigned int));
static void MD5_memset PROTO_LIST ((POINTER, int, unsigned int));

static unsigned char PADDING[64] = {
  0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/* F, G, H and I are basic MD5 functions.
 */
#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

/* ROTATE_LEFT rotates x left n bits.
 */
#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32-(n))))

/* FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
Rotation is separate from addition to prevent recomputation.
 */
#define FF(a, b, c, d, x, s, ac) { \\
 (a) += F ((b), (c), (d)) + (x) + (UINT4)(ac); \\
 (a) = ROTATE_LEFT ((a), (s)); \\
 (a) += (b); \\
  }
#define GG(a, b, c, d, x, s, ac) { \\
 (a) += G ((b), (c), (d)) + (x) + (UINT4)(ac); \\
 (a) = ROTATE_LEFT ((a), (s)); \\
 (a) += (b); \\
  }
#define HH(a, b, c, d, x, s, ac) { \\
 (a) += H ((b), (c), (d)) + (x) + (UINT4)(ac); \\
 (a) = ROTATE_LEFT ((a), (s)); \\
 (a) += (b); \\
  }
#define II(a, b, c, d, x, s, ac) { \\
 (a) += I ((b), (c), (d)) + (x) + (UINT4)(ac); \\
 (a) = ROTATE_LEFT ((a), (s)); \\
 (a) += (b); \\
  }

/* MD5 initialization. Begins an MD5 operation, writing a new context.
 */
void MD5Init (context)
MD5_CTX *context;                                        /* context */
{
  context->count[0] = context->count[1] = 0;
  /* Load magic initialization constants.
*/
  context->state[0] = 0x67452301;
  context->state[1] = 0xefcdab89;
  context->state[2] = 0x98badcfe;
  context->state[3] = 0x10325476;
}

/* MD5 block update operation. Continues an MD5 message-digest
  operation, processing another message block, and updating the
  context.
 */
void MD5Update (context, input, inputLen)
MD5_CTX *context;                                        /* context */
unsigned char *input;                                /* input block */
unsigned int inputLen;                     /* length of input block */
{
  unsigned int i, index, partLen;

  /* Compute number of bytes mod 64 */
  index = (unsigned int)((context->count[0] >> 3) & 0x3F);

  /* Update number of bits */
  if ((context->count[0] += ((UINT4)inputLen << 3))

   < ((UINT4)inputLen << 3))
 context->count[1]++;
  context->count[1] += ((UINT4)inputLen >> 29);

  partLen = 64 - index;

  /* Transform as many times as possible.
*/
  if (inputLen >= partLen) {
 MD5_memcpy
   ((POINTER)&context->buffer[index], (POINTER)input, partLen);
 MD5Transform (context->state, context->buffer);

 for (i = partLen; i + 63 < inputLen; i += 64)
   MD5Transform (context->state, &input[i]);

 index = 0;
  }
  else
 i = 0;

  /* Buffer remaining input */
  MD5_memcpy
 ((POINTER)&context->buffer[index], (POINTER)&input[i],
  inputLen-i);
}

/* MD5 finalization. Ends an MD5 message-digest operation, writing the
  the message digest and zeroizing the context.
 */
void MD5Final (digest, context)
unsigned char digest[16];                         /* message digest */
MD5_CTX *context;                                       /* context */
{
  unsigned char bits[8];
  unsigned int index, padLen;

  /* Save number of bits */
  Encode (bits, context->count, 8);

  /* Pad out to 56 mod 64.
*/
  index = (unsigned int)((context->count[0] >> 3) & 0x3f);
  padLen = (index < 56) ? (56 - index) : (120 - index);
  MD5Update (context, PADDING, padLen);

  /* Append length (before padding) */
  MD5Update (context, bits, 8);

  /* Store state in digest */
  Encode (digest, context->state, 16);

  /* Zeroize sensitive information.
*/
  MD5_memset ((POINTER)context, 0, sizeof (*context));
}

/* MD5 basic transformation. Transforms state based on block.
 */
static void MD5Transform (state, block)
UINT4 state[4];
unsigned char block[64];
{
  UINT4 a = state[0], b = state[1], c = state[2], d = state[3], x[16];

  Decode (x, block, 64);

  /* Round 1 */
  FF (a, b, c, d, x[ 0], S11, 0xd76aa478); /* 1 */
  FF (d, a, b, c, x[ 1], S12, 0xe8c7b756); /* 2 */
  FF (c, d, a, b, x[ 2], S13, 0x242070db); /* 3 */
  FF (b, c, d, a, x[ 3], S14, 0xc1bdceee); /* 4 */
  FF (a, b, c, d, x[ 4], S11, 0xf57c0faf); /* 5 */
  FF (d, a, b, c, x[ 5], S12, 0x4787c62a); /* 6 */
  FF (c, d, a, b, x[ 6], S13, 0xa8304613); /* 7 */
  FF (b, c, d, a, x[ 7], S14, 0xfd469501); /* 8 */
  FF (a, b, c, d, x[ 8], S11, 0x698098d8); /* 9 */
  FF (d, a, b, c, x[ 9], S12, 0x8b44f7af); /* 10 */
  FF (c, d, a, b, x[10], S13, 0xffff5bb1); /* 11 */
  FF (b, c, d, a, x[11], S14, 0x895cd7be); /* 12 */
  FF (a, b, c, d, x[12], S11, 0x6b901122); /* 13 */
  FF (d, a, b, c, x[13], S12, 0xfd987193); /* 14 */
  FF (c, d, a, b, x[14], S13, 0xa679438e); /* 15 */
  FF (b, c, d, a, x[15], S14, 0x49b40821); /* 16 */

 /* Round 2 */
  GG (a, b, c, d, x[ 1], S21, 0xf61e2562); /* 17 */
  GG (d, a, b, c, x[ 6], S22, 0xc040b340); /* 18 */
  GG (c, d, a, b, x[11], S23, 0x265e5a51); /* 19 */
  GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa); /* 20 */
  GG (a, b, c, d, x[ 5], S21, 0xd62f105d); /* 21 */
  GG (d, a, b, c, x[10], S22,  0x2441453); /* 22 */
  GG (c, d, a, b, x[15], S23, 0xd8a1e681); /* 23 */
  GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8); /* 24 */
  GG (a, b, c, d, x[ 9], S21, 0x21e1cde6); /* 25 */
  GG (d, a, b, c, x[14], S22, 0xc33707d6); /* 26 */
  GG (c, d, a, b, x[ 3], S23, 0xf4d50d87); /* 27 */

  GG (b, c, d, a, x[ 8], S24, 0x455a14ed); /* 28 */
  GG (a, b, c, d, x[13], S21, 0xa9e3e905); /* 29 */
  GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8); /* 30 */
  GG (c, d, a, b, x[ 7], S23, 0x676f02d9); /* 31 */
  GG (b, c, d, a, x[12], S24, 0x8d2a4c8a); /* 32 */

  /* Round 3 */
  HH (a, b, c, d, x[ 5], S31, 0xfffa3942); /* 33 */
  HH (d, a, b, c, x[ 8], S32, 0x8771f681); /* 34 */
  HH (c, d, a, b, x[11], S33, 0x6d9d6122); /* 35 */
  HH (b, c, d, a, x[14], S34, 0xfde5380c); /* 36 */
  HH (a, b, c, d, x[ 1], S31, 0xa4beea44); /* 37 */
  HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9); /* 38 */
  HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60); /* 39 */
  HH (b, c, d, a, x[10], S34, 0xbebfbc70); /* 40 */
  HH (a, b, c, d, x[13], S31, 0x289b7ec6); /* 41 */
  HH (d, a, b, c, x[ 0], S32, 0xeaa127fa); /* 42 */
  HH (c, d, a, b, x[ 3], S33, 0xd4ef3085); /* 43 */
  HH (b, c, d, a, x[ 6], S34,  0x4881d05); /* 44 */
  HH (a, b, c, d, x[ 9], S31, 0xd9d4d039); /* 45 */
  HH (d, a, b, c, x[12], S32, 0xe6db99e5); /* 46 */
  HH (c, d, a, b, x[15], S33, 0x1fa27cf8); /* 47 */
  HH (b, c, d, a, x[ 2], S34, 0xc4ac5665); /* 48 */

  /* Round 4 */
  II (a, b, c, d, x[ 0], S41, 0xf4292244); /* 49 */
  II (d, a, b, c, x[ 7], S42, 0x432aff97); /* 50 */
  II (c, d, a, b, x[14], S43, 0xab9423a7); /* 51 */
  II (b, c, d, a, x[ 5], S44, 0xfc93a039); /* 52 */
  II (a, b, c, d, x[12], S41, 0x655b59c3); /* 53 */
  II (d, a, b, c, x[ 3], S42, 0x8f0ccc92); /* 54 */
  II (c, d, a, b, x[10], S43, 0xffeff47d); /* 55 */
  II (b, c, d, a, x[ 1], S44, 0x85845dd1); /* 56 */
  II (a, b, c, d, x[ 8], S41, 0x6fa87e4f); /* 57 */
  II (d, a, b, c, x[15], S42, 0xfe2ce6e0); /* 58 */
  II (c, d, a, b, x[ 6], S43, 0xa3014314); /* 59 */
  II (b, c, d, a, x[13], S44, 0x4e0811a1); /* 60 */
  II (a, b, c, d, x[ 4], S41, 0xf7537e82); /* 61 */
  II (d, a, b, c, x[11], S42, 0xbd3af235); /* 62 */
  II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb); /* 63 */
  II (b, c, d, a, x[ 9], S44, 0xeb86d391); /* 64 */

  state[0] += a;
  state[1] += b;
  state[2] += c;
  state[3] += d;

  /* Zeroize sensitive information.

*/
  MD5_memset ((POINTER)x, 0, sizeof (x));
}

/* Encodes input (UINT4) into output (unsigned char). Assumes len is
  a multiple of 4.
 */
static void Encode (output, input, len)
unsigned char *output;
UINT4 *input;
unsigned int len;
{
  unsigned int i, j;

  for (i = 0, j = 0; j < len; i++, j += 4) {
 output[j] = (unsigned char)(input[i] & 0xff);
 output[j+1] = (unsigned char)((input[i] >> 8) & 0xff);
 output[j+2] = (unsigned char)((input[i] >> 16) & 0xff);
 output[j+3] = (unsigned char)((input[i] >> 24) & 0xff);
  }
}

/* Decodes input (unsigned char) into output (UINT4). Assumes len is
  a multiple of 4.
 */
static void Decode (output, input, len)
UINT4 *output;
unsigned char *input;
unsigned int len;
{
  unsigned int i, j;

  for (i = 0, j = 0; j < len; i++, j += 4)
 output[i] = ((UINT4)input[j]) | (((UINT4)input[j+1]) << 8) |
   (((UINT4)input[j+2]) << 16) | (((UINT4)input[j+3]) << 24);
}

/* Note: Replace \"for loop\" with standard memcpy if possible.
 */

static void MD5_memcpy (output, input, len)
POINTER output;
POINTER input;
unsigned int len;
{
  unsigned int i;

  for (i = 0; i < len; i++)

 output[i] = input[i];
}

/* Note: Replace \"for loop\" with standard memset if possible.
 */
static void MD5_memset (output, value, len)
POINTER output;
int value;
unsigned int len;
{
  unsigned int i;

  for (i = 0; i < len; i++)
 ((char *)output)[i] = (char)value;
}



static void md5_sum(unsigned char *output,unsigned char *input, unsigned int len)
{
  MD5_CTX ctx;
  MD5Init(&ctx);
  MD5Update(&ctx,input,len);
  MD5Final(output,&ctx);
}

static void four_random_01_doubles_from_seeds(int nr_seeds, double *seeds, double *output)
{
  unsigned char buffer[16];
  unsigned int *x;

  md5_sum(buffer,(unsigned char *)seeds,nr_seeds*sizeof(double));

  x=(unsigned int *)buffer;

  output[0]=((double)x[0])/4294967296.0;
  output[1]=((double)x[1])/4294967296.0;
  output[2]=((double)x[2])/4294967296.0;
  output[3]=((double)x[3])/4294967296.0;
}

static double gauss_random(double u, double v, double mean, double sigma)
{
  double result, term;
  term = -2.0*log(v>0.0 ? v : 1.0);
  if (term > 0.0)

/*  {result = mean + sigma*(cos(2.0*3.1415926535897932384626*u)*sqrt(-2.0*log(v)));} */

  {result = mean + sigma*(cos(2.0*3.1415926535897932384626*u)*sqrt(term));}
  else {result=mean;}

  /* the above code is an hack to an hack: */
  /* the following line may not work for all the systems!! (it requires standard c99)*/
  /*if(!__finite(result)){result=mean;}*/

  return result;
}
"
  ];;

(* stdio and math will be needed virtually always... *)

let opt_remove_stale_c_functions = ref true;; (* Disable for debugging! *)

let active_functions = Hashtbl.create 10;;

external c_null_funpointer: unit -> c_funptr = "ocaml_c_fastfield_null_funpointer";;

external dl_open: string -> dl_handle = "ocaml_c_fastfield_dlopen";;
external dl_close: dl_handle -> unit = "ocaml_c_fastfield_dlclose";;
external dl_sym: dl_handle -> string -> c_funptr = "ocaml_c_fastfield_dlsym";;

external c_fastfield_eval:
c_funptr -> float array -> float array -> bool =
  "ocaml_c_fastfield_eval";;

external c_fastfield_eval_bigarray:
c_funptr ->
  (float, float64_elt, c_layout) Array1.t ->
    (float, float64_elt, c_layout) Array1.t ->
      bool =
      "ocaml_c_fastfield_eval_bigarray";;

external gcc_flags_shlibs: unit -> string = "gcc_flags_shlibs";;

let get_it x = 
  match x with
  | None -> failwith "Inconceivable!"
  | Some z -> z
;;

(* For conventions, see mkdtemp(3) *)

(* === Note: these directory functions should go into snippets.ml === *)

external mkdtemp: string -> string = "ocaml_c_wrapped_mkdtemp";;

type inode =
  | INO_Directory of string * inode array
  | INO_File of string
  | INO_Other of string;;

let try_and_cleanup f c =
  let f_value=ref None in
  try
    begin
      f_value := Some (f());
      c();
      !f_value
    end
  with
  | x ->
      begin
	(if !f_value = None
	then 
	  c ()
	else (* Exception was raised in cleanup! *)
	  raise x);
	!f_value
      end
;;

let dir_tree start_directory =
  let opt_readdir dir_handle =
    try
      Some (Unix.readdir dir_handle)
    with
    | End_of_file -> None
  in
  let rec walk_dir path dir_handle read_so_far =
    let next = opt_readdir dir_handle in
    match next with
    | None ->
	  Array.of_list (List.rev read_so_far)
    | Some name ->
	if (name = "." || name = "..")
	then
	  walk_dir path dir_handle read_so_far
	else
	  try
	    let extended_path=Printf.sprintf "%s/%s" path name in
	    let stat_data = Unix.lstat extended_path in
	    let this_ino =
	      match stat_data.Unix.st_kind with
	      | Unix.S_REG -> INO_File extended_path
	      | Unix.S_DIR -> get_dir extended_path
	      | _ -> INO_Other extended_path
	    in
	    walk_dir path dir_handle (this_ino::read_so_far)
	  with
	  | _ -> walk_dir path dir_handle read_so_far
		(* Just ignore errors *)
  and get_dir path =
    let d = Unix.opendir path in
    let some_subtree =
      try_and_cleanup
	(fun () ->
	  let d_tree = walk_dir path d [] in
	  INO_Directory (path,d_tree))
	(fun () -> Unix.closedir d)
    in
    (match some_subtree with
    | None -> failwith "Directory Problem"
    | Some x -> x)
  in
  get_dir start_directory
;;

let do_dir_tree_from_bottom dir_tree f =
  let rec walk subtree =
    match subtree with
    | INO_File x -> f false x
    | INO_Other x -> f false x
    | INO_Directory (x,contents) ->
	begin
	  Array.iter walk contents;
	  f true x
	end
  in
  walk dir_tree
;;

let rmdir_recursive path =
  let dt = dir_tree path in
  do_dir_tree_from_bottom dt
    (fun is_dir subpath -> 
      if is_dir
      then Unix.rmdir subpath
      else Unix.unlink subpath)
;;

let quick_write filename content =
  let oh = open_out filename in
  let () = Printf.fprintf oh "%s" content in
  close_out oh
;;

(* === === *)

(* Note that initialization and finishing functions are idempotent! *)

let rec
 init () =
  if !the_directory == None
  then
    begin
      the_directory:=Some (mkdtemp !opt_directory);
      at_exit finish;
    end
  else ()
and
 finish () =
  match !the_directory with
  | None -> ()
  | Some x ->
      begin
	rmdir_recursive x;
	the_directory:=None;
	match !the_lib_handle with
	| Some x -> 
	    begin
	      dl_close x;
	      the_lib_handle:=None
	    end
	| _ -> ()
      end
;;

let set_directory dir = opt_directory:=dir;;
let set_cc cc = opt_cc:=cc;;
let set_cc_opts cc_opts = opt_cc_opts:=cc_opts;;

let set_cc_debug_handle new_handle =
  let old_handle = !_cc_debug_handle in
  let () = _cc_debug_handle := new_handle
  in old_handle
;;

let register_include ?(library_file=true) name =
  let directive =
    if library_file
    then Printf.sprintf "#include <%s>\n" name
    else Printf.sprintf "#include \"%s\"\n" name
  in
  try
    let _ = List.find (fun x -> x = directive) !opt_includes
    in ()
  with
  | Not_found ->
      begin
	opt_includes:= directive :: !opt_includes;
	()
      end
;;

let add_header_def str =
  own_header := str :: !own_header
;;

let do_compile () =
  begin
    let wd = Unix.getcwd () in
    let _ = 
      try_and_cleanup
	(fun () ->
	let cc_invocation =
	  Printf.sprintf 
	    (* "cp fastfields_ccode.c .. ;%s %s %s -o fastfields_dynlib.so fastfields_ccode.c" *)
	    "%s %s %s -o fastfields_dynlib.so fastfields_ccode.c"
	    !opt_cc (gcc_flags_shlibs ()) !opt_cc_opts
	in
	begin
	  Unix.chdir (get_it !the_directory);
	  (match !_cc_debug_handle with
	     | None -> ()
	     | Some h ->
		 begin
		   Printf.fprintf h "%s\n" cc_invocation;
		   flush h;
		 end);
	  ignore(Unix.system cc_invocation);
	end
	)
	(fun () -> Unix.chdir wd)
    in ()
  end
;;

let do_relink_if_necessary () =
  if not(!need_relink) then
    (* let () = Printf.printf "do_relink_if_necessary() - it is unnecessary!\n%!" in *)
      ()
  else
    let header = String.concat "" (List.append !opt_includes !own_header) in
    let c_funs = ref [] in
    begin
      Gc.full_major ();
      (* This way we ensure we really cleanse any no longer used functions.
	 This is not just for tidiness or optimization, but also
	 matters semantically:
	 
	 When the user provides a C function which contains a
	 syntactical error, and then makes the system forget all
	 references to that errorneous definition, the system would
	 nevertheless fail to recompile the dynamic code unless
	 a GC run really removed that bogus entry.
	 
	 Hence, we simply place the Gc call here, to make sure this is
	 done automatically.
       *)
      Hashtbl.iter
	(fun key_sym (sym,code,_) ->
	   c_funs:= code:: !c_funs)
	active_functions;
      let c_source = 
	String.concat "\n" (header:: "\n" :: !c_funs)
      in
      let d = get_it !the_directory in
      let c_name = Printf.sprintf "%s/fastfields_ccode.c" d in
      let so_name = Printf.sprintf "%s/fastfields_dynlib.so" d in
      begin
	quick_write c_name c_source;
	Hashtbl.iter
	  (fun key_sym (sym,_,a_c_ptr) ->
	    a_c_ptr.(0) <- c_null_funpointer())
	  active_functions;
	(match !the_lib_handle with
	| None -> ()
	| Some x -> dl_close x);
	do_compile();
	the_lib_handle := Some (dl_open so_name);
	Hashtbl.iter
	  (fun key_sym (sym,_,a_c_ptr) ->
	     let () = (if debug then Printf.printf "DDD re-linking/updating c funptr for '%s'\n%!" sym else ()) in
	       a_c_ptr.(0) <- dl_sym (get_it !the_lib_handle) sym)
	  active_functions;
	need_relink := false;
	(if debug then Printf.printf "do_relink_if_necessary() - done!\n%!" else ());
      end
    end
;;

(* Note that we explicitly allow the user to
   (1) specify the names used for in and out arguments,
   and
   (2) specify #define directives
   (which MUST NOT conflict with any other
   #defines that are currently in use,
   say, from stdio.h) which are wrapped around the code.

   This way, it is possible to use the fastfields mechanism as a basis
   for more elaborate schemes to provide means to use fast
   run-time-compiled code to applications. 
 *)

let rec fun_counter = ref 0
and c_define_directives defines =
  String.concat ""
    (List.map
       (fun (name,args,def) ->
	 match args with
	 | [] ->
	     Printf.sprintf "#define %s %s\n" name def
	 | list_args ->
	     let str_args = String.concat "," list_args in
	     Printf.sprintf "#define %s(%s) %s\n" name str_args def
       ) defines)
and c_undef_directives defines =
  String.concat ""
    (List.map
       (fun (name,_,_) -> Printf.sprintf "#undef %s\n" name)
       defines)
and c_register_field
    ?(in_name="position")
    ?(out_name="result")
    ?(extra_defines=[])
    c_code =
  let () = init () in
  let nr = !fun_counter in
  let () = fun_counter:= nr+1 in
  let sym = Printf.sprintf "ocaml_fastfield_field_%d" nr in
  let define_directives = c_define_directives extra_defines in
  let undef_directives = c_undef_directives extra_defines in
  (* Note that we include a "return 1" at the end - the default is
     to finally announce that the calculation was successful.
     If one wants to early-abort, this can be done via "return 0;".
   *)
  let full_c_code =
    Printf.sprintf
      "%s\n\nint %s(double *%s, double *%s){\n%s\nreturn 1;\n}\n\n%s\n\n"
      define_directives
      sym in_name out_name c_code
      undef_directives
  in
  let () = need_relink := true in
  let ref_fp = [|c_null_funpointer()|] in
  let the_c_field = (sym,full_c_code, ref_fp) in
  let the_c_field2 = (sym,full_c_code, ref_fp) in (* This is a sneaky/tricky hack on the GC! *)
  let () = Gc.finalise
    (fun (sym,_,_) ->
       let () = (if debug then Printf.printf "DDD Removing forgotten C function '%s'%s\n%!" sym
		   (if !opt_remove_stale_c_functions then "" else " - disabled") else ())
       in
	 if !opt_remove_stale_c_functions
	 then
	   Hashtbl.remove active_functions sym
	 else ())
    the_c_field
  in
  let () = Hashtbl.add active_functions sym the_c_field2 in
    the_c_field
and c_register_field_manipulator
    ?(buffer_name="__refbuf")
    ?(posbuffer_name="__position")
    ?(extra_defines=[])
    c_code =
  let () = init () in
  let nr = !fun_counter in
  let () = fun_counter:= nr+1 in
  let sym = Printf.sprintf "ocaml_fastfield_manipulator_%d" nr in
  let define_directives = c_define_directives extra_defines in
  let undef_directives = c_undef_directives extra_defines in
  let full_c_code =
    Printf.sprintf
      "%s\n\nint %s(double **%s, double *%s){\n%s\nreturn 1;\n}\n\n%s\n\n"
      define_directives
      sym buffer_name posbuffer_name c_code
      undef_directives
  in
  let () = need_relink := true in
  let ref_fp = [|c_null_funpointer()|] in
  let the_c_manipulator = (sym,full_c_code, ref_fp) in
  let the_c_manipulator2 = (sym,full_c_code, ref_fp) in
  let () = Gc.finalise
    (fun (sym,_,_) ->
       let () = 
	 (if debug then
	    Printf.printf "DDD Removing forgotten C manipulator '%s'%s\n%!" sym
	      (if !opt_remove_stale_c_functions then "" else " - disabled")
	  else ())
       in
	 if !opt_remove_stale_c_functions
	 then
	   Hashtbl.remove active_functions sym
	 else ())
    the_c_manipulator
  in
  let () = Hashtbl.add active_functions sym the_c_manipulator2 in
  let () =
    begin
      (*
	do_relink_if_necessary();
	Printf.printf "DDD fastfields do_relink_if_necessary() called conservatively (maybe unnecessarily!) - remove after devel phase!\n%!";
      *)
    end
  in
    the_c_manipulator
;;

(* XXX semi-internal! This is to be used by libraries only! *)
let _c_manipulator_funptr ((_,_,a):c_manipulator) = a.(0)
;;

let c_field_evaluator_mapping_float_array_to_float_array
    ?(allocate_every_result=true) 
    dim_output
    c_field =
  let the_result =
    if allocate_every_result
    then None
    else Some (Array.make dim_output 0.0)
  in
  let fun_compute_me position =
    let output =
      if allocate_every_result
      then (Array.make dim_output 0.0)
      else
	match the_result with
	| Some x -> x
	| None -> failwith "Impossible!"
    in
    let c_funptr = 
      let () = do_relink_if_necessary () in
      let (_,_,x) = c_field in x.(0)
    in
    let success = c_fastfield_eval c_funptr position output in
    if success then output else raise Fastfields_eval_problem
  in
  fun_compute_me
;;


let c_field_evaluator_modifying_float_array
    c_field =
  let fun_compute_me ~position ~result =
    let c_funptr = 
      let () = do_relink_if_necessary () in
      let (_,_,x) = c_field in x.(0)
    in
    let success = c_fastfield_eval c_funptr position result in
    if success then () else raise Fastfields_eval_problem
  in
  fun_compute_me
;;

let c_field_evaluator_modifying_bigarray
    c_field =
  let fun_compute_me ~position ~result =
    let c_funptr = 
      let () = do_relink_if_necessary () in
      let (_,_,x) = c_field in x.(0)
    in
    let success = c_fastfield_eval_bigarray c_funptr position result in
    if success then () else raise Fastfields_eval_problem
  in
  fun_compute_me
;;

let version () = "$Id$";;
