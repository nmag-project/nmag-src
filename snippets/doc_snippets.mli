 (*
  (C) 2005 Dr. Thomas Fischbacher, Giuliano Bordignon, Dr. Hans Fangohr,
  SES, University of Southampton

  $Header$
*)

(** A collection of small generally useful functions *)

(** 
 A collection of generally useful functions that do not fit into a
 more specific category, but are quite handy for a lot of
 applications.

{4 Notes}
   There is a lot of evolution going on in this catch-it-all module:
   Typically, whenever a specialized module is implemented, one will
   recognize the necessity for a collection of small, useful functions
   that may have broader applicability than originally intended.
   These should go into the [Snippets] module first. Over time, it may
   happen that a large enough collection of those snippet functions can be
   grouped under a more specific common theme so that moving them out
   into a more specific library is justified. (All modules using the
   corresponding functions from [Snippets] have to be adjusted afterwards
   to use that new library. But this is not a problem, as failed linking
   attempts will tell us where definitions of moved functions are missing.)

*)

val version : unit -> string

val register_feature : ?domain:string -> string -> string -> unit
val get_feature : ?domain:string -> string -> string option
val all_features : unit -> (string * (string * string) list) list
(** The [features] mechanism is similar to Common LISP's [*features*],
    and allows key/value strings to be registered globally so that pieces
    of a large program can check for the presence of other pieces,
    and get an idea of configuration issues. 

    Unlike LISP, our implementation provides associative string/string
    pairs instead of just binary information (feature present/not present),
    and furthermore groups features into (named) domains, the default
    one being [main].
 *)

val register_version : string list -> unit
(** This will take a list of strings that describe the pieces of the running program in question
    (typically, a script)
    and [register_feature ~domain:"main" "version" {some unique ID derived from these strings}].
*)

type 'a array2 = 'a array array
(** This is a convenient type-alias shorthand for two-dimensional
    arrays, represented as arrays of arrays. Whenever this is used
    to represent a matrix, indexing will be as in [matrix.(row).(column)].
 *)

val identity: 'a -> 'a
val constantly: 'a -> 'b -> 'a
val evaluate_at: 'a -> ('a -> 'b) -> 'b
(** These are basic combinators which we do need from time to time *)


val pi: float
(** The number pi, to machine precision *)

val deg_rad: float -> float
val rad_deg: float -> float
(** The degree-to-radians and radians-to-degree
   conversion functions, 180 degree = pi rad. *)

val round_to_n_digits : ?base:int -> int -> float -> float
(** Rounding a number not towards the nearest integer,
    but to a certain number of valid digits. Base may be specified.
    
    This is especially interesting in conjunction with the fact that
    base-2 rounded numbers (unless too large or too long) can be 
    expressed exactly in floatingpoint.
*)

val impossible: unit -> 'a
(** There are situations for which one should provide a code path,
    even though they cannot arise.

    An example: suppose we know from arguments involving what
    an algorithm does that a certain case cannot occur,
    but the compiler is not smart enough to see this, and demands
    e.g. that some pattern matching completely covers all cases.

    It would perhaps be appropriate to put a
{v
failwith "Something thougt impossible just happened"
v}
   in the corresponding place. This function is just a shorthand
   for that.
 *)

val not_implemented_yet: string (* message *) -> ('a -> 'b) 
(** Args:
{v
val not_implemented_yet: string (* message *) -> ('a -> 'b) (* stub-function *)
v}

    Occasionally, we want to write code in a slightly non-sequential order,
    that is, finish the work on a function [f] which in a special subcase 
    uses a function [g] before that function [g] has been written.

    Therefore, we need a way to place a stub that just says
    "this function des not exist yet".

    This function maps the string [message] to such a [stub] function
    which, when called, will just raise the exception:
{v
Failure: "This function has not been implemented yet: <message>"
v}

   Note that the type is generic enough to use this stub in place
   of any function.
 *)

val missing_param: unit -> 'a
val is_obj_nan : 'a -> bool 
val iterate : int (* max *) -> ('a -> 'a) (* mapping *) -> 'a (* start *) -> 'a
val church  : int -> ('a -> 'a) -> 'a -> 'a
(** These two functions are synonymous and implement
    counting as iteration. The idea is that one may re-interpret
    e.g. the number five as a machine which says: "give me some
    notion of a successor [mapping], and a value to [start] with,
    and I tell you what you get if you successively apply that
    [mapping] five times to the start value".
    This is just Church's implementation of natural numbers
    in lambda calculus. These functions just map a number to such an
    iterating function.

   {e Example}
{v
# iterate 8 (fun x -> x * 2) 100;;
=> int = 25600

# let ten_times = iterate 10 in ten_times (fun x -> x*.x -. x) 0.3;;
=> float = 0.177284654445099776
v}

 *)

val pointwise : ('val_f -> 'val_g -> 'val_f_op_g) (* op *)
  -> ('x -> 'val_f) (* f *) -> ('x -> 'val_g) (* g *)
  -> ('x -> 'val_f_op_g) (* f_op_g *)
(** One re-occurring construction is the point-wise combination
    of two functions: given two functions [f], [g], which have
    the same domain, and a binary operation [op] defined on the co-domains
    of those functions, we can obtain a new function by mapping a value
    [x] to the result of applying the given operation to [f(x)]
    and [g(x)], which we will call [f_op_g].

   Note: many systems (such as e.g. Maple) use an 'operator overloading'
   approach to achieve similar effects: once addition is defined on numbers,
   its action automatically is extended to functions mapping numbers
   to numbers, etc.

{e Example}
{v
# let pointwise_pair = pointwise (fun x y -> (x,y)) in
   let fg = pointwise_pair (fun x -> x*x) (fun x -> x*x*x) in
   Array.map fg [|0;1;2;3|];;
=> (int * int) array = [|(0, 0); (1, 1); (4, 8); (9, 27)|]
v}

*)

val optionally: ('a -> 'b) -> ('a option -> 'b option) (* "map" for the "option" monadic type *)

val array_pointwise :
 ('a -> 'b -> 'c) (* op *) ->
 'a array -> 'b array ->
 'c array
(** Given a binary operation [op] and two arrays of values, map them to
   an array of combined values.

   This is, in fact, the function [List.map2] for Arrays. We therefore 
   define the synonym [array_map2] below.

{e Example}
{v
#use "topfind";;
#require "Snippets";;

let b =[|"dog";"cat";"mouse";"elephant";"tiger"|] in 
  let a=[|1;3;5;7;11|] in 
  Snippets.array_pointwise (fun a b -> (a,b)) a b;;

Output is 
 (int * string) array =
[|(1, "dog"); (3, "cat"); (5, "mouse"); (7, "elephant"); (11, "tiger")|]
v}
*)

val array_map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
(** Synonym for array_pointwise. In fact, array_map provides the functionality
   one would expect from the (nonexistent standard library function) [Array.map]
   (in analogy to the existing [List.map2].
*)

val list_foreach_do: ('a list) -> ('a -> unit) -> unit
val array_foreach_do: ('a array) -> ('a -> unit) -> unit
val array_foreach_do_n: ('a array) -> (int -> 'a -> unit) -> unit
val hashtbl_foreach_do : ('a, 'b) Hashtbl.t -> ('a -> 'b -> unit) -> unit
(** These functions are just [Array.iter] and [Array.iteri], resp.
    [Hashtbl.iter] with argument order reversed
 *)


val array_compare_pointwise: ('a -> 'b -> bool) -> 'a array -> 'b array -> bool

val memoized : ('a, 'b) Hashtbl.t -> ('a -> 'b) -> 'a -> 'b
(** Sometimes, the easiest way to express an idea is to use
   functions that are costly to evaluate, but needed only
   for a few frequently re-occurring values. Hence, it would be
   good to just automatically remember what the values of
   previous evaluations of that function were. This technique is known as
   "memoizing" and sometimes can be used very effectively to turn a naively
   implemented algorithm that performs poorly into one that performs well - 
   with negligible effort on the programmer's side.

   {e Example}
{v
let rec fibonacci n =
  if n < 2 then 1 else fibonacci (n-1) + fibonacci (n-2)
;;
(*
# timing fibonacci 35;;
Time passed: 4.254157 sec
=> int = 14930352
*)

let mem_fibonacci =
  let mem = Hashtbl.create 10 in
  let rec mf x =
    memoized mem
      (fun n -> if n < 2 then 1 else (mf (n-1)) + (mf (n-2))) x
  in mf
;;

(* Note: one might consider eta-reducing away the "x" in the
   definition above. Unfortunately, that won't work, as OCaml
   does not allow an application as a right-hand-side of a let rec.
 *)

(* 
# timing mem_fibonacci 35;;
Time passed: 0.000149 sec
=> int = 14930352

# timing mem_fibonacci 35;;
Time passed: 0.000005 sec
=> int = 14930352
*)

v}

*)

val uniq : (unit -> 'a option) -> ('a, 'a) Hashtbl.t
(** Given a generator function which will return a series of elements on subsequent calls,
    wrapped up in an ['a option], or None when finished, return an uniq-ing hash which
    maps every value encountered to an unique single memory representative
*)

val float_factorial : int (* n *) -> float
(** Map the number [n] to the floatingpoint value of its factorial. *)

val int_gcd : int -> int -> int
val int_lcm : int -> int -> int
(** Greatest common divisor and least common multiple for integers. *)

val int_power: int -> float -> float
(** [x^n] for integer n *)

val int_length: int -> int
(** Length of the binary representation of an integer *)

val do_for_any_two_of: 'a array (* a *) -> ('a -> 'a -> unit) (* f *) -> unit
(** Given an array [a] and a function [f], call [f a.(i) a.(j)] for all
   [i], [j] with [i<j] in lexicographically ascending order.
 *)

val do_for_all_distributions :
  int -> int array -> (int array -> unit) -> unit
(** Given a number of elements to distribute and an array of maximum
    number of elements per bucket, call the given function for every
    possible distribution of elements to buckets once. Note: the "counting
    vector" passed to that function is re-used, so you should make a copy
    if you want to store it away. *)

val hashtbl_increase : ('coeff -> 'coeff -> 'coeff) (* sum *) ->
 ('a, 'coeff) Hashtbl.t (* h *) -> 'a -> 'coeff -> unit
(** One sometimes wants to use hash tables to count/accumulate occurrences.
   One example application would be accumulating coefficients in some simple
   term manipulation function. Another application would be grouping log file
   entries (e.g. collecting web requests by source IP).

   Given a coefficient-adding function [sum], a hash table [h], a key and 
   a correponding coefficient contribution, either make a new entry into [h],
   or if there already was one, increase the coefficient of that entry in [h]
   by [sum]ing the old coefficient and the new contribution.

   Note: entries are not deleted automatically if an increment makes
   a coefficient zero.
*)

val hashtbl_push : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit

val hashtbl_pushn : ('a, 'b list) Hashtbl.t -> 'a -> 'b list -> unit

val hashtbl_sum : ('coeff -> 'coeff -> 'coeff) ->
 ('a, 'coeff) Hashtbl.t -> ('a, 'coeff) Hashtbl.t -> ('a, 'coeff) Hashtbl.t
(** Starting from [hashtbl_increase], one can form the sum of two such
   accumulating hash tables in the obvious way. This is what this function does.
 *)

val hashtbl_reduce_map :
  ('a -> 'b -> 'b -> 'b) ->
  ('c -> 'd -> 'a * 'b) -> ('c, 'd) Hashtbl.t -> ('a, 'b) Hashtbl.t
(**
 Given a hash table [h1] and a mapping function [f] on its elements
 this function returns a new hash table [h2] with elements f-mapped
 from the original. If more than one entry is mapped to the same key,
 the reduction function is used to map the new key, new value,
 and previous new value, to an updated new value.
*)

val hashtbl_iteri: (int -> 'a -> 'b -> unit) (* f *) -> ('a, 'b) Hashtbl.t (* h *)-> unit
(** Given a function [f] and an hash table [h], f is applied in turn 
   to all the elements of h, with the index of the element as first argument, 
   and the key-value pair as second and third argument*)
 
val hashtbl_delete_if : ('a -> 'b -> bool)  (* f *) -> ('a, 'b) Hashtbl.t  (* h *) -> unit
(** Function similar to [hashtbl_iteri] but now the elements of the hash table [h]
   are deleted if the function [f], applied to the hash table elements with a 
   boolean output, is true*)

val hashtbl_find_or : ?make_new_entry:bool -> ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b

val hashtbl_arbitrary_element : ('a, 'b) Hashtbl.t  (* h *) -> ('a * 'b) option
(** Function that returns None if the hash table [h] is empty or Some if h contains 
   something *)

val map_hashtbl_to_array : ?sorter:('a -> 'a -> int)  (* s *) -> ('b -> 'c -> 'a)  (* f *) -> ('b, 'c) Hashtbl.t  (* h *)-> 'a array  (* a *)
(** Function that creates an array [a] from the values given by the mapping of a 
   function [f] on the elements of the hash table [h]. The entries of the array 
   are optionally sorted if a sorting function [s] is given *)

val hashtbl_to_array : ?sorter:('a * 'b -> 'a * 'b -> int)  (* s *) -> ('a, 'b) Hashtbl.t  (* h *)-> ('a * 'b) array (* a *)
(** Function that creates an array [a] from the elements of an hash table [h]. 
   The entries of the array are optionally sorted if a sorting function [s] is
   given *)

val map_array_to_hashtbl : mapper:('a -> 'b) -> ('c * 'a) array -> ('c, 'b) Hashtbl.t

val hashtbl_keys : ?sorter:('a -> 'a  -> int)  (* s*) ->
  ('a, 'b) Hashtbl.t  (* h *) -> 'a array  (* a *)
(** Function that creates an array [a] from the keys of an hash table [h]. 
   The entries of the array are optionally sorted if a sorting function [s] 
   is given *)

val hashtbl_map_values : ('a -> 'b -> 'c) -> ('a, 'b) Hashtbl.t -> ('a, 'c) Hashtbl.t
(** Given a hash table and a mapping function [f] that acts on its entries, create a new
    hash table where every key is mapped to [f key old_value]
*)

val array_all_satisfy: ('a -> bool)  (* f *) -> ('a array)  (* a *) -> bool
(** Function that returns true if all the entries of an array [a] satisfy
   the condition given by the function [f] *)

val array_sorted : ('a -> 'a -> int)  (* f *) -> 'a array  (* a *) -> 'a array
(** Function that sorts an array [a] through a function [f]; the original
   array is preserved *)

val sorted_array_has_duplicate : ('a -> 'a -> bool) -> 'a array -> bool

val array_rev : 'a array  (* a *) -> 'a array
(** Function that reverses an array [a]; the original array is preserved *)

val array_repeat_ntimes : int -> 'a array -> 'a array
(** Concatenate an array n-times to itself *)

val array_reducing_mapi : ('a -> 'b -> int -> 'c * 'a) -> 'b array -> 'a -> 'c array
(** TODO *)

val array_find : ('a -> bool)  (* f *) -> 'a array  (* a *) -> 'a
(** Function that searches for any entry of the array [a] to satisfy 
   the condition in [f] and returns the first good entry or Not_found *)

val array_filter: ('a -> bool)  (* f *) -> ('a array)  (* a *) -> ('a array)
(** Function that filters the entries of an array [a] keeping the
   ones which satisfy [f] *)

val array_mapfilter : ('a -> 'b option) -> 'a array -> 'b array
(** TODO Function that filters the entries of an array keeping the
   ones which satisfy f and maps the result th*)


val array_mapfilteri : (int -> 'a -> 'b option) -> 'a array -> 'b array
(** TODO *)

val array_mapfilterij : (int -> int -> 'a -> 'b option) -> 'a array -> 'b array
(** TODO *)


val array_one_shorter : 'a array  (* a *) -> int  (* p *) -> 'a array
(** Function that shortens an array [a] cutting the entry at position [p] *)

val array_one_shorter_mapped : ('a -> 'b) (* f *) -> 'a array  (* a *) -> int  (* p *) -> 'b array
(** Function that shorts an array [a] deleting the entry at position [p] and
   maps the results through the function [f] *)

val array_all_one_shorter: 'a array  (* a *) -> 'a array array  (* aa *)
(** Function that creates an array of arrays [aa] where each array entry
   e is built deleting from [a] the entry correspondent to the index of e in [aa] *)

val array_all_one_shorter_mapped : ('a -> 'b)  (* f *) -> 'a array -> 'b array array
(** Function similar to [array_all_one_shorter] but maps each array entry e through
   the function [f] *)

val array_outer_product : ('a -> 'b -> 'c)  (* f *) -> 'a array  (* a1 *) -> 'b array  (* a2 *) -> 'c array array (* aa *)
(** Function that creates an array [aa] whose entries are all the array pairs e 
   built with the values of [f] sampled in the points (x,y) where x is any of the 
   entries of the array [a1] and y any of the entries of the array [a2]*)

val array_uniqify : 'a array array (* aa *) -> 'a array (* a *) 
(** Function that merges all the array entries e of [aa] in an single array [a] and 
   deletes all the multiple entries from a*)

val array_to_hashtbl : ?reduce:('a -> 'a -> 'a) -> ('b * 'a) array -> ('b, 'a) Hashtbl.t
(** Map an array of pairs [(key,value)] to a corresponding hash table. 
    A reduction fun may be specified to deal with re-occurring keys, the default being to
    just enter the last association appearing in the array into the hash.
*)

val components_of_connection : ('a -> 'a array) -> 'a array -> 'a array array

val array_nums_with_property : (int -> bool) -> int -> int array
(** Given a maximum number [n], form an array of all numbers
    in the interval 0..(n-1) (including boundaries) for which the given
    property holds, in increasing order.
*)

val array_gcd_reduced : int array -> int array
(** Given an array of integers, compute another array of integers
    where the gcd has been divided out.
 *)

val float_arrays_avg : ?storage:float array  (* s *) -> float array array  (* aa *) -> float array (* ave *) 
(** Function that, given an array of arrays [aa], whose entries are e, 
   returns the average value [ave] of the array entries e.
   If a store array [s] is given, the result is saved there *)

val bool_array_to_string : bool array  (* a *) -> string 
(** Function that transforms a boolean array [a] in a string *)

val a_array_to_string: ('a -> string) -> 'a array -> string
val string_array_to_string: string array -> string

val quote_string: string -> string
(** This maps a string like ["one \"two\" three"] to the string ["\"one \\\"two\\\" three\""].
*)

val string_compare_n: int -> string -> string -> bool

val string_one_shorter: string -> int -> string

val string_append: string -> string -> string

val interpolate_points : float array -> float array -> int -> float array array

(* XXX TODO: Document these functions! *)
val interpolate :
    ('a -> 'a -> 'b) ->
      (float -> 'c -> 'a) -> float * 'c -> float * 'c -> float -> 'b

val interpolate_piecewise :
  ('a -> 'a -> 'b) ->
  (float -> 'b -> 'a) -> (float * 'b) array -> float -> 'b

val color_interpolate_piecewise :
  (float * float array) array -> float -> float array


val float_array_to_string : float array  (* a *) -> string 
(** Function that transforms a float array [a] into a string *)

val int_array_to_string : int array  (* a *) -> string
(** Function that transforms an integer array [a] into a string *)

val float_array2_to_string : float array array (* a *) -> string 
(** Function that transforms a float array array [a] into a string *)

val int_array2_to_string : int array array (* a *) -> string 
(** Function that transforms an int array array [a] into a string *)

val string_to_charcode_string : string -> string

val sparse_float_matrix_to_string : ?min_size:float  (* min *) -> float array array  (* m *) -> string (* s *) 
(** Function that transforms a float sparse matrix m in a string [s] taking only
   the values exceeding a minimum value [min] (to avoid printing noise) *)

val float_array_max : float array  (* a *) -> float
(** Functions that returns the max value of the float array [a]:
   works only for arrays whose lenght is > 0 *)

val array_position_if : ('a -> bool)  (* f *) -> 'a array  (* a *) -> int  (* p0 *) -> int (* p *) 
(** Function that returns the position [p] of the first entry e of the array
   [a] that satisfy the conditon [f]. The search starts from the position [p0] 
   and returns -1 if the condition is not satisfied *)

val array_position : 'a  (* e *) -> 'a array  (* a *) -> int  (* p0 *) -> int
(** Function that returns the position of the entry [e] in the array [a] 
   if its index is bigger than the index [p0] *)

val array_mapjoin : ('a -> 'b)  (* f *) -> 'a array array  (* aa *) -> 'b array (* a *) 
(** Function that takes a mapping function [f] and an array of arrays [aa], 
   whose elements are e, and merges the elements e mapped through f in [a]*)
 
val array_join : 'a array array  (* aa *) -> 'a array  (* a *) 
(** Function that merges in [a] the array elements e of the array of arrays (* aa *) *)

val list_iteri : (int -> 'a -> unit) -> 'a list -> unit
(** Like [Array.iteri], but iterates over a list. *)

val list_intersection : 'a list  (* l1 *) -> 'a list  (* l2 *) -> 'a list (* l3 *) 
(** Function that returns a list [l3] whose elements are the 
   elements common to the lists [l1] and [l2] *)

val lists_intersection : ?sorter:('a -> 'a -> int)  (* s *) -> 'a list list  (* ll *) -> 'a list  (* l *) 
(** Function that returns a list [l] whose elements are the 
   elements common to all the elements e of the given list of lists [ll]. 
   The entries of l are optionally sorted if a sorting function [s] is
   given*)

val list_mapfind : ('a -> 'b option) -> 'a list -> 'b option
(** TODO*)

val list_position_if: ('a -> bool) -> 'a list -> int

val list_all_satisfy: ('a -> bool) -> 'a list -> bool

val string_multifill_template_then_concat :
  ?separator:string -> string -> string array -> string array array -> string

val gauss_random : ?fun_rng:(float -> float) (* g  *) -> float  (* m  *) -> float (* w  *)  -> float (* n  *) 
(** Functions that returns a random number [n] from a gaussian distribution [g]
   with a certain mean [m] and width [w]. The distribution g can be optionally 
   changed *)

val estimate_max_and_average :
  ?conservative_factor:float (* cf  *) -> int (* n  *)  -> ('a -> float)  (* f  *) -> (unit -> 'a)  (* g *) -> float * float (*  m,a  *)
(** Function that returns estimate values for the max and average [m,a] of a
   density function [f] sampling it to a given number [n] of points with a density
   function [g]. The conservative factor [cf], used to compute the max, can be 
   changed from its default value 1.12 *)  

val gauss_random_n_dim :
  ?fun_rng:(float -> float) (* f  *)  -> float array (* m *) -> float (* w *) -> float array (* p *) 
(**Function that returns a random N-dim point [p] from a N-dim gaussian distribution [f] with
   a N-dim mean [m] and width [w] *)

val distribute_points_randomly :
  ?fun_rng:(float -> float)  (* f  *) ->
  ?max_density_and_average:float * float (* m,a *)  ->
  (unit -> 'a) -> ('a -> float) -> int -> 'a array * float
(** Function that returns a set of random points taken from
   the given density function [f] with an optionally max density and average [m,a]
 TODO *)

val scalar_product: float array  (* a0 *) -> float array (* a1 *) -> float
(** N-dim scalar product between [a0] and [a1]*)

val cross_product_3d: float array -> float array -> float array
(** Compute the vector cross product of two 3d vectors *)

val triangle_space_angle_3d: float array -> float array -> float array -> float array -> float
(** Given an observer point and three vertices, this gives the (positive) space angle
    taken up by the triangle as seen from the observer.
*)


val euclidean_distance_sq: (float array) (* a0 *)  -> (float array) (* a1 *) -> float
(** N-dim distance between [a0] and [a1] *)

val euclidean_len_sq: (float array) (* v *) -> float
(** Square of the N-dim module of the vector [v] *)

val triangle_area : float array -> float array -> float array -> float
(** Given three points in D-dimensional space, this gives the area of the enclosed triangle *)

val random_point_on_d_sphere :
  ?fun_rng:(float -> float) -> int -> float array

val symm_grad_storing_result: ?epsilon:float (* e *) -> int  (* d *) -> float array (* p *) -> (float array -> float) (* g *)  -> (float array) (* v *)  -> unit
(** [d]-dim gradient computed with the central differences method. A step size epsilon [e]
   is mapped to a gradient function [g] that stores the value g([p]) in an output vector [v] *)

val symm_div: ?epsilon:float (* e *)  -> int (* d *)  -> (float array -> float array)  (* g *) -> (float array -> float)
(** TODO [d]-dim divergence computed with the central differences method. A step size epsilon [e]
   is mapped to a divergence function [g] *)
 

val find_minimum_on_line : ?x_tolerance:float -> (float -> float) -> float -> float -> float
(** Given a concave function, and an auxiliary start and end search point,
    find its minumum on a line
*)

val walk_to_minimum :
  ?x_tolerance:float ->
  ?grad_step:float ->
  ?stop_grad_len:float ->
  (float array -> float) -> float array -> float array
(** Given a concave multidimensional function and a start position,
    walk to the minimum of that function *)

val make_gram_schmidt_orthonormalizer :
  int ->
  ((unit -> unit) * (unit -> bool) * (float array -> unit) * (int -> float array -> unit))
(** TODO: document me! *)

val compute_det_on_scratchpad: int (* d *)  -> float array  (* m *) -> float
(** Function that computes the determinant of a matrix written in a 
   serialized form [m] where the original matrix dimension is [d]x[d] *)

val determinant: int (* d *)  -> float array array  (* m *) -> float
(** Determinant of a matrix [m] whose dimension is [d]x[d] *)
 
val hodge_dualize_to_1form : int -> float array array -> float array

val det_and_inv: int (* d *)  -> float array array -> float  (* m *) * float array array (* d-i *) 
(** Function that returns the pair determinant-inverse [d-i] of a matrix [m] 
   whose dimension is [d]x[d] *)

val mx_mult: float array array (* m1 *)  -> float array array (* m2 *)  -> float array array (* m3 *) 
(** Multiplication of matrices: [m1] x [m2] = [m3] *)

val mx_x_vec : ?store_result:float array (* a *)  -> float array array (* m *) -> float array  (* v *) -> float array
(** Multiplication matrix [m] vector [v]: if given, the result is stored in the array [a] *)

val mx_transpose: float array array -> float array array

val all_distributions_to_buckets: int (* n *)  -> int (* k *) -> int array array (* aa *) 
(** Given [n] stones and [k] buckets, compute an array [aa]
   whose entries are arrays of length [k] and represent all the configurations 
   how k stones can be distributed into [n] buckets.

{e Example:}
{v
# Snippets.all_distributions_to_buckets 3 0;;
=>  int array array = [|[|0; 0; 0|]|]
# Snippets.all_distributions_to_buckets 3 1;;
=>  int array array = [|[|0; 0; 1|]; [|0; 1; 0|]; [|1; 0; 0|]|]
# Snippets.all_distributions_to_buckets 3 2;;
=>  int array array =
[|[|0; 0; 2|]; [|0; 1; 1|]; [|0; 2; 0|]; [|1; 0; 1|]; [|1; 1; 0|];
  [|2; 0; 0|]|]
v}
*)

val partitioning_multinomial_weight: int array -> float
(** XXX TODO*)

val hyperplane_eval: float array (* eq *)  -> float array (* p *)  -> float
(** Function that evaluates the plane equation [eq] in a point [p] *)

val hyperplane_eqn: int (* n *)  -> float array array (* pp *)  -> float array (* eq *) 
(** Function that returns the equation [eq] of the (n-1)-dim plane passing
   through the [n] points whose coordinates are the elements of the array [pp] *)

val points_mid_hyperplane_sr: float array -> float array -> float array -> float array
(** TODO *)

val hyperplane_do_normalize_return_norm: float array (* eq *) -> float
(** Function that normalize the coefficients of a hyperplane equation [eq] *)

val hyperplanes_intersection_point: int -> float array array -> float array
(** TODO *)

val simplex_circumcircle_midpoint_radius :
  ?dim_simplex:int -> ?smallest_allowed_distance:float -> int -> float array array -> float array * float
(** TODO: update this documentation!
 Function that given a [n]-dim simplex through its nodes coordinates [pp]
 returns the coordinates of the circumcirle center and the related radius [c,r]
*)

val simplex_incircle_midpoint_radius:
  int (* n *)  -> ?smallest_allowed_distance:float -> float array array (* pp *)  -> float array * float (* c,r *) 
(** Function that given a [n]-dim simplex through its nodes coordinates [pp]
   returns the coordinates of the incircle center and the related radius [c,r] *)

val d_dimensional_space_angle_ub_lb: int -> float array -> float array array -> (float * float)
(** Given a dimension, an observer position and a set of vertices, this computes
    simple upper and lower bound estimates for the spatial angle subtended by the simplex
    as seen from the observer.

    Note: This function is sectioned at the first argument.
    (XXX TODO: need to introduce the notion/technique of "sectioning" (a term I just invented)
    at the beginning of this documentation, so the previous sentence makes sense.

    Sectioning basically means that if we just provide arguments up to
    some place, the function can digest them and do some internal setup, which will make the
    further computation that is started by providing the missing arguments much faster than if
    one always gave the full list of arguments. Should be explained through an example where
    one can see this difference. Also, note on non-reentrantness of such techniques.
*)

val line_point_sr: float array (* s *)  -> float array (* p0 *) 
  -> float array (* p1 *)  -> float  (* t *) -> float array (* p2 *) 
(** Function that given two points [p0] and [p1] builds a line connecting these
   points and returns the point [p2] associated with the parameter [t]
   along this line. The parameter is defined to be 0 in p0 and
   1 in p1. If given, the result is stored in the array [s] *)

val line_point: float array (* p0 *)  -> float array (* p1 *)  -> float (* t *)  -> float array (* p2 *) 
(** Function that, given the points [p0] and [p1] and a parameter [t] 
   along the line connecting these points (with t=0 in p0 and
   t=1 in p1), returns the coordinates of the point [p2] along the
   line whose parameter is t *)

val line_hyperplane_intersection_param:
  float array (* p0 *) -> float array (* p1 *) -> float array (* eq *)  -> float (* t *) 
(** Function that given the points [p0] and [p1] and the hyperplane
   equation [eq] returns the parameter [t] along the line p0-p1 
   (see [line_point]) associated with the intersection line-hyperplane *)

val volume_regular_simplex: int -> float
(** Given a dimension d, this function computes the volume of the regular simplex in d dimensions. For d=2, this is the volume of a triangle (with side length 1), for d=3 the volume of a tetrahedron, etc.

 (-> see [examples/geometricfunctions.ml] )
*)

val volume_p_simplex_in_q_dimensions : int -> int -> float array array -> float


val volume_d_sphere: int -> float
(** Given a dimension d, this computes the volume of the d-dimensional
(unit) sphere (with radius 1). 

   (-> see [examples/geometricfunctions.ml] )
*)

val surface_d_sphere: int -> float
(** Given a dimension d, this computes the surface of the d-dimensional
(unit) sphere (with radius 1). 
*)

val points_on_3d_sphere: int -> float array array
(** This maps a number [N] to an array of 2+4^(N+1) somewhat
   equidistributed points on a three-dimensional sphere, obtained
   by recursive regular sub-division of an octahedron
 *)

val sphere_packing_ratio_lattice_type_d: int -> float
(** Given a dimension d, this Computes the densest packing ratio of
spheres in d dimensions. Probably only accurate up to d=5. 

   (-> see [examples/geometricfunctions.ml] )
*)

val numerically_integrate_float_over_box:
  ?fun_rng:(float -> float) ->
  ?nr_points:int ->
  float array * float array -> (float array -> float) -> float
(** Given two float arrays a and b which contain positions of 
   diametrally opposite corneres of a box, and a function f that
   maps a position r to a float, this function will numerically
   integrate f(r) over the box (using Monte Carlo integration).

   Optional parameters are [fun_random_float]: a random number generator
   and [nr_points]: the number of random points to be used to estimate
   the integral (defaults to 1000).

   (-> see [examples/integration.ml] )
*)



(*
val line_hyperplane_intersection_sr:
  float array ->
  float array -> float array -> float array -> (float * float array)

val line_hyperplane_intersection:
  float array -> float array -> float array -> (float * float array)
*)

val read_file_as_lines : string -> string list
(** Given a filename ([string]), this function reads the file and returns a list of strings where each string represenets one line.

(-> [examples/readfile.ml])
*)

val read_dir: string -> string list
(** Read the entries of a directory *)

val regexp_decompose : int -> string -> string -> string option array list

val chomp : string -> string
(** Given a string, this will remove '[\n]' at the end and return the remaining part of the string. If there is a '[\n]', the input string is returned. *)

val gensym : string -> string
(** Given a string x, this will add '-N' where N is an integer starting with 1. N will be chosen such that for subsequent calls of gensym with the same string x, N increases by 1 from call to call. The purpose of this function is to generate unique names that are based on a given string x. *)

val md5: string -> string
(** Maps a string to its hexstring representation of its md5 sum. *)

val for_permutations : int -> (int array -> unit) -> unit

val permutation_sign : int array -> int

val guess_limit : ?sum:bool -> float array -> float

val graph_components :
  ?fun_join_data:('a -> 'a -> 'a) ->
  (produce:(int -> int -> 'a -> unit) -> unit) -> (int, int array * 'a) Hashtbl.t

val sleep_float: float -> unit

val expire_dir : ?silent:bool -> string -> float -> unit

val timing : ?tagfun:('a -> string) -> ?channel:out_channel -> ('a -> 'b) -> 'a -> 'b

val multifor : int array -> (int -> int array -> unit) -> unit

val array_multiinit : int array -> (int -> int array -> 'a) -> 'a array

(** Functions for simple ad-hoc parallelization of uniform workloads on
    2-way and 4-way machines through intermediate forking: *)

val split_range : int -> int -> int -> (int * int) array

val compute_uniform_workload_forked :
  ?bailout:(string -> 'a) ->
  fun_combine:('b -> 'c -> 'c) -> (unit -> 'c) array -> 'c

val parse_or_error : ?fun_fail:(string -> 'a) -> 'b -> ('b -> Lexing.lexbuf -> 'a) -> string -> 'a
val token_stream : (Lexing.lexbuf -> 'a) -> 'a -> string -> 'a list

val gc_info: bool -> string

(* Low-level extensions *)

val encode_double_into_7bit_string: string -> float -> unit 

val decode_7bit_string_to_double: string -> float 

val put_double_into_string: string -> float -> unit

val with_cpu_instructions_counted: ('a -> 'b) -> 'a -> (float -> 'b -> 'c) -> 'c

val funcall_printing_cpu_instructions : ?channel:out_channel -> string -> ('a -> 'b) -> 'a -> 'b

val funcall_logging_cpu_instructions : (float -> unit) -> ('a -> 'b) -> 'a -> 'b

val snapping_return_value : (('a -> unit) -> unit) -> 'a

val debugprint_entity: 'a -> unit
