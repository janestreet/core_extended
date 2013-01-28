open Core.Std

(* stands for a comparison like would be returned by polymorphic compare *)
type comparison = int

(* 'comparable is supposed to indicate a type on which polymorphic compare will not raise
   an exception *)
val cmp_fasc : ('a -> 'comparable) -> 'a -> 'a -> comparison
val cmp_fdesc : ('a -> 'comparable) -> 'a -> 'a -> comparison

val maxf : ('a -> 'comparable) -> 'a -> 'a -> 'a
val minf : ('a -> 'comparable) -> 'a -> 'a -> 'a

val in_order : 'a list -> cmp:('a -> 'a -> comparison) -> bool

val max_len : key:('a -> string) -> 'a array -> int

(* [f] should return the comparison of its argument with the "good" value. *)
val bsearch     : f:(int -> comparison) -> low:int -> high:int -> int option
val bsearch_opt : f:(int -> comparison) -> low:int -> high:int -> int option
val bsearch_exn : f:(int -> comparison) -> low:int -> high:int -> int       

type poly_comparison = [`Low | `Good | `High]
val bsearch2     : f:(int -> poly_comparison) -> low:int -> high:int -> int option
val bsearch2_opt : f:(int -> poly_comparison) -> low:int -> high:int -> int option
val bsearch2_exn : f:(int -> poly_comparison) -> low:int -> high:int -> int       

val bsearch_val     : f:(int -> comparison * 'a) -> low:int -> high:int -> (int * 'a) option
val bsearch_val_opt : f:(int -> comparison * 'a) -> low:int -> high:int -> (int * 'a) option
val bsearch_val_exn : f:(int -> comparison * 'a) -> low:int -> high:int -> (int * 'a)       
