(** Extensions to [Core.Core_list].*)

open Core.Std

(** [of_option o] returns a list that is empty if [o] is None, otherwise it is a singleton
    list. Useful to get filter_map-like behavior in the context of something like a
    concat_map. *)
val of_option : 'a option -> 'a list

(** [set_inter l1 l2] returns a list without duplicates of all elements of l1 that are in l2 *)
val set_inter : 'a list -> 'a list -> 'a list

(** [set_diff l1 l2] returns a list of all elements of l1 that are not in l2
*)
val set_diff : 'a list -> 'a list -> 'a list

(** [classify l ~equal ~f] elements [x] and [y] of list [l] are assigned to the
    same class iff [equal (f x) (f y)] returns true. The default for [equal] is ( = ) *)
val classify : ?equal:('b -> 'b -> bool) -> f:('a -> 'b) -> 'a list ->
  ('b * 'a list) list

(** [take_while xs f] produces the longest prefix of [xs] on which [f] is always true. *)
val take_while : 'a list -> ('a -> bool) -> 'a list

(** [split_while xs f] splits [xs] into sublists [ys] and [zs] such that [ys] is
        the longest prefix on which [f] is true, and [zs] is the remaining elements. *)
val split_while : 'a list -> ('a -> bool) -> ('a list * 'a list)

(** [intersperse] returns a list with the given element placed between every two
  original elements.  e.g.  intersperse [1;2;3] 0  =  [1; 0; 2; 0; 3] *)
val intersperse : 'a list -> 'a -> 'a list

(** [enumerate_from n xs] returns a list of pairs constructed by pairing an
    incrementing counter, starting at [n], with the elements of [xs].
    e.g.  enumerate_from 1 [a,b,c]  =  [a,1; b,2; c,3] *)
val enumerate_from : int -> 'a list -> ('a * int) list

(** fold_left_term is like fold_left, except that you can halt early.
    The function to be folded should return a bool along with the new
    accumulator.  True indicates that it should continue, false means
    it should halt *)
val fold_left_term : 'a list -> f:('b -> 'a -> [`Final of 'b | `Continue of 'b]) -> init:'b -> 'b

val max : ?cmp:('a -> 'a -> int) -> 'a list -> 'a option
val min : ?cmp:('a -> 'a -> int) -> 'a list -> 'a option

val max_exn : ?cmp:('a -> 'a -> int) -> 'a list -> 'a
val min_exn : ?cmp:('a -> 'a -> int) -> 'a list -> 'a

(**
   Find the longest common subsequence between two list.
*)
val lcs : 'a list -> 'a list -> 'a list

(**
  Numbers the elements in a list by occurence:

   [[a;b;c;a;d] -> [(a,0);(b,0);(c,0);(a,1);(d,0)]]

*)

val number : 'a list -> ('a * int) list

(**
   Merges several list trying to keep the order in which the elements appear.
   The elements of the individual are not deduped.

   multimerge [[[a;b;d;a] [b;c;d]] -> [a;b;c;d;a]]
*)
val multimerge : 'a list list -> 'a list
val multimerge_unique : 'a list list -> 'a list

(**
   Takes a list of [`key*`value lists] and returns a
   header * table_body body that is obtained by splitting the lists and
   re-oddering the terms (so that they all have the same header).

   If [null_value] is not specified and the rows have different keys
   the function will raise an exception.
   [
   square ~null
   [[(1,a_1);(2,b_1);(4,c_1)];
    [(3,a_2)];
    [(0,a_3);(1,b_3);(2,c_3);(3,d_3);(4,e_3)]]
   =
   ([0   ;1   ;2   ;3   ;4],
   [[null;a_1 ;b_1 ;null;c_1 ]
    [null;null;null;a_2 ;null]
    [a_3 ;b_3 ;c_3 ;d_3 ;e_3 ]])
   ]
*)
val square : ?null:'v -> ('k * 'v) list list -> 'k list * 'v list list
val square_unique : ?null:'v ->
  ('k * 'v) list list ->
  'k list * 'v list list

val equal : equal:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val compare : cmp:('a -> 'b -> int) -> 'a list -> 'b list -> int
