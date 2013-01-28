open Core.Std

type 'a t = {
  l : 'a list;
  r : 'a list;
}

val create : 'a list -> 'a list -> 'a t
val drop_before : 'a t -> ('a * 'a t) option
val drop_after : 'a t -> ('a * 'a t) option
(* returns l unreversed *)
val drop_all_before : 'a t -> ('a list * 'a t) option
val drop_all_after : 'a t -> ('a list * 'a t) option
val insert_before : 'a t -> 'a -> 'a t
val insert_after : 'a t -> 'a -> 'a t
val previous : 'a t -> 'a t option
val next : 'a t -> 'a t option
val replace_left : 'a t -> 'a list -> 'a t
val replace_right : 'a t -> 'a list -> 'a t
