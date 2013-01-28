(** Astract iterators. *)

(* Abstract iterator - a source of objects
 * Common use cases:
 * == run FUNC on each line in FILE and collect results in a list:
 *    read_wrap FILE ~f:(fun ic -> to_list (of_channel ic) ~f:FUNC)
 * == map objects in array into a list without an intermediate list/array
 *    to_list ~f (of_array a)
 *)

type 'a t

(** get the next element of the iterator *)
val next : 'a t -> 'a option
val next_exn : 'a t -> 'a

(** get the position in the iterator either None or Some x in [0;1] *)
val progress : 'a t -> float option

(** convert the progress return value to a string: None->"", Some->" 33%" *)
val progress_string : float option -> string

(** iterate over the iterator: call f on each element *)
val i : 'a t -> f:('a -> unit) -> unit

(** concatenate a list of iterators *)
val concat : 'a t list -> 'a t

(** fold over the iterator: call f on each element and return the accumulator *)
val reduce : 'a t -> init:'i -> f:('i -> 'a -> 'i) -> 'i

(** transform the iterator *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** fold is the same as reduce *)
val fold : 'a t -> init:'i -> f:('i -> 'a -> 'i) -> 'i
val unfold : init:'i -> f:('i -> ('a * 'i)) -> stop:'i -> 'a t

(** find an element that satisfies the predicate *)
val find : 'a t -> f:('a -> bool) -> 'a

  
(** iterate over elements that satisfy the predicate *)
val filter : 'a t -> f:('a -> bool) -> 'a t

(** evaluate a predicate over the entire iterator *)
val for_all : 'a t -> f:('a -> bool) -> bool
val exists : 'a t -> f:('a -> bool) -> bool

(** create an iterator from an iterating function *)
val t : ?progress:(unit -> float option) -> (unit -> 'a option) -> 'a t

(** an iterator that halts immediately *)
val empty : 'a t

(** create an iterator that may iterate over one value *)
val of_opt : 'a option -> 'a t

(** create an iterator that will go over list elements *)
val of_list : 'a list -> 'a t

(** iterate a function and collect results to a list *)
val to_list : 'a t -> f:('a -> 'b) -> 'b list
val to_list_opt : 'a t -> f:('a -> 'b option) -> 'b list

(** create an iterator that will go over array elements *)
val of_array : 'a array -> 'a t

(** iterate a function and collect results to a array *)
val to_array : 'a t -> f:('a -> 'b) -> 'b array
val to_array_opt : 'a t -> f:('a -> 'b option) -> 'b array

(** create a progress function for an input channel *)
val channel_progress : ?total:int64 -> in_channel -> (unit -> float option)

(** create an iterator that will read from file using f *)
val of_channel : ?total:int64 -> in_channel -> f:(in_channel -> 'a) -> 'a t

(** call f on channel until End_of_file *)
val channel : in_channel -> f:(in_channel -> unit) -> unit
