open Core.Std

(* A lazy list, but without memoization.

   The lack of memozation prevents any problems with space leaks. It's memory-safe to
   hold on to even a very long lazy sequence and perform operations or iterate through
   it multiple times.

   However, the lack of memoization also means that when iterating through a sequence
   multiple times, the data will be reloaded/regenerated every time. For example,
   iterating twice through a sequence produced by reading a file will open, read, and
   close the file twice.
*)

type 'a t

include Container.S1 with type 'a t := 'a t

val map: 'a t -> f:('a -> 'b) -> 'b t
val filter: 'a t -> f:('a -> bool) -> 'a t
val filter_map: 'a t -> f:('a -> 'b option) -> 'b t
val fold_map: 'a t -> init:'state -> f:('state -> 'a -> ('state * 'b)) -> 'b t
val filter_fold_map:
  'a t -> init:'state -> f:('state -> 'a -> ('state * 'b option)) -> 'b t

(* Will not filter through the rest of the sequence after stopping, so can be
   slightly more efficient than filter_map *)
val filter_map_partial: 'a t -> f:('a -> [ `Continue of 'b option | `Stop ]) -> 'b t

(* Pairs up all the a's and b's. If one list ends before the other, then will return
   None for the elements of the list that ended *)
val zip_full: 'a t -> 'b t -> ('a option * 'b option) t

val concat: 'a t t -> 'a t
val concat_seq_list: 'a t list -> 'a t
val concat_list_seq: 'a list t -> 'a t

val concat_map: 'a t -> f:('a -> 'b list) -> 'b t

(* [length_bounded_by ~min ~max t] returns true if [min <= length t] and [length t <= max]
   When [min] or [max] are not provided, the check for that bound is omitted.
   Non-lazy, but walks through only as much of the sequence as necessary. *)
val length_bounded_by: ?min:int -> ?max:int -> _ t -> bool

(* For walking through a lazy sequence, element by element *)
module Iterator : sig
  type 'a seq = 'a t
  type 'a t

  val create: 'a seq -> 'a t
  val close: _ t -> unit
  val with_sequence: 'a seq -> f:('a t -> 'b) -> 'b

  val has_next: _ t -> bool   (* false once the end of the sequence is reached *)

  (* These may raise an exception in the case that the producer of the sequence raises.
     If this is the case, then the iterator will automatically be closed and the
     exception reraised here. *)
  val peek: 'a t -> 'a option (* returns None when the end of the sequence is reached *)
  val get: 'a t -> 'a option  (* returns None when the end of the sequence is reached *)
  val get_exn: 'a t -> 'a

  (* Walk through the iterator, consuming elements. Upon exception, the iterator will
     automatically be closed *)
  val iter: 'a t -> f:('a -> unit) -> unit
  val fold: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
end

val of_list: 'a list -> 'a t

(* [init f] Creates a sequence by lazily evaluating [f] on the infinite sequence
   [0; 1; 2; 3; ...], stopping if/when [f] returns None. *)
val init: (int -> 'a option) -> 'a t

(* [read_lines filename] Returns a lazy sequence of all the lines in the given file *)
val read_lines: string -> string t


(* Producing a lazy sequence manually ------------------------------------------------ *)

(* Support is provided for using mutable state to generate the sequence (such as reading
   files, etc.). This is safe because once a user begins traversing the sequence, it is
   not possible in this interface to copy the state of the sequence to be retraversed
   from that point multiple times.

   Producers using mutable state should make sure that all initialization of the mutable
   state is wrapped with [initialize], so that if the full sequence is traversed
   several times, the proper initialization happens each time. Additionally, [protect]
   should be used for any resources that should be closed at the end of a sequence or on
   exception.

   Example use involving mutable state, [initialize], and [protect]:

   let read_lazy file = Lazy_sequence.initialize (fun () ->
     let ic = In_channel.create file in
     Lazy_sequence.protect ~finally:(fun () -> In_channel.close ic) (
       let (==>) = Lazy_sequence.(==>) in
       let rec loop () =
         match In_channel.input_line ic with
         | None -> Lazy_sequence.stop
         | Some line -> line ==> fun () -> loop ()
       in
       loop ()
     ))
*)

val stop: _ t (* Nil *)
val (==>): 'a -> (unit -> 'a t) -> 'a t (* Cons *)
val (==>>): 'a list -> (unit -> 'a t) -> 'a t (* Multi-cons, lazy even if list is empty *)

(* [initialize f] produces a lazy sequence where f is called when the first element
   of the sequence is requested. This should be used for initialization of mutable state
   (such as opening files to be read from) *)
val initialize: (unit -> 'a t) -> 'a t

(* [protect ~finally f] produces a lazy sequence where f is called when the first element
   of the sequence is requested, and where [finally] is called at the end of the sequence
   or when an iterator is closed that has forced at least one element of the sequence.

   If additional elements are cons-ed on the head of returned sequence, only the
   original subsequence will be protected by the [finally].
*)
val protect: finally:(unit -> unit) -> (unit -> 'a t) -> 'a t

(* type 'a referentially_transparent = private 'a t *)
(* val empty : _ referentially_transparent *)
(* val put : 'a t -> ... *)
(* val put_rt : 'a rt -> ... *)
(* val is_transparent__unsafe : 'a t -> 'a referentially_transparent *)
(* val is_empty: _ referentially_transparent -> bool
 * val get: 'a referentially_transparent -> [`Next of 'a * 'a referentially_transparent | `Done]
 * val get_exn: 'a referentially_transparent -> 'a * 'a referentially_transparent *)
