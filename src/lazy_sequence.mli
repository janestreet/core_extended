open! Core

(* A lazy list, but without memoization.

   The lack of memoization prevents any problems with space leaks. It's memory-safe to
   hold on to even a very long lazy sequence and perform operations or iterate through
   it multiple times.

   However, the lack of memoization also means that when iterating through a sequence
   multiple times, the data will be reloaded/regenerated every time. For example,
   iterating twice through a sequence produced by reading a file will open, read, and
   close the file twice.

   Generally speaking, functions below that return something of type [t] are lazy,
   whereas functions that return an actual value will force the sequence.
*)

type +'a t

(* This includes functions like [iter], [fold], [length], [to_list]... *)
include Container.S1 with type 'a t := 'a t

val iteri: 'a t -> f:(int -> 'a -> unit) -> unit
val foldi: 'a t -> init:'state -> f:(int -> 'state -> 'a -> 'state) -> 'state
val map: 'a t -> f:('a -> 'b) -> 'b t
val mapi: 'a t -> f:(int -> 'a -> 'b) -> 'b t
val filter: 'a t -> f:('a -> bool) -> 'a t
val filter_map: 'a t -> f:('a -> 'b option) -> 'b t
val filter_mapi: 'a t -> f:(int -> 'a -> 'b option) -> 'b t
val folding_map: 'a t -> init:'state -> f:('state -> 'a -> ('state * 'b)) -> 'b t
val fold_map: 'a t -> init:'state -> f:('state -> 'a -> ('state * 'b)) -> 'b t
[@@deprecated "[since 2017-03] Use folding_map instead"]
val folding_filter_map: 'a t -> init:'state -> f:('state -> 'a -> ('state * 'b option)) -> 'b t
val filter_fold_map: 'a t -> init:'state -> f:('state -> 'a -> ('state * 'b option)) -> 'b t
[@@deprecated "[since 2017-03] Use folding_filter_map instead"]
val concat: 'a t t -> 'a t
val concat_seq_list: 'a t list -> 'a t
val concat_list_seq: 'a list t -> 'a t
val concat_map: 'a t -> f:('a -> 'b list) -> 'b t

(* Will only force as much of the sequence as necessary, but may still be expensive to
   call repeatedly, particularly if forcing it requires some initialization time. *)
val hd: 'a t -> 'a option
val last: 'a t -> 'a option
val nth: 'a t -> int -> 'a option (* n=0 means first element *)

(* If elements are dropped from the front of the sequence, it's still the case that every
   time the sequence is forced, the cost of computing those elements will be present. *)
val tl: 'a t -> 'a t (* tl of the empty sequence is the empty sequence *)
val take: 'a t -> int -> 'a t
val drop: 'a t -> int -> 'a t
val append: 'a t -> 'a t -> 'a t
val sub: 'a t -> pos:int -> len:int -> 'a t

(* Will not filter through the rest of the sequence after stopping, so can be
   slightly more efficient than filter_map and can be used on infinite sequences *)
val filter_map_partial: 'a t -> f:('a -> [ `Continue of 'b option | `Stop ]) -> 'b t

(* Pairs up all the a's and b's. If one list ends before the other, then will return
   None for the elements of the list that ended *)
val zip_full: 'a t -> 'b t -> ('a option * 'b option) t

(* [length_if_at_most ~max t] returns Some len if [len = length t <= max], and otherwise
   returns None. Non-lazy, but walks through only as much of the sequence as necessary. *)
val length_if_at_most: max:int -> _ t -> int option

(* [length_bounded_by ~min ~max t] returns true if [min <= length t] and [length t <= max]
   When [min] or [max] are not provided, the check for that bound is omitted.
   Non-lazy, but walks through only as much of the sequence as necessary. *)
val length_bounded_by: ?min:int -> ?max:int -> _ t -> bool

val of_list: 'a list -> 'a t
val of_array: 'a array -> 'a t

(* [init f] Creates a sequence by lazily evaluating [f] on the infinite sequence
   [0; 1; 2; 3; ...], stopping if/when [f] returns None. *)
val init: (int -> 'a option) -> 'a t

(* [read_lines filename] Returns a lazy sequence of all the lines in the given file *)
val read_lines: string -> string t


(* NOT LAZY! Will immediately force the entire sequence into memory and return the
   forced version of the sequence. Equivalent to [of_list (to_list t)] *)
val force: 'a t -> 'a t


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
     automatically be closed.
     It's also okay to call other functions on the iterator in the middle of the iter or
     fold. For example, calling [ignore (get t)] in the middle of an iter or fold will
     cause it to skip the next element. Calling [close] will cause the iter or fold to
     terminate after f returns. *)
  val iter: 'a t -> f:('a -> unit) -> unit
  val fold: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  (* Copied iterators share the same underlying instance of the sequence. So all copies of
     an iterator can be advanced independently while still only loading the sequence once
     (such as from a file).

     Note that the portion of the sequence between the leftmost non-closed iterator and
     the rightmost point in the sequence reached by any iterator (even if later closed)
     will be kept in memory, which could be a concern for very long sequences.

     [close] will close only the existing iterator but not affect copies.
     [with_sequence] will close the shared underlying instance of the sequence upon return
     for *all* copies of the iterator that it creates. However, the parts of the sequence
     loaded into in memory might still be accessible from the iterators or their copies
     even after it returns. *)
  val copy: 'a t -> 'a t
end

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
     Lazy_sequence.protect ~finally:(fun () -> In_channel.close ic) (fun () ->
       let (==>) = Lazy_sequence.(==>) in
       let rec loop () =
         match In_channel.input_line ic with
         | None -> Lazy_sequence.empty
         | Some line -> line ==> fun () -> loop ()
       in
       loop ()
     ))
*)

val empty: _ t (* Empty lazy sequence *)
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
   original subsequence will be protected by the [finally]. *)
val protect: finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
