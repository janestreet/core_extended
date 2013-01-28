open Core.Std

(* Essentially a lazy list, but with a few differences.

   Unlike lazy lists, laziness is achieved using thunks, *without* memoization. While this
   has some downsides, it has the benefit of preventing any problems with space leaks.
   It's safe to hold on to a very long lazy sequence even as you iterate through it, or
   reuse it multiple times.

   Lazy sequences may be infinite.

   Support is provided for producers that wish to use mutable state to generate the
   sequence (such as reading files, etc.). This is safe because once a user begins
   traversing the sequence, it is not possible to copy the state of the sequence at that
   point to be re-used multiple times. The [Iterator] module allows a user to stop in the
   middle of the sequence, but the iterator itself can only make one pass through the
   sequence and cannot be copied or reused.

   Producers using mutable state should make sure that all initialization of the mutable
   state is wrapped with [initialize], so that if the full sequence is traversed
   several times, the proper initialization happens each time. Additionally, [protect]
   can be used for any resources that should be closed at the end of a sequence or on
   exception.

   Example use involving mutable state, [initialize], and [protect]:

   let read_lazy file = Lazy_sequence.initialize (fun () ->
     let ic = In_channel.create file in
     Lazy_sequence.protect ~finally:(fun () -> In_channel.close ic) (
       let (==>) = Lazy_sequence.(==>) in
       let rec loop () =
         match In_channel.input_line file with
         | None -> Lazy_sequence.stop
         | Some line -> line ==> fun () -> loop ()
       in
       loop ()
     ))
*)

type 'a t

val map: 'a t -> f:('a -> 'b) -> 'b t
val filter: 'a t -> f:('a -> bool) -> 'a t
val filter_map: 'a t -> f:('a -> 'b option) -> 'b t
val fold_map: 'a t -> init:'state -> f:('state -> 'a -> ('state * 'b)) -> 'b t
val filter_fold_map:
  'a t -> init:'state -> f:('state -> 'a -> ('state * 'b option)) -> 'b t


include Container.S1 with type 'a t := 'a t


module Iterator : sig
  type 'a seq = 'a t
  type 'a t


  val create: 'a seq -> 'a t
  val close: _ t -> unit
  val with_sequence: 'a seq -> f:('a t -> 'b) -> 'b

  val has_next: _ t -> bool   (* false once the end of the sequence is reached *)
  val peek: 'a t -> 'a option (* returns None when the end of the sequence is reached *)
  val get: 'a t -> 'a option  (* returns None when the end of the sequence is reached *)
  val get_exn: 'a t -> 'a

  val iter: 'a t -> f:('a -> unit) -> unit
  val fold: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
end


(* Producing a lazy sequence ------------------------------------------------------- *)

val stop: _ t (* Nil *)
val (==>): 'a -> (unit -> 'a t) -> 'a t (* Cons *)
val (==>>): 'a list -> (unit -> 'a t) -> 'a t (* still lazy even if list is empty *)

(* [initialize f] produces a lazy sequence where f is called when the first element
   of the sequence is requested. This should be used for initialization of mutable state
   (such as opening files to be read from) *)
val initialize: (unit -> 'a t) -> 'a t

(* [protect ~finally t] produces a lazy sequence that is the same as [t] but where
   [finally] is called at the end of the sequence or when an iterator is closed that has
   forced at least one element of the sequence.

   If additional elements are cons-ed on the sequence thereafter, only the original
   subsequence will protected by the [finally].
*)
val protect: finally:(unit -> unit) -> 'a t -> 'a t

(* [init f] Creates a sequence by lazily evaluating [f] on the infinite sequence
   [0; 1; 2; 3; ...], stopping if/when [f] returns None. *)
val init: (int -> 'a option) -> 'a t

val of_list: 'a list -> 'a t


(* type 'a referentially_transparent = private 'a t *)
(* val empty : _ referentially_transparent *)
(* val put : 'a t -> ... *)
(* val put_rt : 'a rt -> ... *)
(* val is_transparent__unsafe : 'a t -> 'a referentially_transparent *)
(* val is_empty: _ referentially_transparent -> bool
 * val get: 'a referentially_transparent -> [`Next of 'a * 'a referentially_transparent | `Done]
 * val get_exn: 'a referentially_transparent -> 'a * 'a referentially_transparent *)


(* XCR moconnor for dwu.  Consider: *)
(* in ml: *)
(*   type 'a t =
     | Nil
     | Strict_cons of 'a * 'a t
     | Lazy_cons of (unit -> 'a * 'a t)
 *
 * let empty = Nil
   let put_lazy x t = Fun (fun () -> Cons (x,t))
   let put_strict x t = Cons (x,t)

 in mli:

 * type 'a t
 *
 * val empty : (_,_) t
 * val put : 'a -> (unit -> 'a t) -> 'a t

  * val of_fun : (unit -> 'a t) -> 'a t
*)


