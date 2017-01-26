open! Core

(* Generic mapping and folding.

   The types are strange, you should ignore them.  Just look at some examples:

       open Generic

       map ([[1];[2;3];[3;4]] >: __ list) ~f:List.length
       (* Result: [1;2;2] *)

       map ([[1];[2;3];[3;4]] >: __) ~f:List.length
       (* Result: 3 *)

       length ([[1];[2;3];[3;4]] >: __ list list)
       (* Result: 5 *)

       length ([[1];[2;3];[3;4]] >: __ list)
       (* Result: 3 *)

       length ( [1;2;3;4] >: __ )
       (* Result: 1 *)

       [ "asdf"; "a" ] >: string list >>| fun c ->
       Char.uppercase c
       (* Result: [ "ASDF"; "A" ] *)

       to_list ([ [ Error "foo"]; [Ok (); Error "banana"]] >: __ error list list)
       (* Result: [ "foo"; "banana" ] *)

       to_list ([Some (1,'s'); None; Some (3,'a')] >: __ first option list)
       (* Result: [1;3] *)
*)

type ('a,'b) c = ('a -> 'b) -> 'b

val ( >: ) : 'a -> (('b -> 'b) -> 'c -> 'a -> 'd) -> 'c -> 'd

val map : ('a -> 'b) -> f:'a -> 'b
val fold : (('a -> 'a) -> _) -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val iter : (('a -> 'a) -> _) -> f:('a -> unit) -> unit
val length : (('a -> 'a) -> _) -> int
val to_list : (('a -> 'a) -> _) -> 'a list
val (>>|) : ('a -> 'b) -> 'a -> 'b

val __ : ((('a -> 'b) -> 'a -> 'b) -> 'c) -> 'c
val list :
  ('a -> 'b -> 'c) ->
  (('a -> 'b list -> 'c list) -> 'd) -> 'd
val option :
  ('a -> 'b -> 'c) ->
  (('a -> 'b option -> 'c option) -> 'd) -> 'd
val ok :
  ('a -> 'b -> 'c) ->
  (('a -> ('b, 'd) Result.t -> ('c, 'd) Result.t) ->
   'e) ->
  'e
val error :
  ('a -> 'b -> 'c) ->
  (('a -> ('d,'b) Result.t -> ('d,'c) Result.t) ->
   'e) ->
  'e
val string :
  (((char -> char) -> string -> string) -> 'a) -> 'a
