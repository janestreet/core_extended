open Core.Std

(** role assignment for systems with a primary and backup instances that
    switch roles daily for ease of deploying new versions. *)

(* 
   Note: most weekends are happily two days long, so that primary and
   backup are properly swapped between Friday and Monday.  This breaks
   down on three-day weekends, but one shouldn't deploy leading into
   one of those anyway.
*)

type 'a t = { primary : 'a; backup : 'a }

val select : Date.t -> slot1:'a -> slot2:'a -> 'a t

