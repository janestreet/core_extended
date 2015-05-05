(** Extensions to [Core.Gc].*)

(** [without_compactions f a] will call f a so that Gc.compact is never called
    during its execution, then restore compactions to the previous setting. *)
val without_compactions : ?logger:(string -> unit) -> f:('a -> 'b) -> 'a -> 'b

(*
  (* cdaemon can't seem to compile the C bindings, even though I can. *)

(** [print_roots] will trace the ocaml stack/global runtime roots and
    print data types it recognizes to stderr.

    Its purpose is to aid discovery of space leaks, calling it as a normal
    matter of course is not recommended as it holds the global lock.
 *)
val print_roots : unit -> unit
*)
