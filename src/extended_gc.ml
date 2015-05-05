open Core.Std

(* execute function supressing compactions. the doc says:
   mutable max_overhead : int;
   Heap compaction is triggered when the estimated amount of "wasted" memory
   is more than max_overhead percent of the amount of live data.
   If max_overhead is set to 0, heap compaction is triggered at the end of each
   major GC cycle (this setting is intended for testing purposes only).
   If max_overhead >= 1000000, compaction is never triggered.
   Default: 500. *)
let without_compactions ?logger ~f a =
  (* note that f may call get/set itself, and the fields in the Gc.control
     struct are mutable, so it does not make much sense to save it *)
  protectx (Gc.get ()).Gc.Control.max_overhead
    ~f:(fun _ ->
      Gc.tune ?logger ~max_overhead:1000000 ();
      f a)
    ~finally:(fun max_overhead -> Gc.tune ?logger ~max_overhead ())

(* external print_roots : unit -> unit = "print_roots" ;; *)
