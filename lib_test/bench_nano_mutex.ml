open Core.Std

module Bench = Core_extended.Std.Bench

module type Mutex = sig
  type t
  val create : unit -> t
  val lock : t -> unit
  val unlock : t -> unit
end

let concat = String.concat

let make ~name (m : (module Mutex)) =
  let module M = (val m : Mutex) in
  [ concat [ name; " create"], (fun () -> ignore (M.create ()));
    concat [ name; " lock/unlock"],
    let l = M.create () in
    (fun () -> M.lock l; M.unlock l);
  ]
;;

module Nano_mutex : Mutex = struct
  include Core.Std.Nano_mutex

  let lock = lock_exn
  let unlock t = unlock_exn t
end

let () =
  Or_error.ok_exn Bench.bench
    (List.map ~f:(fun (name, thunk) -> Bench.Test.create ~name thunk)
       (
         make ~name:"Caml.Mutex" (module Caml.Mutex : Mutex)
         @ make ~name:"Core.Mutex" (module Core.Std.Mutex : Mutex)
         @ make ~name:"Agnostic_mutex" (module Agnostic_mutex : Mutex)
         @ make ~name:"Nano_mutex" (module Nano_mutex : Mutex)
       ))
;;
