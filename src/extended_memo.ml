open Core

let general_rec g =
  let fref = ref (fun _ -> assert false) in
  let f = Memo.general (fun x -> g !fref x) in
  fref := f;
  f
;;

let reentrant_unit f =
  let lock = Nano_mutex.create () in
  let memo = Memo.unit f in
  fun () ->
    Nano_mutex.lock_exn lock;
    protect ~f:memo ~finally:(fun () -> Nano_mutex.unlock_exn lock)

