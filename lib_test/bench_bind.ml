open Core.Std
open Core_extended.Std.Bench

module F = struct
  let ok x = Ok x

  let bind_cascade () =
    let open Or_error.Monad_infix in
  (* let open Result.Monad_infix in *)
    let x = () in
    ok x >>= fun x ->
    ok x >>= fun x ->
    ok x >>= fun x ->
    ok x >>= fun x ->
    ok x >>= fun x ->
    ok x >>= fun x ->
    ok x >>= fun x ->
    ok x >>= fun x ->
    ok x

  let match_cascade () =
    let x = () in
    match ok x with Error _ as x -> x | Ok x ->
    match ok x with Error _ as x -> x | Ok x ->
    match ok x with Error _ as x -> x | Ok x ->
    match ok x with Error _ as x -> x | Ok x ->
    match ok x with Error _ as x -> x | Ok x ->
    match ok x with Error _ as x -> x | Ok x ->
    match ok x with Error _ as x -> x | Ok x ->
    match ok x with Error _ as x -> x | Ok x ->
    ok x
end


let () =
  bench
    ~trials:100
    ~time_format:`Ns
    [ Test.create ~size:8 ~name:"bind"
        (fun () -> ignore (F.bind_cascade () : unit Or_error.t))
    ; Test.create ~size:8 ~name:"match"
      (fun () -> ignore (F.match_cascade () : unit Or_error.t))
    ]
