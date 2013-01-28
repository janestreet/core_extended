open Core.Std

let default_callback ~elapsed =
  eprintf "tick thread stopped for %s\n%!" (Time.Span.to_string elapsed)
;;

let start ?(callback = default_callback) () =
  let r = ref (Time.now ()) in
  ignore
    (Thread.create (fun () ->
      while true do
        Time.pause (Time.Span.of_ms 10.0);
        let now = Time.now () in
        let elapsed = Time.diff now !r in
        r := now;
        if Time.Span.(>) elapsed (Time.Span.of_ms 50.) then callback ~elapsed;
      done)
       () : Thread.t);
;;
