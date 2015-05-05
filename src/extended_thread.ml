open Core.Std

let safe_create f =
  Thread.create
    (fun () ->
       try
         f ()
       with e ->
         eprintf "In thread %i\n" (Thread.id (Thread.self ()));
         if Printexc.backtrace_status () then
           Printexc.print_backtrace stderr;
         prerr_endline (Exn.to_string e);
         exit 1)
    ()
