open Core.Std


(* Useful debugging functions working at a low level.

   NOT FOR USE IN PRODUCTION CODE.

   Functions here that stop programs with SIGSTOP are designed to be used
   in conjunction with "gdb -p". *)

(* Stop the calling process or any forked children (with SIGSTOP) upon
   reception of SIGBUS.  This overrides any other Caml-installed handler. *)
val stop_upon_sigbus : unit -> unit

(* Stop the calling process or any forked children (with SIGSTOP) upon
   reception of SIGSEGV.  This overrides any other Caml-installed handler. *)
val stop_upon_sigsegv : unit -> unit

(* Stop the calling process or any forked children (with SIGSTOP) upon
   reception of SIGPIPE.  This overrides any other Caml-installed handler. *)
val stop_upon_sigpipe : unit -> unit

(* Stop the calling process or any forked children upon normal process
   termination. *)
val stop_upon_exit : unit -> unit

(* Stop the calling process right away.

   This is effectively the same as setting a gdb breakpoint, but is likely
   easier to use, especially if the program should only be stopped under
   certain circumstances.

   One example of the use of this function is to replace "raise" by a call
   to [stop_me_now]; this enables you to see the stack trace leading up to
   the raise.  (This might be useful if many functions can call a single
   raise point, and you don't know which caller is triggering it.)  You can
   of course view backtraces using OCAMLRUNPARAM=b, but that can be misleading:
   for example if an exception is re-raised then the backtrace will show it
   as raised only at the most recent raise, and previous frames (including the
   original raise) won't be named.  Using [stop_me_now] on the original raise
   also has the advantage that you don't need to adjust any "with" clauses
   between the raise point and the top level, which would have to be removed
   to see a backtrace with OCAMLRUNPARAM=b. *)
val stop_me_now : unit -> unit

(** A canary thread starts two threads, one in caml and one in C.  The caml
    thread tries to take the caml lock every [check_interval], and immediately
    releases it once it succeeds. The C thread checks that your thread is
    getting the caml lock within [max_wait], and if it isn't it stops the
    program (see stop_me_now).  You can then inspect your program with gdb and
    hopefully determine what caused it to block.  It is safe to continue a
    program stopped by the canary thread, though there is no special handling in
    the canary thread to support this, so it may stop your program again.  If
    you do manage to continue, the canary thread should continue to work.

    It is not safe to call this function more than once, in a given program an
    exception will be raised if you do (but a second canary will not be
    started).
*)
val start_canary_thread :
  max_wait:Time.Span.t
  -> check_interval:Time.Span.t
  -> unit
