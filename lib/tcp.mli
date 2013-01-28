open Core.Std ;;

(** [with_conn] opens a connection to the TCP service on [host] [port] and
  if successful calls [f] with In_channel.t and Out_channel.t as arguments.
  The return value of [f] will be returned by [with_conn].

  [with_conn] raises Connect_timeout if the connection attempt times out.
  Unix_error will be raised if there are any other connect errors.
  Any exceptions raised by [f] will be re-raised, which may include
  Unix_errors raised by underlying channel read/write calls.

  The [In_channel.t], [Out_channel.t], and underlying socket
  will be closed by with_conn before it returns.
 *)

exception Connect_timeout

val with_connection :
    ?timeout:Time.Span.t
  -> host:string
  -> port:int
  -> f:(In_channel.t -> Out_channel.t -> 'a)
  -> unit
  -> 'a

