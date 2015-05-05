open Core.Std
open Unix

exception Timeout

let h_name_of_sockaddr = function
  | ADDR_INET (inet_addr, _) -> (Host.getbyaddr_exn inet_addr).Host.name
  | ADDR_UNIX _ -> failwith "h_name_of_sockaddr: ADDR_UNIX"

let string_of_sockaddr = function
  | ADDR_INET (inet_addr, _) -> Inet_addr.to_string inet_addr
  | ADDR_UNIX file -> file

let h_name_or_string_of_sockaddr = function
  | ADDR_INET (inet_addr, _) ->
      (try (Host.getbyaddr_exn inet_addr).Host.name
      with Not_found -> Inet_addr.to_string inet_addr)
  | ADDR_UNIX _ -> failwith "h_name_or_string_of_sockaddr: ADDR_UNIX"

let inet_addr_of_sockaddr = function
  | ADDR_INET (inet_addr, _) -> inet_addr
  | ADDR_UNIX _ -> failwith "inet_addr_of_sockaddr: ADDR_UNIX"

let port_of_sockaddr = function
  | ADDR_INET (_, port) -> port
  | ADDR_UNIX _ -> failwith "port_of_sockaddr: ADDR_UNIX"

let port_of_in_channel ic =
  let sa = getsockname (descr_of_in_channel ic) in
  port_of_sockaddr sa

let tcp_socket () =
  Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_STREAM ~protocol:0

let connect_tmout s sockaddr con_timeout =
  set_nonblock s;
  try Unix.connect s ~addr:sockaddr; true
  with Unix_error (EINPROGRESS, _, _) ->
    match select ~read:[s] ~write:[s] ~except:[] ~timeout:con_timeout () with
    | { Select_fds.read = []; write = []; except = [] } -> false
    | _ ->
        let err = getsockopt_int s SO_ERROR in
        if err <> 0 then unix_error err "connect" "";
        clear_nonblock s;
        true

let open_fd_connection_tmout ~con_timeout ~rcv_timeout ~snd_timeout sockaddr =
  let s = tcp_socket () in
  try
    if connect_tmout s sockaddr con_timeout then (
      setsockopt_float s SO_RCVTIMEO rcv_timeout;
      setsockopt_float s SO_SNDTIMEO snd_timeout;
      s)
    else raise Timeout
  with exc -> (try close s with _ -> ()); raise exc

let open_connection_tmout ~con_timeout ~rcv_timeout ~snd_timeout sockaddr =
  let s =
    open_fd_connection_tmout ~con_timeout ~rcv_timeout ~snd_timeout sockaddr
  in
  in_channel_of_descr s, out_channel_of_descr s

let set_in_channel_timeout ic rcv_timeout =
  let s = descr_of_in_channel ic in
  setsockopt_float s SO_RCVTIMEO rcv_timeout

let set_out_channel_timeout oc snd_timeout =
  let s = descr_of_out_channel oc in
  setsockopt_float s SO_SNDTIMEO snd_timeout
