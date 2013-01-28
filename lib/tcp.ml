open Core.Std ;;

exception Connect_timeout ;;

let with_connection ?(timeout=(Time.Span.of_float 30.)) ~host ~port ~f () =
  Exn.protectx
    (Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_STREAM ~protocol:0)
    ~f:(fun sd ->
      Unix.set_nonblock sd;
      begin
        try
          Unix.connect sd ~addr:(Unix.get_sockaddr host port)
        with
        | Unix.Unix_error (Unix.EINPROGRESS, _, _) -> ()
        | e -> raise e
      end;
      let sfds =
        Unix.select
          ~read:[]
          ~write:[sd]
          ~except:[sd]
          ~timeout:(Time.Span.to_float timeout)
          ()
      in
      begin match sfds.Unix.Select_fds.except with
      | [] -> ()
      | sd :: [] ->

          Unix.unix_error
            (Unix.getsockopt_int sd Unix.SO_ERROR)
            "connect"
            (sprintf "%s:%d" host port)

      | _ -> assert false
      end;

      begin match sfds.Unix.Select_fds.write with
      | [] -> raise Connect_timeout
      | sd :: [] ->
          Unix.clear_nonblock sd;
          Exn.protectx
            (Unix.in_channel_of_descr sd,
             Unix.out_channel_of_descr sd)
          ~f:(fun (in_chan, out_chan) -> f in_chan out_chan)
          ~finally:(fun (in_chan, out_chan) ->
            try Out_channel.close out_chan with _e -> ();
            try In_channel.close in_chan with _e -> ()
          )

      | _ -> assert false
      end
    )
    ~finally:(fun sd -> try Unix.close sd with _e -> ())
;;

