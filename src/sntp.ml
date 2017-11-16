open Core ;;

type t =
  { remote_host      : string
  ; offset           : Time.Span.t
  ; round_trip_delay : Time.Span.t
  ; stratum          : int
  ; root_delay       : Time.Span.t
  ; root_dispersion  : Time.Span.t
  ; t1               : float
  ; t2               : float
  ; t3               : float
  ; t4               : float }
[@@deriving sexp, fields]
;;

module Rfc5905 = struct
  let ntp_conv_const = 2. ** 32.

  let delta = 2208988800. (** difference in seconds between 1900 and 1970 *)

  let ntpfp_to_float (ipart,fpart) =
    Int64.to_float ipart +. Int64.to_float fpart /. ntp_conv_const
    -. delta
  ;;

  let float_to_ntpfp f =
    let f = f +. delta in
    let ipart = Int64.of_float f in
    let fpart = Int64.of_float ((f -. Float.round ~dir:`Down f) *. ntp_conv_const) in
    (ipart,fpart)
  ;;

  let bytemask = Int64.of_int 0xFF

  let short_int64_to_buf buf pos bits =
    for i = 0 to 3 do
      let byte64 = Int64.bit_and (Int64.shift_right_logical bits (8 * i)) bytemask in
      let char = Char.of_int_exn (Int64.to_int_exn byte64) in
      Bytes.set buf (pos + 3 - i) char
    done
  ;;

  let short_int64_of_buf buf pos =
    let value = ref Int64.zero in
    for i = 0 to 3 do
      let byte = Int64.of_int (Char.to_int (Bytes.get buf (pos + 3 - i))) in
      value := Int64.(+) !value (Int64.shift_left byte (i * 8))
    done;
    !value
  ;;

  let float_to_buf buf pos f =
    let (ipart,fpart) = float_to_ntpfp f in
    short_int64_to_buf buf pos ipart;
    short_int64_to_buf buf (pos + 4) fpart
  ;;

  let float_from_buf buf pos =
    let ipart = short_int64_of_buf buf pos in
    let fpart = short_int64_of_buf buf (pos + 4) in
    ntpfp_to_float (ipart,fpart)
  ;;

  let float_from_ntp_short_buf buf pos =
    let short_int64_of_buf buf pos =
      Int64.(+)
      (Int64.of_int (Char.to_int (Bytes.get buf (pos + 1))))
      (Int64.shift_left (Int64.of_int (Char.to_int (Bytes.get buf pos))) 8)
    in
    let ipart = short_int64_of_buf buf pos in
    let fpart = short_int64_of_buf buf (pos + 2) in
    Int64.to_float ipart +. Int64.to_float fpart /. (2. ** 16.)
  ;;

  let ntp_packet_length = 48

  (* State variable for sanity-checking (see rfc5905), this is basically just the t3 field
     (server transmit timestamp) of the last received packet, to detect dupes/replays.

     We store and check the wire representation because there is a loss of precision when
     converting from the float returned from [Unix.gettimeofday] and ntp's 64-bit
     timestamp format. *)
  let org_wire_format = ref ""

  let query ~timeout ~port remote_host =
    try
      let buf = Bytes.create ntp_packet_length in
      Bytes.fill ~pos:0 ~len:ntp_packet_length buf '\000';
      let addr = Unix.get_sockaddr remote_host port in
      Exn.protectx
        (Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_DGRAM ~protocol:0)
        ~finally:Unix.close
        ~f:(fun s ->
          (* Emulating the packets that `ntpdate -p 1 -q server' sends, ie:
              Leap Indicator: 11 (Not synchronised)
              Version:       100 (NTPv4)
              Mode:         0011 (Client)
          *)
          Bytes.set buf 0 (Char.of_int_exn 0b11100011);
          float_to_buf buf 40 (Unix.gettimeofday ());
          let xmt_wire_format = Bytes.To_string.sub buf ~pos:40 ~len:8 in
          Unix.sendto s ~buf ~pos:0 ~len:(Bytes.length buf) ~mode:[] ~addr
          |> ignore;
          let sfds =
            Unix.select
              ~read:[s]
              ~write:[]
              ~except:[]
              ~timeout:(`After (Time_ns.Span.of_span timeout)) ()
          in
          let fail fmt =
            ksprintf (fun msg -> Error (`Err (Error.of_string msg))) fmt
          in
          match sfds.Unix.Select_fds.read with
          | []    -> Error `Timeout
          | s::[] ->
            let _ = Unix.recvfrom s ~buf ~pos:0 ~len:(Bytes.length buf) ~mode:[] in
            let t4 = Unix.gettimeofday () in  (* destination timestamp *)
            let t1 = float_from_buf buf 24 in (* origin timestamp *)
            let t2 = float_from_buf buf 32 in (* receive timestamp *)
            let t3 = float_from_buf buf 40 in (* transmit timestamp *)
            let stratum = Char.to_int (Bytes.get buf 1) in
            let root_delay = Time.Span.of_sec (float_from_ntp_short_buf buf 4) in
            let root_dispersion = Time.Span.of_sec (float_from_ntp_short_buf buf 8) in
            (* sanity checks as per rfc5905 *)
            let t1_wire_format = Bytes.To_string.sub buf ~pos:24 ~len:8 in
            let t3_wire_format = Bytes.To_string.sub buf ~pos:40 ~len:8 in
            let prev_org_wire_format = !org_wire_format in
            org_wire_format := t3_wire_format;
            if String.equal t1_wire_format xmt_wire_format
            then
              if String.equal t3_wire_format prev_org_wire_format
              then
                (* same t3 as last time *)
                fail "Received duplicate or replayed packet (%s == %s)"
                  t3_wire_format prev_org_wire_format
              else
                Ok { remote_host
                   ; stratum
                   ; root_delay
                   ; root_dispersion
                   ; t1
                   ; t2
                   ; t3
                   ; t4
                   (* See rfc4330 and rfc5905 *)
                   ; offset = Time.Span.of_sec (((t2 -. t1) +. (t3 -. t4)) /. 2.)
                   ; round_trip_delay = Time.Span.of_sec ((t4 -. t1) -. (t3 -. t2)) }
            else
              fail "Received packet with bogus t1 timestamp (%s != %s)"
                t1_wire_format xmt_wire_format
          | _ -> fail "BUG! [Unix.select] returned too many file descriptors?!")
    with
    | e -> Error (`Exn e)
  ;;
end

let default_timeout = Time.Span.of_sec 5.0
let default_port    = 123

let query ?(timeout=default_timeout) ?(port=default_port) hostname =
  match Rfc5905.query ~timeout ~port hostname with
  | Ok {offset; _} -> `Offset offset
  | Error `Timeout -> `Timeout
  | Error (`Exn e) -> `Error e
  | Error (`Err e) -> `Error (Error.to_exn e)
;;

let extended_query ?(timeout=default_timeout) ?(port=default_port) hostname =
  Rfc5905.query ~timeout ~port hostname
  |> Result.map_error ~f:(function
    | `Timeout -> Error.createf !"Timeout after %{Time.Span}" timeout
    | `Exn exn -> Error.of_exn exn
    | `Err err -> err)
;;
