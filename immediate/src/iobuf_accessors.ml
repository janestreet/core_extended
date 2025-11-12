open Core
include Iobuf_accessors_intf

module For_cinaps = struct
  let[@cold] fail name arg value lower_bound upper_bound =
    failwithf
      "%s: %s = %d out of bounds [%d,%d]"
      name
      arg
      value
      lower_bound
      upper_bound
      ()
  ;;

  let len_str = "argument len"
  let pos_str = "argument pos"
  let length_str = "length of immediate string"

  let get_pos ~(local_ pos_opt) ~full_length ~name =
    match pos_opt with
    | None -> 0
    | Some pos ->
      let pos = pos + 0 in
      if pos >= 0 && pos <= full_length then pos else fail name pos_str pos 0 full_length
  ;;

  let get_len ~(local_ len_opt) ~pos ~full_length ~name =
    let upper_bound = full_length - pos in
    match len_opt with
    | None -> upper_bound
    | Some len ->
      let len = len + 0 in
      if len >= 0 && len <= upper_bound then len else fail name len_str len 0 upper_bound
  ;;

  let check_length x ~length upper_bound ~name =
    let len = length x in
    if len <= upper_bound then () else fail name length_str len 0 upper_bound
  ;;

  let checked_read_with_pos_and_len
    ?pos:(local_ pos_opt)
    ?len:(local_ len_opt)
    (local_ buf)
    f
    name
    =
    let full_length = Iobuf.length buf in
    let pos = get_pos ~pos_opt ~full_length ~name in
    let len = get_len ~len_opt ~pos ~full_length ~name in
    f ~pos ~len buf [@nontail]
  ;;

  let checked_read_with_len ?len:len_opt buf f name =
    let full_length = Iobuf.length buf in
    let len = get_len ~len_opt ~pos:0 ~full_length ~name in
    f ~len buf
  ;;

  let checked_write_with_pos_and_len
    x
    ~length
    ?pos:(local_ pos_opt)
    ?len:(local_ len_opt)
    buf
    f
    name
    =
    let full_length = Iobuf.length buf in
    let pos = get_pos ~pos_opt ~full_length ~name in
    let len = get_len ~len_opt ~pos ~full_length ~name in
    check_length x ~length len ~name;
    f x ~pos ~len buf
  ;;

  let checked_write_with_len x ~length ?len:(local_ len_opt) buf f name =
    let full_length = Iobuf.length buf in
    let len = get_len ~len_opt ~pos:0 ~full_length ~name in
    check_length x ~length len ~name;
    f x ~len buf
  ;;

  let checked_write_with_pos x ~length ?pos:(local_ pos_opt) buf f name =
    let full_length = Iobuf.length buf in
    let pos = get_pos ~pos_opt ~full_length ~name in
    check_length x ~length (full_length - pos) ~name;
    f x ~pos buf
  ;;

  let checked_write x ~length buf f name =
    let full_length = Iobuf.length buf in
    check_length x ~length full_length ~name;
    f x buf
  ;;

  let bigstring_read_padding_and_get_unpadded_length ~padding ~pos ~padded_length buf =
    let last = ref (padded_length - 1) in
    while !last >= 0 && phys_equal padding (Bigstring.unsafe_get buf (pos + !last)) do
      decr last
    done;
    1 + !last
  ;;

  let read_padding_and_get_unpadded_length ~padding ~pos ~padded_length buf =
    bigstring_read_padding_and_get_unpadded_length
      ~padding
      ~padded_length
      ~pos:(Iobuf.Expert.lo buf + pos)
      [%template (Iobuf.Expert.buf [@mode local]) buf] [@nontail]
  ;;

  let bigstring_write_padding ~padding ~pos ~unpadded_length ~padded_length buf =
    for i = unpadded_length to padded_length - 1 do
      Bigstring.unsafe_set buf (pos + i) padding
    done
  ;;

  let write_padding ~padding ~pos ~unpadded_length ~padded_length buf =
    bigstring_write_padding
      ~padding
      ~pos:(Iobuf.Expert.lo buf + pos)
      ~unpadded_length
      ~padded_length
      [%template (Iobuf.Expert.buf [@mode local]) buf] [@nontail]
  ;;
end
