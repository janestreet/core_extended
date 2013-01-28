open Core.Std

let load ?pos ?len file bin_read_t =
  let failwith s = failwith ("Core_extended.Bin_io_utils.load: " ^ s) in
  let pos =
    match pos with
    | None -> Int64.zero
    | Some pos when pos < Int64.zero -> failwith "pos < 0"
    | Some pos -> pos
  in
  let bstr =
    In_channel.with_file file ~f:(fun ic ->
      let file_size = In_channel.length ic in
        let len64 =
          match len with
          | None when pos > file_size -> failwith "pos > file size"
          | None -> Int64.(-) file_size pos
          | Some len64 when len64 < Int64.zero -> failwith "len < 0"
          | Some len64 when Int64.(+) pos len64 > file_size ->
              failwith "pos + len < file size"
          | Some len64 -> len64
        in
        let len =
          match Int64.to_int len64 with
          | None -> failwith "len exceeds maximum integer"
          | Some len -> len
        in
        let bstr = Bigstring.create len in
        Bigstring.really_input ic bstr;
        bstr)
  in
  let pos_ref = ref 0 in
  let v = bin_read_t bstr ~pos_ref in
  if !pos_ref <> Bigstring.length bstr then failwith "garbage after data"
  else v

let save ?header ?perm file bin_writer_t v =
  let bstr = Bin_prot.Utils.bin_dump ?header bin_writer_t v in
  let tmp_name, oc =
    let in_dir =
      if Filename.is_relative file then "."
      else Filename.dir_sep
    in
    Filename.open_temp_file ~in_dir file "tmp"
  in
  try
    Bigstring.really_output oc bstr;
    close_out oc;
    let perm =
      match perm with
      | Some perm -> perm
      | None ->
          let umask = Unix.umask 0 in
          ignore (Unix.umask umask);
          umask lxor 0o666
    in
    if perm <> 0o600 then Unix.chmod tmp_name ~perm;
    Sys.rename tmp_name file
  with e ->
    close_out_noerr oc;
    raise e

let end_char = '\n'
let escape_char = '\\'
let escaped_end_char = 'n'

let to_line bin_t v =
  let bstr = Bin_prot.Utils.bin_dump bin_t.Bin_prot.Type_class.writer v ~header:false in
  let len = Bigstring.length bstr in
  let escaped_bstr = Bigstring.create (2 * len + 1) in
  let end_pos = ref 0 in
  let write char =
    escaped_bstr.{!end_pos} <- char;
    incr end_pos
  in
  for i = 0 to len - 1 do
    if bstr.{i} = end_char
    then begin
      write escape_char;
      write escaped_end_char;
    end
    else if bstr.{i} = escape_char
    then begin
      write escape_char;
      write escape_char
    end
    else write bstr.{i}
  done;
  write end_char;
  Bigstring.sub escaped_bstr ~pos:0 ~len:!end_pos

let of_line s bin_t =
  let len = String.length s in
  let bstr = Bigstring.create len in
  let bstr_pos = ref 0 in
  let write char =
    bstr.{!bstr_pos} <- char;
    incr bstr_pos
  in
  let rec loop s_pos =
    if s_pos < len
    then
      if s.[s_pos] <> escape_char
      then begin
        write s.[s_pos];
        loop (s_pos + 1)
      end
      else
        let next = s.[s_pos+1] in
        if next = escape_char
        then write escape_char
        else if next = escaped_end_char
        then write end_char
        else failwith "bug in write_robust or read_robust";
        loop (s_pos + 2)
  in
  loop 0;
  bin_t.Bin_prot.Type_class.reader.Bin_prot.Type_class.read
    (Bigstring.sub_shared bstr ~pos:0 ~len:!bstr_pos) ~pos_ref:(ref 0)
