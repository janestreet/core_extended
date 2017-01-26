open Core

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
    let in_dir, basename = Filename.split file in
    Filename.open_temp_file ~in_dir basename "tmp"
  in
  try
    Bigstring.really_output oc bstr;
    Out_channel.close oc;
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
    try Out_channel.close oc with _ -> ();
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

module Serialized = struct
  type 'a t = string

  let bin_shape_t t = t

  let create { Bin_prot.Type_class. size; write } a =
    let size = size a in
    let buf = Bigstring.create size in
    let written = write buf ~pos:0 a in
    assert (size = written);
    Bigstring.to_string buf
  ;;

  let value t { Bin_prot.Type_class. read; vtag_read = _ } =
    let pos_ref = ref 0 in
    read (Bigstring.of_string t) ~pos_ref

  let bin_size_t t = String.length t

  let bin_write_t =
    fun buf ~pos t ->
      let len = String.length t in
      Bigstring.From_string.blit
        ~src:t ~src_pos:0
        ~dst:buf ~dst_pos:pos
        ~len;
      pos + len

  let bin_read_t_with_value bin_read_a =
    fun buf ~pos_ref ->
      let pos = !pos_ref in
      let a = bin_read_a buf ~pos_ref in
      let len = !pos_ref - pos in
      let t = Bigstring.To_string.sub buf ~pos ~len in
      (t, a)

  let bin_read_t bin_read_a =
    fun buf ~pos_ref ->
      fst (bin_read_t_with_value bin_read_a buf ~pos_ref)

  let __bin_read_t__ _ =
    (* this should only be called when [t] is a polymorphic variant (it is not) *)
    fun _ ~pos_ref:_ -> assert false

  let bin_writer_t (_ : 'a Bin_prot.Type_class.writer) =
    { Bin_prot.Type_class.size = bin_size_t
    ; write = bin_write_t }

  let bin_reader_t
        { Bin_prot.Type_class. read = bin_read_a; vtag_read } =
    { Bin_prot.Type_class.read = bin_read_t bin_read_a
    ; vtag_read = __bin_read_t__ vtag_read }

  let bin_reader_t_with_value
        { Bin_prot.Type_class. read = bin_read_a; vtag_read } =
    { Bin_prot.Type_class.read = bin_read_t_with_value bin_read_a
    ; vtag_read = __bin_read_t__ vtag_read }

  let bin_writer_t_with_value (_ : 'a Bin_prot.Type_class.writer) =
    { Bin_prot.Type_class.size = (fun x -> bin_size_t (fst x))
    ; write = (fun buf ~pos t -> bin_write_t buf ~pos (fst t))
    }

  let bin_t { Bin_prot.Type_class. writer = bin_writer_a; reader = bin_reader_a
            ; shape = bin_shape_a } =
    { Bin_prot.Type_class. writer = bin_writer_t bin_writer_a
    ; reader = bin_reader_t bin_reader_a
    ; shape = bin_shape_t bin_shape_a
    }

  let bin_t_with_value
    { Bin_prot.Type_class. writer = bin_writer_a; reader = bin_reader_a
    ; shape = bin_shape_a } =
    { Bin_prot.Type_class.writer = bin_writer_t_with_value bin_writer_a
    ; reader = bin_reader_t_with_value bin_reader_a
    ; shape = bin_shape_t bin_shape_a
    (* This is a case where we actually want the same shape because ['a Serialized.t] has
       the same binary serialization as ['a].  *)
    }

  module Make (B : Binable) = struct
    type t = string

    let create bt = create B.bin_writer_t bt
    let value t = value t B.bin_reader_t

    let bin_shape_t = B.bin_shape_t

    let bin_size_t = bin_size_t
    let bin_write_t = bin_write_t
    let bin_read_t = bin_read_t B.bin_read_t
    let __bin_read_t__ = __bin_read_t__ B.__bin_read_t__
    let bin_writer_t = bin_writer_t B.bin_writer_t
    let bin_reader_t = bin_reader_t B.bin_reader_t
    let bin_t = bin_t B.bin_t

    let bin_reader_t_with_value = bin_reader_t_with_value B.bin_reader_t

    let bin_t_with_value = bin_t_with_value B.bin_t
  end

  let bin_size_t _ = bin_size_t
  let bin_write_t _ = bin_write_t

  let%test_module _ = (module struct
    module Make_test (B : sig
                   type t [@@deriving bin_io, sexp_of, compare]
                   val values_and_sizes : (t * int) list
                 end) =
    struct
      module Serialized_t = Make (B)

      let%test_unit "roundtrip" =
        List.iter B.values_and_sizes ~f:(fun (value, _size) ->
          let t = Serialized_t.create value in
          [%test_result: B.t] ~expect:value (Serialized_t.value t))
      ;;

      let%test_unit "serialized writer matches writer" =
        List.iter B.values_and_sizes ~f:(fun (value, _size) ->
          let t = Serialized_t.create value in
          let expect =
            Bin_prot.Utils.bin_dump B.bin_writer_t value
            |> Bigstring.to_string
          in
          let buffer =
            Bin_prot.Utils.bin_dump Serialized_t.bin_writer_t t
            |> Bigstring.to_string
          in
          [%test_result: string] ~expect buffer)
      ;;

      let%test_unit "serialized reader matches reader" =
        List.iter B.values_and_sizes ~f:(fun (value, _size) ->
          let buffer = Bin_prot.Utils.bin_dump B.bin_writer_t value in
          let t = Serialized_t.bin_read_t buffer ~pos_ref:(ref 0) in
          [%test_result: B.t] ~expect:value (Serialized_t.value t))
      ;;

      let%test_unit "bin_size_t" =
        List.iter B.values_and_sizes ~f:(fun (value, size) ->
          let t = Serialized_t.create value in
          [%test_result: B.t * int]
            ~expect:(value, size)
            (value, (Serialized_t.bin_size_t t)))
    end

    module Test_int = Make_test (struct
      include Int

      let values_and_sizes =
        [ 5, 1
        ; 255, 3
        ; 1024, 3
        ]
    end)

    module Test_string = Make_test (struct
      include String

      let values_and_sizes =
        [ "foo", 4
        ; "foo bar", 8
        ]
    end)

    module Variant = struct
      type t = | A of int | B of string | C [@@deriving sexp, bin_io, compare]
    end

    module Test_variant = Make_test (struct
      include Variant

      let values_and_sizes =
        [ A 5, 2
        ; B "foo bar buz", 13
        ; C, 1
        ]
    end)

    module Record = struct
      type t =
        { name : string
        ; age : int
        } [@@deriving sexp, bin_io, compare]
    end

    module Test_record = Make_test (struct
      include Record

      let values_and_sizes =
        [ { name = "Drew"; age = 26 }, 6 ]
    end)

    module Polymorphic_variant = struct
      type t = [ `A of int | `B of string | `C ] [@@deriving sexp, bin_io, compare]
    end

    module Test_polymorphic_variant = Make_test (struct
      include Polymorphic_variant

      let values_and_sizes =
        [ `A 6, 5
        ; `B "foo bar guz", 16
        ; `C, 4
        ]
    end)
  end)
end
