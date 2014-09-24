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

module Serialized = struct
  type 'a t = string

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

  let bin_t { Bin_prot.Type_class. writer = bin_writer_a; reader = bin_reader_a } =
    { Bin_prot.Type_class. writer = bin_writer_t bin_writer_a
    ; reader = bin_reader_t bin_reader_a }

  let bin_t_with_value
    { Bin_prot.Type_class. writer = bin_writer_a; reader = bin_reader_a } =
    { Bin_prot.Type_class.writer = bin_writer_t_with_value bin_writer_a
    ; reader = bin_reader_t_with_value bin_reader_a
    }

  module Make (B : Binable) = struct
    type t = string

    let create bt = create B.bin_writer_t bt
    let value t = value t B.bin_reader_t

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

  TEST_MODULE = struct
    module Make_test (B : sig
                   type t with bin_io, sexp_of, compare
                   val values_and_sizes : (t * int) list
                 end) =
    struct
      module Serialized_t = Make (B)

      TEST_UNIT "roundtrip" =
        List.iter B.values_and_sizes ~f:(fun (value, _size) ->
          let t = Serialized_t.create value in
          <:test_result<B.t>> ~expect:value (Serialized_t.value t))
      ;;

      TEST_UNIT "serialized writer matches writer" =
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
          <:test_result<string>> ~expect buffer)
      ;;

      TEST_UNIT "serialized reader matches reader" =
        List.iter B.values_and_sizes ~f:(fun (value, _size) ->
          let buffer = Bin_prot.Utils.bin_dump B.bin_writer_t value in
          let t = Serialized_t.bin_read_t buffer ~pos_ref:(ref 0) in
          <:test_result<B.t>> ~expect:value (Serialized_t.value t))
      ;;

      TEST_UNIT "bin_size_t" =
        List.iter B.values_and_sizes ~f:(fun (value, size) ->
          let t = Serialized_t.create value in
          <:test_result<B.t * int>>
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
      type t = | A of int | B of string | C with sexp, bin_io, compare
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
        } with sexp, bin_io, compare
    end

    module Test_record = Make_test (struct
      include Record

      let values_and_sizes =
        [ { name = "Drew"; age = 26 }, 6 ]
    end)

    module Polymorphic_variant = struct
      type t = [ `A of int | `B of string | `C ] with sexp, bin_io, compare
    end

    module Test_polymorphic_variant = Make_test (struct
      include Polymorphic_variant

      let values_and_sizes =
        [ `A 6, 5
        ; `B "foo bar guz", 16
        ; `C, 4
        ]
    end)
  end
end

module Wrapped = struct
  let bin_write_int64 = Bin_prot.Write.bin_write_int_64bit
  let bin_read_int64 = Bin_prot.Read.bin_read_int_64bit

  module Opaque = struct
    type t = Bigstring.t sexp_opaque with sexp

    (* Can't use Bigstring.{read,write}_bin_prot here, because that will:
       1. Write the 8-byte size header
       2. Then serialize the bigstring, which does:
          2a. Write a one-byte length of the bigstring;
          2b. Write the bigstring contents itself.
       This is almost what we want, but we need to avoid (2a).
    *)

    let bin_size_t t =
      8 (* size of 64 bit int *) + (Bigstring.length t)

    let bin_write_t buf ~pos t =
      let pos = bin_write_int64 buf ~pos (Bigstring.length t) in
      Bigstring.blit
        ~src:t ~src_pos:0
        ~dst:buf ~dst_pos:pos
        ~len:(Bigstring.length t);
      pos + (Bigstring.length t)

    let bin_read_t buf ~pos_ref =
      let size = bin_read_int64 buf ~pos_ref in
      let t = Bigstring.create size in
      Bigstring.blit
        ~src:buf ~src_pos:(!pos_ref)
        ~dst:t ~dst_pos:0
        ~len:size;
      pos_ref := !pos_ref + size;
      t

    let __bin_read_t__ _ ~pos_ref =
      Bin_prot.Common.raise_variant_wrong_type
        "Core_extended.Bin_io_utils.Wrapped.Opaque.t"
        !pos_ref
    ;;

    let bin_writer_t =
      { Bin_prot.Type_class.size = bin_size_t
      ; write = bin_write_t }

    let bin_reader_t =
      { Bin_prot.Type_class.read = bin_read_t
      ; vtag_read = __bin_read_t__ }

    let bin_t =
      { Bin_prot.Type_class. writer = bin_writer_t
      ; reader = bin_reader_t }
  end

  module Ignored = struct
    (* The representation of an ignored value is just the size of the value it was created
       from (i.e., the number of bytes that were ignored from the buffer we were reading
       -- we exclude the 8 byte size header from which the size was read). *)
    type t = int with sexp

    (* dhouse: could use Bigstring.{read,write}_bin_prot here, but we don't just to stay
       consistent with Opaque.t and Wrapped.t. (And also we'd need special code in
       bin_read_t anyway, since we just want to read the size and then increment
       pos_ref. *)

    let bin_size_t size =
      8 + size

    let bin_write_t _buf ~pos:_ (_ : t) =
      failwith
        "Core_extended.Bin_io_utils.Wrapped.Ignored.bin_write_t: not supported, \
         perhaps you want [Wrapped.Opaque.t]?"

    let bin_read_t buf ~pos_ref =
      let size = bin_read_int64 buf ~pos_ref in
      pos_ref := !pos_ref + size;
      size

    let __bin_read_t__ _ ~pos_ref =
      Bin_prot.Common.raise_variant_wrong_type
        "Core_extended.Bin_io_utils.Wrapped.Ignored.t"
        !pos_ref
    ;;

    let bin_writer_t =
      { Bin_prot.Type_class.size = bin_size_t
      ; write = bin_write_t }

    let bin_reader_t =
      { Bin_prot.Type_class.read = bin_read_t
      ; vtag_read = __bin_read_t__ }

    let bin_t =
      { Bin_prot.Type_class. writer = bin_writer_t
      ; reader = bin_reader_t }
  end

  type 'a t = 'a with sexp

  let bin_size_t bin_size_a a =
    Bigstring.bin_prot_size_header_length + bin_size_a a


  let bin_write_t bin_write_a =
    fun buf ~pos a ->
      let start_a = pos + 8 in
      let end_a = bin_write_a buf ~pos:start_a a in
      let size = end_a - start_a in
      let written = bin_write_int64 buf ~pos size in
      assert (written = start_a);
      end_a

  let bin_read_t bin_read_a =
    fun buf ~pos_ref ->
      let expected_size = bin_read_int64 buf ~pos_ref in
      let start_a = !pos_ref in
      let a = bin_read_a buf ~pos_ref in
      let end_a = !pos_ref in
      assert (end_a - start_a = expected_size);
      a

  let __bin_read_t__ _ _ ~pos_ref =
    Bin_prot.Common.raise_variant_wrong_type
      "Core_extended.Bin_io_utils.Wrapped.t"
      !pos_ref
  ;;

  let bin_writer_t { Bin_prot.Type_class. size = bin_size_a; write = bin_write_a } =
    { Bin_prot.Type_class.size = bin_size_t bin_size_a
    ; write = bin_write_t bin_write_a }

  let bin_reader_t
        { Bin_prot.Type_class. read = bin_read_a; vtag_read = __bin_read_a__ } =
    { Bin_prot.Type_class.read = bin_read_t bin_read_a
    ; vtag_read = __bin_read_t__ __bin_read_a__ }

  let bin_t { Bin_prot.Type_class. writer = bin_writer_a; reader = bin_reader_a } =
    { Bin_prot.Type_class. writer = bin_writer_t bin_writer_a
    ; reader = bin_reader_t bin_reader_a }

  let to_opaque t bin_writer =
    Bin_prot.Utils.bin_dump bin_writer t

  let of_opaque_exn (buffer : Opaque.t) bin_reader =
    bin_reader.Bin_prot.Type_class.read buffer ~pos_ref:(ref 0)

  TEST_MODULE = struct
    module Mystery = struct
      type t =
        { name : string
        ; age : int
        ; favorite_colors : string list
        } with sexp, bin_io

      let value =
        { name = "Drew"
        ; age = 25
        ; favorite_colors = [ "Blue"; "Yellow" ]
        }
    end

    module T = struct
      type 'a t =
        { header : string
        ; mystery : 'a
        ; footer : string
        } with sexp, bin_io

      let value mystery =
        { header = "header"
        ; mystery
        ; footer = "footer"
        }
    end

    (* Some Rumsfeldian tests follow... *)
    module Known = struct
      type nonrec t = Mystery.t t T.t with sexp, bin_io

      let value = T.value Mystery.value
    end

    module Unknown = struct
      type t = Opaque.t T.t with sexp, bin_io

      let value = T.value (to_opaque Mystery.value Mystery.bin_writer_t)
    end

    let convert bin_writer bin_reader value =
      let buffer = Bin_prot.Utils.bin_dump bin_writer value in
      bin_reader.Bin_prot.Type_class.read buffer ~pos_ref:(ref 0)

    let roundtrip { Bin_prot.Type_class. reader; writer } value =
      assert (convert writer reader value = value)

    TEST_UNIT = roundtrip Known.bin_t Known.value
    TEST_UNIT = roundtrip Unknown.bin_t Unknown.value

    TEST_UNIT "opaque and wrapped serialize the same way" =
      let known_buffer = Bin_prot.Utils.bin_dump Known.bin_writer_t Known.value in
      let unknown_buffer = Bin_prot.Utils.bin_dump Unknown.bin_writer_t Unknown.value in
      <:test_pred<string * string>>
        (fun (a, b) -> String.(=) a b)
        (Bigstring.to_string known_buffer, Bigstring.to_string unknown_buffer)

    TEST_UNIT "serialized wrapped deserializes to the expected opaque" =
      let unknown_from_known =
        convert Known.bin_writer_t Unknown.bin_reader_t Known.value
      in
      assert (Unknown.value = unknown_from_known)

    TEST_UNIT "serialized opaque deserializes to the expected wrapped" =
      let known_from_unknown =
        convert Unknown.bin_writer_t Known.bin_reader_t Unknown.value
      in
      assert (Known.value = known_from_unknown)

    module Dropped = struct
      type t = Ignored.t T.t with sexp, bin_io
    end

    TEST_UNIT =
      let buffer = Bin_prot.Utils.bin_dump Known.bin_writer_t Known.value in
      let value = Dropped.bin_reader_t.Bin_prot.Type_class.read buffer ~pos_ref:(ref 0) in
      let ignored = value.mystery in
      (* The value deserialized with [Dropped] agrees with the value serialized by
         [Known], except for the ignored bit. *)
      assert ({ Known.value with mystery = ignored} = value );
      (* [Dropped] remembered the size of the ignored data. *)
      assert (Dropped.bin_size_t value = Known.bin_size_t Known.value);
      (* [Dropped] can't be serialized. *)
      assert (Option.is_none (Option.try_with (fun () ->
        ignore (Bin_prot.Utils.bin_dump Dropped.bin_writer_t value))))
    ;;
  end
end
