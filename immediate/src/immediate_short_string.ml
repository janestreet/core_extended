(* Represents strings up to 7 characters long using an int63.

   {v
      +---------- bits 62-59 are unused (by this module)
      |   +------ bits 58-56 represent length
      |   |   +-- bits 55-0 represent characters
      v   v   v
     [0000lll_cccccccc_..._cccccccc]
   v}

   Characters are stored in reverse order (first character in the lowest order bits),
   right-justified, and 0-padded.  This is intended both for efficient manipulation and to
   push entropy toward low-order bits so that the identity hash function is reasonable.

   Conveniently, this also makes the int values relatively easy to read in hex form.  For
   example, "abc" is 0x0300_0000_0063_6261 (note ASCII codes a=0x61, b=0x62, and
   c=0x63). *)

open Core.Core_stable

module Stable = struct
  module V1 = struct
    module T_stringable = struct
      (* In this case, as opposed to interned strings, it is safe to use [int]'s stable
         [bin_io]. *)
      type t = int
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , stable_witness]

      let max_length = 0b111
      let length_offset = 56
      let[@inline] char_offset i = i lsl 3
      let[@inline] unsafe_create len = len lsl length_offset
      let[@inline] length t = (t lsr length_offset) land max_length

      let[@inline] unsafe_get t i =
        Core.Char.unsafe_of_int ((t lsr char_offset i) land 0xff)
      ;;

      (* Assumes that character [i] of [t] has not been set yet (all bits still 0) *)
      let[@inline] unsafe_set t i c = t lor (Core.Char.to_int c lsl char_offset i)

      let[@inline] unsafe_set_len t new_len =
        let old_len = length t in
        t lxor ((new_len lxor old_len) lsl length_offset)
      ;;

      let[@zero_alloc] rec pad_right' t char index old_len =
        if index < old_len
        then t
        else pad_right' (unsafe_set t index char) char (index - 1) old_len
      ;;

      let[@zero_alloc] pad_right t ~char ~len =
        assert (len <= max_length);
        let old_len = length t in
        if len > old_len
        then pad_right' (unsafe_set_len t len) char (len - 1) old_len
        else t
      ;;

      let[@inline] is_zeroed n ~lo_inclusive ~hi_exclusive =
        let hi_zeroes = (-1 lsl hi_exclusive) lxor -1 in
        let lo_zeroes = -1 lsl lo_inclusive in
        hi_zeroes land lo_zeroes land n = 0
      ;;

      let[@inline] zeroed_prefix_bits n = is_zeroed n ~lo_inclusive:59 ~hi_exclusive:63

      let[@inline] zeroed_padding_bits n =
        is_zeroed n ~lo_inclusive:(char_offset (length n)) ~hi_exclusive:length_offset
      ;;

      let[@inline] is_valid n = zeroed_prefix_bits n && zeroed_padding_bits n
      let unsafe_of_int t = t
      let unsafe_to_int t = t

      let[@inline] of_int_exn n =
        assert (is_valid n);
        n
      ;;

      let to_int_exn t = t

      let to_local_bytes t = exclave_
        let module Bytes = Base.Bytes in
        let len = length t in
        let bytes = Bytes.create_local len in
        for i = 0 to len - 1 do
          Bytes.unsafe_set bytes i (unsafe_get t i)
        done;
        bytes
      ;;

      let to_local_string t = exclave_
        let str = to_local_bytes t in
        Base.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:str
      ;;

      let to_string t =
        let str = Base.Bytes.globalize (to_local_bytes t) in
        Base.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:str
      ;;

      let of_substring_failure ~pos ~len ~name (local_ str) =
        let str = Core.String.globalize str in
        failwith
          (Printf.sprintf
             "Immediate.Short_string.%s: cannot take a substring from pos %d of length \
              %d: %s"
             name
             pos
             len
             str)
      ;;

      let of_string_failure ~len ~name (local_ str) =
        let str = Core.String.globalize str in
        failwith
          (Printf.sprintf
             "Immediate.Short_string.%s: cannot create a string of length %d > %d: %s"
             name
             len
             max_length
             str)
      ;;

      let of_substring_internal (local_ str) ~pos ~len ~name =
        let module String = Core.String in
        let strlen = String.length str in
        if pos < 0 || len < 0 || pos + len < 0 || pos + len > strlen
        then of_substring_failure ~pos ~len ~name str
        else if len > max_length
        then of_string_failure ~len ~name str
        else (
          let t = ref (unsafe_create len) in
          for i = 0 to len - 1 do
            t := unsafe_set !t i (String.unsafe_get str (pos + i))
          done;
          !t)
      ;;

      let of_string_internal str ~name =
        of_substring_internal str ~pos:0 ~len:(Core.String.length str) ~name
      ;;

      let of_local_string (local_ str) = of_string_internal str ~name:"of_local_string"
      let of_string str = of_string_internal str ~name:"of_string"

      let of_substring (local_ str) ~pos ~len =
        of_substring_internal str ~pos ~len ~name:"of_substring"
      ;;

      let of_bytes bytes =
        let module Bytes = Base.Bytes in
        of_string_internal
          ~name:"of_bytes"
          (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes)
      ;;

      let of_uint32 uint32 = unsafe_create 4 lor Int_repr.Uint32.to_base_int_exn uint32
      let[@inline] is_valid_length n = n land max_length = n
      let[@inline] is_valid_string (local_ str) = is_valid_length (Core.String.length str)
    end

    include T_stringable
    include Sexpable.Of_stringable.V1 [@modality portable] (T_stringable)

    let rec lexicographic_compare_from t1 t2 ~len1 ~len2 ~min_len ~pos =
      if pos >= min_len
      then compare len1 len2
      else (
        match Stdlib.Char.compare (unsafe_get t1 pos) (unsafe_get t2 pos) with
        | 0 -> lexicographic_compare_from t1 t2 ~len1 ~len2 ~min_len ~pos:(pos + 1)
        | n -> n)
    ;;

    let lexicographic_compare t1 t2 =
      let len1 = length t1 in
      let len2 = length t2 in
      let min_len = min len1 len2 in
      lexicographic_compare_from t1 t2 ~len1 ~len2 ~min_len ~pos:0
    ;;

    let%test_unit "Stable.V1.lexicographic_compare consistent with String.compare" =
      let variant n =
        if n < 0 then `Less_than else if n > 0 then `Greater_than else `Equal
      in
      let strings =
        [ ""
        ; "a"
        ; "aa"
        ; "b"
        ; "bbb"
        ; "c"
        ; "cccc"
        ; "d"
        ; "ddddd"
        ; "e"
        ; "eeeeee"
        ; "f"
        ; "fffffff"
        ; "g"
        ]
      in
      ListLabels.iter strings ~f:(fun str1 ->
        ListLabels.iter strings ~f:(fun str2 ->
          let t1 = of_string str1 in
          let t2 = of_string str2 in
          [%test_result: [ `Less_than | `Equal | `Greater_than ]]
            (variant (lexicographic_compare t1 t2))
            ~expect:(variant (Core.String.compare str1 str2))
            ~message:(Printf.sprintf "compare %S %S" str1 str2)))
    ;;
  end

  module%test V1 = struct
    let tests =
      (* Stringable, Intable, Sexpable, Binable *)
      [ "", 0x0, "\"\"", "\000"
      ; "a", 0x100_0000_0000_0061, "a", "\252a\000\000\000\000\000\000\001"
      ; "AB", 0x200_0000_0000_4241, "AB", "\252AB\000\000\000\000\000\002"
      ; "Q\000?", 0x300_0000_003f_0051, "Q\000?", "\252Q\000?\000\000\000\000\003"
      ; "1546024", 0x734_3230_3634_3531, "1546024", "\2521546024\007"
      ; "abcdef", 0x600_6665_6463_6261, "abcdef", "\252abcdef\000\006"
      ; "abcdefg", 0x767_6665_6463_6261, "abcdefg", "\252abcdefg\007"
      ; "\000", 0x100_0000_0000_0000, "\000", "\252\000\000\000\000\000\000\000\001"
      ; "\001", 0x100_0000_0000_0001, "\001", "\252\001\000\000\000\000\000\000\001"
      ; "\255", 0x100_0000_0000_00ff, "\255", "\252\255\000\000\000\000\000\000\001"
      ; "abcde\000", 0x600_0065_6463_6261, "abcde\000", "\252abcde\000\000\006"
      ; "\000abcde", 0x600_6564_6362_6100, "\000abcde", "\252\000abcde\000\006"
      ]
    ;;

    let%test_unit "Intable and Stringable" =
      Core.Or_error.combine_errors_unit
        (Core.List.map tests ~f:(fun (s, t, _, _) ->
           Core.Or_error.tag
             (Core.Or_error.try_with (fun () ->
                [%test_result: Core.Int.Hex.t] (V1.of_string s) ~expect:t;
                [%test_result: string] (V1.to_string t) ~expect:s;
                [%test_result: Core.Int.Hex.t] (V1.to_int_exn t) ~expect:t;
                [%test_result: V1.t] (V1.of_int_exn t) ~expect:t))
             ~tag:s))
      |> Core.ok_exn
    ;;

    module%test [@name "Sexpable and Binable"] _ = Unit_test (struct
        type t = V1.t [@@deriving bin_io, sexp]

        let equal = Core.Int.equal
        let tests = Core.List.map tests ~f:(fun (_, t, s, b) -> t, s, b)
      end)
  end
end

module Option_stable = struct
  module V1 = struct
    type t = int
    [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize, hash]

    let none = Core.Int.max_value

    (* Ensure invariant: Option.none is a non-negative int and not a valid string. *)
    let () =
      assert (none >= 0);
      assert (not (Stable.V1.is_valid none))
    ;;

    let some (value : Stable.V1.t) = value
    let[@zero_alloc] is_none t = t = none
    let is_some t = not (is_none t)
    let some_is_representable _ = true
    let[@zero_alloc] unchecked_value (t : t) = t

    let value_exn_not_found =
      Not_found_s [%message "[Immediate.Short_string.Option.value_exn]: given [none]"]
    ;;

    let value_exn t = if is_some t then t else raise value_exn_not_found
    let value t ~default = Core.Bool.select (is_none t) default t
    let to_option t = if is_some t then Some (unchecked_value t) else None

    let of_option = function
      | None -> none
      | Some v -> some v
    ;;

    let sexp_of_t t = to_option t |> [%sexp_of: Stable.V1.t option]
    let t_of_sexp s = [%of_sexp: Stable.V1.t option] s |> of_option

    let t_sexp_grammar : t Sexplib.Sexp_grammar.t =
      Sexplib.Sexp_grammar.coerce [%sexp_grammar: string option]
    ;;

    let to_string_option t =
      if is_none t then None else Some (Stable.V1.to_string (unchecked_value t))
    ;;

    let of_string_option = function
      | None -> none
      | Some s -> some (Stable.V1.of_string s)
    ;;

    let of_int_exn n = if is_none n then n else Stable.V1.of_int_exn n
    let to_int_exn t = t
  end

  module%test [@name "V1 : Binable and Sexpable"] _ = Unit_test (struct
      include V1

      let equal = Core.Int.equal

      let tests =
        [ V1.of_string_option None, "()", "\252\255\255\255\255\255\255\255?"
        ; V1.of_string_option (Some "somestr"), "(somestr)", "\252somestr\007"
        ; V1.of_string_option (Some ""), "(\"\")", "\000"
        ; ( V1.of_string_option (Some "\000")
          , "(\"\\000\")"
          , "\252\000\000\000\000\000\000\000\001" )
        ]
      ;;
    end)
end

open! Core
open! Int.Replace_polymorphic_compare

module%test [@name "Stable.V1"] _ = struct
  open Stable.V1

  type hex = int [@@deriving compare ~localize]

  let sexp_of_hex n = sexp_of_string (sprintf "0x%x" n)

  let test i s =
    [%test_eq: hex] i (to_int_exn (of_string s));
    true
  ;;

  let%test _ = test 0x0000_0000_0000_0000 ""
  let%test _ = test 0x0100_0000_0000_0061 "a"
  let%test _ = test 0x0200_0000_0000_6261 "ab"
  let%test _ = test 0x0300_0000_0063_6261 "abc"
  let%test _ = test 0x0400_0000_6463_6261 "abcd"
  let%test _ = test 0x0500_0065_6463_6261 "abcde"
  let%test _ = test 0x0600_6665_6463_6261 "abcdef"
  let%test _ = test 0x0767_6665_6463_6261 "abcdefg"

  let test i s =
    [%test_eq: string] (to_string (of_int_exn i)) s;
    true
  ;;

  let%test _ = test 0x0000_0000_0000_0000 ""
  let%test _ = test 0x0100_0000_0000_0061 "a"
  let%test _ = test 0x0200_0000_0000_6261 "ab"
  let%test _ = test 0x0300_0000_0063_6261 "abc"
  let%test _ = test 0x0400_0000_6463_6261 "abcd"
  let%test _ = test 0x0500_0065_6463_6261 "abcde"
  let%test _ = test 0x0600_6665_6463_6261 "abcdef"
  let%test _ = test 0x0767_6665_6463_6261 "abcdefg"

  (* Fail: of_string with length > 7. *)
  let%test _ = Exn.does_raise (fun () -> of_string "abcdefgh")
  let%test _ = Exn.does_raise (fun () -> of_string "abcdefghi")
  let%test _ = Exn.does_raise (fun () -> of_string "abcdefghij")
  let%test _ = Exn.does_raise (fun () -> of_string "abcdefghijk")

  (* Fail: of_int_exn with one or more of bits 59-62 set. *)
  let%test _ = Exn.does_raise (fun () -> of_int_exn (1 lsl 62))
  let%test _ = Exn.does_raise (fun () -> of_int_exn (1 lsl 61))
  let%test _ = Exn.does_raise (fun () -> of_int_exn (1 lsl 60))
  let%test _ = Exn.does_raise (fun () -> of_int_exn (1 lsl 59))
  let%test _ = Exn.does_raise (fun () -> of_int_exn (-1))

  (* Fail: of_int_exn where length does not match char data. *)
  (* Lengths set one too short for character data: *)
  let%test _ = Exn.does_raise (fun () -> of_int_exn 0x0000_0000_0000_0061)
  let%test _ = Exn.does_raise (fun () -> of_int_exn 0x0100_0000_0000_6261)
  let%test _ = Exn.does_raise (fun () -> of_int_exn 0x0200_0000_0063_6261)
  let%test _ = Exn.does_raise (fun () -> of_int_exn 0x0300_0000_6463_6261)
  let%test _ = Exn.does_raise (fun () -> of_int_exn 0x0400_0065_6463_6261)
  let%test _ = Exn.does_raise (fun () -> of_int_exn 0x0500_6665_6463_6261)
  let%test _ = Exn.does_raise (fun () -> of_int_exn 0x0667_6665_6463_6261)

  (* Length set to 0 for 7 characters: *)
  let%test _ = Exn.does_raise (fun () -> of_int_exn 0x0067_6665_6463_6261)
end

(* This code is very delicate with all the bit banging.  We're relying heavily on the
   tests in immediate_unit_tests.ml. *)

include Stable.V1
include (Int : Typerep_lib.Typerepable.S with type t := t)

let globalize = globalize_int
let of_char c = unsafe_set (unsafe_create 1) 0 c

let get_failure i ~len =
  failwithf "get: index %d is not in bounds [%d,%d]" i 0 (len - 1) ()
;;

let get t i =
  let len = length t in
  if 0 <= i && i < len then unsafe_get t i else get_failure i ~len
;;

let empty = unsafe_create 0
let is_empty t = Int.equal t empty

module For_mem = struct
  (* The high byte will be masked with 0, as it contains our tag bit and length. *)
  let lo_bits = 0x00_01_01_01__01_01_01_01
  let hi_bits = 0x00_80_80_80__80_80_80_80

  let has_non_zero_char t c =
    let mask = lo_bits * Char.to_int c in
    (* [x] evaluates to an integer where any byte in [t] equal to [char] is now zero. *)
    let x = t lxor mask in
    (* [x2] evaluates to a high bit set in any byte whenever the corresponding byte in [x]
       is zero or greater than 0x80 (except that, if it's 0x81 and the next less
       significant byte is 0x00, the subtraction will carry to produce 0x79 - but in those
       cases the 0x00 byte to the right will serve as witness for [mem]). *)
    let x2 = x - lo_bits in
    (* [x3] evaluates to high bits set in bytes where the byte of [x] doesn't have its
       high bit set (so the byte was less than 0x80). *)
    let x3 = lnot x land hi_bits in
    (* Finally, by ANDing [x2] and [x3] the result is the high bits set of any byte in [x]
       which was zero, since the high bits set due to a value greater than 0x80 in the
       first sub-expression are masked off by the second (again modulo the 0x8100
       exception, which always leaves at least the rightmost equal byte to witness
       [mem]). *)
    x2 land x3 <> 0
  ;;

  (* Search for ['\000'] is a special case in two ways. The first is that the initial mask
     is not necessary (though not harmful), the second is that we need to account for the
     length or we would find our zeroed-out pad bytes. *)
  let has_zero_char t =
    let shift = 8 * (7 - length t) in
    let x2 = t - lo_bits in
    let x3 = lnot t land (hi_bits lsr shift) in
    x2 land x3 <> 0
  ;;
end

let[@zero_alloc] mem t char =
  let open For_mem in
  (* Since [mem] is usually called with a constant search [char], only one version of
       this code is typically inlined. *)
  if Char.equal char '\000' then has_zero_char t else has_non_zero_char t char
;;

let%expect_test "works even if we have to carry (from the left) to compute x2" =
  print_s [%message (mem (of_string "\x00\x81") '\000' : bool)];
  [%expect {| ("mem (of_string \"\\000\\129\") '\\000'" true) |}];
  print_s [%message (mem (of_string "\x81\x00") '\x81' : bool)];
  [%expect {| ("mem (of_string \"\\129\\000\") '\\129'" true) |}]
;;

let[@cold] append_failure fst snd ~len =
  raise_s
    [%message
      "Immediate.Short_string.append_exn: exceeded maximum length"
        (fst : t)
        (snd : t)
        (len : int)
        (max_length : int)]
;;

let append_exn fst snd =
  let fst_len = length fst in
  let snd_len = length snd in
  if fst_len = 0
  then snd
  else if snd_len = 0
  then fst
  else (
    let len = fst_len + snd_len in
    if len > max_length then append_failure fst snd ~len;
    let t = ref (unsafe_set_len fst len) in
    for i = 0 to snd_len - 1 do
      t := unsafe_set !t (fst_len + i) (unsafe_get snd i)
    done;
    !t)
;;

let%test_unit "append_exn" =
  [%test_result: t] (append_exn empty empty) ~expect:empty;
  [%test_result: t] (append_exn (of_char 'X') empty) ~expect:(of_char 'X');
  [%test_result: t] (append_exn empty (of_char 'X')) ~expect:(of_char 'X');
  [%test_result: t]
    (append_exn (of_string "abc") (of_string "def"))
    ~expect:(of_string "abcdef");
  assert (does_raise (fun () -> append_exn (of_string "123456") (of_string "654321")))
;;

include Identifiable.Make [@modality portable] (struct
    include Stable.V1

    let module_name = "Immediate.Short_string"
  end)

include Int.Replace_polymorphic_compare

module%bench [@name "comparisons"] _ = struct
  let t = of_string "abc"

  module%bench [@name "built-in"] _ = struct
    open Poly

    let%bench "=" = t = t
    let%bench "<" = t < t
    let%bench ">" = t > t
    let%bench "<=" = t <= t
    let%bench ">=" = t >= t
    let%bench "<>" = t <> t
  end

  module%bench [@name "exported"] _ = struct
    let%bench "=" = t = t
    let%bench "<" = t < t
    let%bench ">" = t > t
    let%bench "<=" = t <= t
    let%bench ">=" = t >= t
    let%bench "<>" = t <> t
  end
end

module Lexicographic = struct
  type nonrec t = t

  include Identifiable.Make [@modality portable] (struct
      type nonrec t = t [@@deriving bin_io, hash, sexp]

      let of_string = of_string
      let to_string = to_string
      let module_name = "Immediate.Short_string.Lexicographic"
      let compare = lexicographic_compare
    end)
end

module%test Lexicographic = struct
  let lex_is_ok (x, y) =
    String.compare x y = Lexicographic.compare (of_string x) (of_string y)
  ;;

  let test x y = [%test_pred: string * string] lex_is_ok (x, y)

  let strings =
    [ ""
    ; "a"
    ; "b"
    ; "aa"
    ; "ab"
    ; "ba"
    ; "bb"
    ; "aaa"
    ; "aab"
    ; "aba"
    ; "abb"
    ; "baa"
    ; "bab"
    ; "bba"
    ; "bbb"
    ]
  ;;

  let%test_unit _ =
    List.iter strings ~f:(fun x -> List.iter strings ~f:(fun y -> test x y))
  ;;
end

module Option = struct
  module Stable = Option_stable
  include Stable.V1
  include (Int : Typerep_lib.Typerepable.S with type t := t)

  module Optional_syntax = struct
    module Optional_syntax = struct
      let[@zero_alloc] is_none t = is_none t
      let[@zero_alloc] unsafe_value t = unchecked_value t
    end
  end

  include Identifiable.Make [@modality portable] (struct
      include Stable.V1
      include Sexpable.To_stringable [@modality portable] (Stable.V1)

      let module_name = "Immediate.Short_string.Option"
    end)
end

let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

let quickcheck_observer =
  (Quickcheck.Observer.unmap [@mode portable]) String.quickcheck_observer ~f:to_string
;;

let%template gen_with_length len char_gen =
  (String.gen_with_length [@mode p]) len char_gen
  |> (Quickcheck.Generator.map [@mode p]) ~f:of_string
[@@mode p = (portable, nonportable)]
;;

let%template gen' char_gen =
  let open Base_quickcheck.Generator.Let_syntax [@modality p] in
  let%bind len = Int.gen_incl 0 max_length in
  (gen_with_length [@mode p]) len char_gen
[@@mode p = (portable, nonportable)]
;;

let quickcheck_generator = (gen' [@mode portable]) Char.quickcheck_generator

let[@inline] unsafe_of_bigstring ~pos ~len buf =
  let t = ref (unsafe_create len) in
  for i = 0 to len - 1 do
    t := unsafe_set !t i (Bigstring.unsafe_get buf (pos + i))
  done;
  !t
;;

let[@inline] unsafe_of_iobuf_peek ~pos ~len buf =
  unsafe_of_bigstring (Iobuf.Expert.buf buf) ~pos:(Iobuf.Expert.lo buf + pos) ~len
;;

let unsafe_of_iobuf_consume ~len buf =
  let t = unsafe_of_iobuf_peek ~len ~pos:0 buf in
  Iobuf.unsafe_advance buf len;
  t
;;

let unsafe_to_iobuf_poke t ~pos buf =
  let len = length t in
  for i = 0 to len - 1 do
    Iobuf.Unsafe.Poke.char buf ~pos:(pos + i) (unsafe_get t i)
  done
;;

let unsafe_to_bigstring t ~pos buf =
  let len = length t in
  for i = 0 to len - 1 do
    Bigstring.unsafe_set buf (pos + i) (unsafe_get t i)
  done
;;

let unsafe_to_iobuf_fill t buf =
  unsafe_to_iobuf_poke t ~pos:0 buf;
  Iobuf.unsafe_advance buf (length t)
;;

(*$ Immediate_cinaps_helpers.write ~module_name:"Immediate.Short_string" *)
module Unpadded = struct
  let of_iobuf_peek ?pos ?len buf =
    Iobuf_accessors.For_cinaps.checked_read_with_pos_and_len
      ?pos
      ?len
      buf
      (fun ~pos ~len buf -> unsafe_of_iobuf_peek ~pos ~len buf)
      "Immediate.Short_string.of_iobuf_peek" [@nontail]
  ;;

  let of_iobuf_consume ?len buf =
    Iobuf_accessors.For_cinaps.checked_read_with_len
      ?len
      buf
      (fun ~len buf -> unsafe_of_iobuf_consume ~len buf)
      "Immediate.Short_string.of_iobuf_consume" [@nontail]
  ;;

  let to_iobuf_poke t ?pos buf =
    Iobuf_accessors.For_cinaps.checked_write_with_pos
      t
      ~length
      ?pos
      buf
      (fun t ~pos buf -> unsafe_to_iobuf_poke t ~pos buf)
      "Immediate.Short_string.to_iobuf_poke" [@nontail]
  ;;

  let to_iobuf_fill t buf =
    Iobuf_accessors.For_cinaps.checked_write
      t
      ~length
      buf
      (fun t buf -> unsafe_to_iobuf_fill t buf)
      "Immediate.Short_string.to_iobuf_fill" [@nontail]
  ;;
end

include Unpadded

module Padded = struct
  let unsafe_of_iobuf_peek ~padding ~pos ~len:padded_length buf =
    let unpadded_length =
      Iobuf_accessors.For_cinaps.read_padding_and_get_unpadded_length
        ~padding
        ~pos
        ~padded_length
        buf
    in
    unsafe_of_iobuf_peek ~pos ~len:unpadded_length buf
  ;;

  let unsafe_of_iobuf_consume ~padding ~len buf =
    let t = unsafe_of_iobuf_peek ~padding ~len ~pos:0 buf in
    Iobuf.unsafe_advance buf len;
    t
  ;;

  let unsafe_of_bigstring ~padding ~pos ~len:padded_length buf =
    let unpadded_length =
      Iobuf_accessors.For_cinaps.bigstring_read_padding_and_get_unpadded_length
        ~padding
        ~pos
        ~padded_length
        buf
    in
    unsafe_of_bigstring buf ~pos ~len:unpadded_length
  ;;

  let unsafe_to_bigstring t ~padding ~pos ~len:padded_length buf =
    unsafe_to_bigstring t buf ~pos;
    Iobuf_accessors.For_cinaps.bigstring_write_padding
      ~padding
      ~pos
      ~unpadded_length:(length t)
      ~padded_length
      buf
  ;;

  let unsafe_to_iobuf_poke t ~padding ~pos ~len:padded_length buf =
    unsafe_to_iobuf_poke t ~pos buf;
    Iobuf_accessors.For_cinaps.write_padding
      ~padding
      ~pos
      ~unpadded_length:(length t)
      ~padded_length
      buf
  ;;

  let unsafe_to_iobuf_fill t ~padding ~len buf =
    unsafe_to_iobuf_poke t ~padding ~len ~pos:0 buf;
    Iobuf.unsafe_advance buf len
  ;;

  let of_iobuf_peek ~padding ?pos ?len buf =
    Iobuf_accessors.For_cinaps.checked_read_with_pos_and_len
      ?pos
      ?len
      buf
      (fun ~pos ~len buf -> unsafe_of_iobuf_peek ~padding ~pos ~len buf)
      "Immediate.Short_string.Padded.of_iobuf_peek" [@nontail]
  ;;

  let of_iobuf_consume ~padding ?len buf =
    Iobuf_accessors.For_cinaps.checked_read_with_len
      ?len
      buf
      (fun ~len buf -> unsafe_of_iobuf_consume ~padding ~len buf)
      "Immediate.Short_string.Padded.of_iobuf_consume" [@nontail]
  ;;

  let to_iobuf_poke t ~padding ?pos ?len buf =
    Iobuf_accessors.For_cinaps.checked_write_with_pos_and_len
      t
      ~length
      ?pos
      ?len
      buf
      (fun t ~pos ~len buf -> unsafe_to_iobuf_poke ~padding t ~pos ~len buf)
      "Immediate.Short_string.Padded.to_iobuf_poke" [@nontail]
  ;;

  let to_iobuf_fill t ~padding ?len buf =
    Iobuf_accessors.For_cinaps.checked_write_with_len
      t
      ~length
      ?len
      buf
      (fun t ~len buf -> unsafe_to_iobuf_fill ~padding t ~len buf)
      "Immediate.Short_string.Padded.to_iobuf_fill" [@nontail]
  ;;
end

(*$*)

(* It is not possible to test with [Base_for_tests.Test_blit.Test_distinct] in
   [../test/test_imm_strings.ml] because that requires the source sequence to be mutable.
   However, this is still tested via [Immediate.String.Stable.V2.bin_write_t]. *)
module To_bigstring =
  Blit.Make_distinct [@modality portable]
    (struct
      type nonrec t = t

      let length t = length t
    end)
    (struct
      type t = Bigstring.t

      let length = Bigstring.length
      let create ~len = Bigstring.create len

      let[@inline always] drop_bytes int ~num_bytes =
        let num_bits = num_bytes lsl 3 in
        int lsr num_bits
      ;;

      let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
        let src = ref (drop_bytes src ~num_bytes:src_pos) in
        let dst_pos = ref dst_pos in
        (* write a 4-byte unit if there's one to write *)
        if len land 4 = 4
        then (
          Bigstring.unsafe_set_int32_le dst ~pos:!dst_pos !src;
          dst_pos := !dst_pos + 4;
          src := drop_bytes !src ~num_bytes:4);
        (* write a 2-byte unit if there's one to write *)
        if len land 2 = 2
        then (
          Bigstring.unsafe_set_int16_le dst ~pos:!dst_pos !src;
          dst_pos := !dst_pos + 2;
          src := drop_bytes !src ~num_bytes:2);
        (* write a 1-byte unit if there's one to write *)
        if len land 1 = 1 then Bigstring.unsafe_set_int8 dst ~pos:!dst_pos !src
      ;;
    end)
