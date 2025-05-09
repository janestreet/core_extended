(* Immediate_string represents strings using Immediate_short_string if they are short
   enough, and Immediate_interned_string otherwise.

   The integer representation relies on the facts that both Immediate_short_string and
   Immediate_interned_string have non-negative integer representations, and that
   Immediate_short_string.Option.none is a non-negative integer distinct from the
   representation of all (short) strings.

   Short strings are encoded directly using the integer encoding of
   Immediate_short_string.  Longer strings are encoded by negating the bits of the integer
   encoding of Immediate_interned_string, which produces a negative number and is
   therefore always distinct from the short string encoding.

   Options are encoded using the above integer encoding for Some and
   Immediate_short_string.Option.none for None, since it is guaranteed to be non-negative
   and distinct from the encoding of all short strings. *)

(* This code is very delicate with all the bit banging.  We're relying heavily on the
   tests in immediate_unit_tests.ml. *)

module Make (Interned : Immediate_interned_string.S) = struct
  open Core.Core_stable
  module Short_string = Immediate_short_string
  module Interned = Interned
  module Interned_string = Interned

  module Stable = struct
    open Typerep_lib.Std (* for [typerep_of_int] *)

    module V1 = struct
      module T_stringable = struct
        (* This [int] is not transportable, so don't derive [bin_io]. *)
        type t = int
        [@@deriving hash, typerep, globalize, compare ~localize, equal ~localize]

        let[@inline] is_interned t = t < 0
        let[@inline] unsafe_of_short s = Short_string.unsafe_to_int s
        let[@inline] unsafe_of_interned i = Interned_string.unsafe_to_int i lxor -1
        let[@inline] unsafe_to_interned t = Interned_string.unsafe_of_int (t lxor -1)
        let[@inline] unsafe_to_short t = Short_string.unsafe_of_int t
        let of_short_string = [%eta1 unsafe_of_short]

        let[@inline] of_interned_string i =
          if Short_string.is_valid_length (Interned_string.length i)
          then
            unsafe_of_short (Short_string.of_local_string (Interned_string.to_string i))
          else unsafe_of_interned i
        ;;

        let[@cold] to_interned_string_exn_not_found short_string =
          Not_found_s
            [%message
              "[Immediate.String.to_interned_string_exn]: given a short string"
                ~_:(short_string : Short_string.t)]
        ;;

        let to_interned_string_exn t =
          if is_interned t
          then unsafe_to_interned t
          else raise (to_interned_string_exn_not_found (unsafe_to_short t))
        ;;

        let[@cold] to_short_string_exn_not_found interned_string =
          Not_found_s
            [%message
              "[Immediate.String.to_short_string_exn]: given an interned string"
                ~_:(interned_string : Interned_string.t)]
        ;;

        let to_short_string_exn t =
          if is_interned t
          then raise (to_short_string_exn_not_found (unsafe_to_interned t))
          else unsafe_to_short t
        ;;

        let unsafe_of_int t = t
        let unsafe_to_int t = t

        let of_int_exn n =
          if n < 0
          then of_interned_string (Interned_string.of_int_exn (n lxor -1))
          else of_short_string (Short_string.of_int_exn n)
        ;;

        let to_int_exn t = t

        let of_local_string (local_ str) =
          if Short_string.is_valid_string str
          then unsafe_of_short (Short_string.of_local_string str)
          else unsafe_of_interned (Interned_string.of_local_string str)
        ;;

        let to_local_string t = exclave_
          if is_interned t
          then Interned_string.to_string (unsafe_to_interned t)
          else Short_string.to_local_string (unsafe_to_short t)
        ;;

        let of_string str = of_local_string str

        let to_string t =
          if is_interned t
          then Interned_string.to_string (unsafe_to_interned t)
          else Short_string.to_string (unsafe_to_short t)
        ;;

        let lexicographic_compare t u =
          if is_interned t || is_interned u
          then Core.String.compare (to_string t) (to_string u)
          else
            Short_string.Stable.V1.lexicographic_compare
              (unsafe_to_short t)
              (unsafe_to_short u)
        ;;

        let compare = `removed_because_not_antisymmetric

        let mem t char =
          if is_interned t
          then Interned_string.mem (unsafe_to_interned t) char
          else Short_string.mem (unsafe_to_short t) char
        ;;
      end

      include T_stringable
      include Binable.Of_stringable.V1 [@alert "-legacy"] (T_stringable)

      let stable_witness =
        Stable_witness.of_serializable
          stable_witness_string
          T_stringable.of_string
          T_stringable.to_string
      ;;

      (* Override [bin_writer_t] to make it noalloc.

         Don't bother with [bin_reader_t] yet, just for convenience. *)
      let bin_write_t dst ~pos t =
        if is_interned t
        then
          (* This is already noalloc and [string]-shaped. *)
          Interned_string.Stable.V1.bin_write_t dst ~pos (unsafe_to_interned t)
        else (
          (* [Short_string.Stable.V1] is not [string]-shaped. *)
          let src = unsafe_to_short t in
          let len = Short_string.length src in
          let dst_pos =
            Bin_prot.Write.bin_write_nat0 dst ~pos (Bin_prot.Nat0.of_int len)
          in
          Short_string.To_bigstring.blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
          dst_pos + len)
      ;;

      let%template[@mode local] bin_write_t = bin_write_t

      let bin_size_t t =
        if is_interned t
        then Interned_string.Stable.V1.bin_size_t (unsafe_to_interned t)
        else (
          let len = Short_string.length (unsafe_to_short t) in
          Bin_prot.Size.bin_size_nat0 (Bin_prot.Nat0.of_int len) + len)
      ;;

      let%template[@mode local] bin_size_t = bin_size_t

      let bin_writer_t : t Bin_prot.Type_class.writer =
        { write = bin_write_t; size = bin_size_t }
      ;;

      let bin_t = { bin_t with writer = bin_writer_t }

      include Sexpable.Of_stringable.V1 (T_stringable)
      module For_testing_only = T_stringable
    end

    module V2 = struct
      module T = struct
        module T' = struct
          include V1

          (* slow but stable *)
          let compare = lexicographic_compare
        end

        include Comparator.V1.Make (T')
        include T'
      end

      include T
      include Comparable.V1.With_stable_witness.Make (T)

      let of_v1 t = t
    end
  end

  module Option_stable = struct
    module V1 = struct
      type t = int [@@deriving globalize, hash]

      let none = Core.Int.max_value
      let is_none t = t = none
      let is_some t = not (is_none t)
      let some (t : Stable.V1.t) = t
      let some_is_representable _ = true
      let unchecked_value (t : t) = t

      let%template[@mode m = (global, local)] to_option t =
        if is_none t
        then None
        else (
          let v = unchecked_value t in
          Some v [@exclave_if_local m])
      ;;

      let of_option = function
        | None -> none
        | Some v -> some v
      ;;

      let sexp_of_t t = to_option t |> [%sexp_of: Stable.V1.t option]
      let t_of_sexp s = [%of_sexp: Stable.V1.t option] s |> of_option

      module Serializable = struct
        type t = Stable.V1.t option [@@deriving bin_io ~localize, stable_witness]
      end

      include%template
        Binable.Of_binable.V1 [@mode local] [@alert "-legacy"]
          (Serializable)
          (struct
            type nonrec t = t

            let of_binable = of_option
            let%template[@mode m = (global, local)] to_binable = (to_option [@mode m])
          end)

      let stable_witness =
        Stable_witness.of_serializable Serializable.stable_witness of_option to_option
      ;;

      let to_string_option t =
        if is_none t then None else Some (Stable.V1.to_string (unchecked_value t))
      ;;

      let of_string_option = function
        | None -> none
        | Some s -> some (Stable.V1.of_string s)
      ;;

      let compare = `removed_because_unstable

      module For_testing_only = struct
        let of_option = of_option
        let to_option = to_option
        let representation t = t
      end
    end

    module V2 = struct
      include V1

      let compare t u =
        if is_none t
        then if is_none u then 0 else -1
        else if is_none u
        then 1
        else Stable.V2.compare (unchecked_value t) (unchecked_value u)
      ;;

      let%template[@mode local] [@inline] compare t u = compare t u
      let of_v1 t = t
    end
  end

  open Core

  let to_immediate_string t = t
  let of_immediate_string = Fn.id

  module T_hash = struct
    include Stable.V1
    include (Int : Typerep_lib.Typerepable.S with type t := t)

    let empty = of_short_string Short_string.empty
    let is_empty t = t = empty

    let[@inline] length t =
      if is_interned t
      then Interned_string.length (unsafe_to_interned t)
      else Short_string.length (unsafe_to_short t)
    ;;

    let of_char t = unsafe_of_short (Short_string.of_char t)

    let get t i =
      if is_interned t
      then Interned_string.get (unsafe_to_interned t) i
      else Short_string.get (unsafe_to_short t) i
    ;;

    let unsafe_get t i =
      if is_interned t
      then Interned_string.unsafe_get (unsafe_to_interned t) i
      else Short_string.unsafe_get (unsafe_to_short t) i
    ;;

    include Int.Replace_polymorphic_compare

    (* fast but not stable; see .mli *)
  end

  include T_hash

  let quickcheck_generator =
    Quickcheck.Generator.map String.quickcheck_generator ~f:of_string
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.unmap String.quickcheck_observer ~f:to_string
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

  module Stats = Interned_string.Stats

  let grow_by, after_grow = Interned_string.(grow_by, after_grow)

  include Identifiable.Make (struct
      include T_hash

      let module_name = "Immediate.String"
    end)

  module Lexicographic = struct
    type nonrec t = t

    module Comparator :
      Comparator.S
      with type t := t
       and type comparator_witness = Stable.V2.comparator_witness =
      Stable.V2

    include Comparator

    include Identifiable.Make_using_comparator (struct
        include T_hash
        include Comparator

        let module_name = "Immediate.String.Lexicographic"
      end)
  end

  module Option = struct
    include Option_stable.V1
    include (Int : Typerep_lib.Typerepable.S with type t := t)

    let value_exn_not_found =
      Not_found_s [%message "[Immediate.String.Option.value_exn]: given [none]"]
    ;;

    let value_exn t = if is_some t then t else raise value_exn_not_found
    let value t ~default = Bool.select (is_none t) default t

    module Stable = Option_stable

    module Optional_syntax = struct
      module Optional_syntax = struct
        let[@zero_alloc] is_none t = is_none t
        let[@zero_alloc] unsafe_value t = unchecked_value t
      end
    end

    include Identifiable.Make (struct
        include Option_stable.V1

        let compare = Int.compare

        include Sexpable.To_stringable (Option_stable.V1)

        let module_name = "Immediate.String.Option"
      end)

    let%template[@mode local] [@inline] compare t u = compare t u
    let%template[@mode local] [@inline] equal t u = equal t u
    let to_immediate_string_option = Fn.id
    let of_immediate_string_option = Fn.id
  end

  let of_string_no_intern (local_ str) =
    if Short_string.is_valid_string str
    then unsafe_of_short (Short_string.of_local_string str) |> Option.some
    else (
      match%optional.Interned.Option Interned.of_string_no_intern str with
      | None -> Option.none
      | Some str -> unsafe_of_interned str |> Option.some)
  ;;

  let[@inline] unsafe_of_bigstring ~pos ~len buf =
    if Short_string.is_valid_length len
    then unsafe_of_short (Short_string.unsafe_of_bigstring ~pos ~len buf)
    else unsafe_of_interned (Interned_string.unsafe_of_bigstring ~pos ~len buf)
  ;;

  let[@inline] unsafe_of_iobuf_peek ~pos ~len buf =
    if Short_string.is_valid_length len
    then unsafe_of_short (Short_string.unsafe_of_iobuf_peek ~pos ~len buf)
    else unsafe_of_interned (Interned_string.unsafe_of_iobuf_peek ~pos ~len buf)
  ;;

  let unsafe_of_iobuf_consume ~len buf =
    let t = unsafe_of_iobuf_peek ~len ~pos:0 buf in
    Iobuf.unsafe_advance buf len;
    t
  ;;

  let unsafe_to_bigstring t ~pos buf =
    if is_interned t
    then Interned_string.unsafe_to_bigstring (unsafe_to_interned t) ~pos buf
    else Short_string.unsafe_to_bigstring (unsafe_to_short t) ~pos buf
  ;;

  let unsafe_to_iobuf_poke t ~pos buf =
    if is_interned t
    then Interned_string.unsafe_to_iobuf_poke (unsafe_to_interned t) ~pos buf
    else Short_string.unsafe_to_iobuf_poke (unsafe_to_short t) ~pos buf
  ;;

  let unsafe_to_iobuf_fill t buf =
    unsafe_to_iobuf_poke t ~pos:0 buf;
    Iobuf.unsafe_advance buf (length t)
  ;;

  (*$ Immediate_cinaps_helpers.write ~module_name:"Immediate.String" *)
  module Unpadded = struct
    let of_iobuf_peek ?pos ?len buf =
      Iobuf_accessors.For_cinaps.checked_read_with_pos_and_len
        ?pos
        ?len
        buf
        (fun ~pos ~len buf -> unsafe_of_iobuf_peek ~pos ~len buf)
        "Immediate.String.of_iobuf_peek" [@nontail]
    ;;

    let of_iobuf_consume ?len buf =
      Iobuf_accessors.For_cinaps.checked_read_with_len
        ?len
        buf
        (fun ~len buf -> unsafe_of_iobuf_consume ~len buf)
        "Immediate.String.of_iobuf_consume" [@nontail]
    ;;

    let to_iobuf_poke t ?pos buf =
      Iobuf_accessors.For_cinaps.checked_write_with_pos
        t
        ~length
        ?pos
        buf
        (fun t ~pos buf -> unsafe_to_iobuf_poke t ~pos buf)
        "Immediate.String.to_iobuf_poke" [@nontail]
    ;;

    let to_iobuf_fill t buf =
      Iobuf_accessors.For_cinaps.checked_write
        t
        ~length
        buf
        (fun t buf -> unsafe_to_iobuf_fill t buf)
        "Immediate.String.to_iobuf_fill" [@nontail]
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
        "Immediate.String.Padded.of_iobuf_peek" [@nontail]
    ;;

    let of_iobuf_consume ~padding ?len buf =
      Iobuf_accessors.For_cinaps.checked_read_with_len
        ?len
        buf
        (fun ~len buf -> unsafe_of_iobuf_consume ~padding ~len buf)
        "Immediate.String.Padded.of_iobuf_consume" [@nontail]
    ;;

    let to_iobuf_poke t ~padding ?pos ?len buf =
      Iobuf_accessors.For_cinaps.checked_write_with_pos_and_len
        t
        ~length
        ?pos
        ?len
        buf
        (fun t ~pos ~len buf -> unsafe_to_iobuf_poke ~padding t ~pos ~len buf)
        "Immediate.String.Padded.to_iobuf_poke" [@nontail]
    ;;

    let to_iobuf_fill t ~padding ?len buf =
      Iobuf_accessors.For_cinaps.checked_write_with_len
        t
        ~length
        ?len
        buf
        (fun t ~len buf -> unsafe_to_iobuf_fill ~padding t ~len buf)
        "Immediate.String.Padded.to_iobuf_fill" [@nontail]
    ;;
  end

  (*$*)
end

open Core

module type S = Immediate_string_intf.S

include Make (Immediate_interned_string)
module Interned = Immediate_interned_string

module Universe = struct
  module V1 = struct
    module Make () = Make (Immediate_interned_string.Universe.V1.Make ())
  end
end

let%test_unit "Stable.V1.lexicographic_compare consistent with String.compare" =
  let variant n = if n < 0 then `Less_than else if n > 0 then `Greater_than else `Equal in
  let strings =
    [ ""
    ; "a"
    ; "aa"
    ; "aaa"
    ; "aaaa"
    ; "aaaaa"
    ; "aaaaaa"
    ; "aaaaaaa"
    ; "aaaaaaaa"
    ; "aaaaaaaaa"
    ; "aaaaaaaaaa"
    ; "aaaaaaaaab"
    ; "aaaaaaaab"
    ; "aaaaaaab"
    ; "aaaaaab"
    ; "aaaaab"
    ; "aaaab"
    ; "aaab"
    ; "aab"
    ; "ab"
    ; "ac"
    ; "b"
    ; "ca"
    ]
  in
  ListLabels.iter strings ~f:(fun str1 ->
    ListLabels.iter strings ~f:(fun str2 ->
      let t1 = of_string str1 in
      let t2 = of_string str2 in
      [%test_result: [ `Less_than | `Equal | `Greater_than ]]
        (variant (Stable.V1.lexicographic_compare t1 t2))
        ~expect:(variant (Core.String.compare str1 str2))
        ~message:(Printf.sprintf "compare %S %S" str1 str2)))
;;

let%test_unit "Stable.V2.compare consistent with String.compare" =
  let variant n = if n < 0 then `Less_than else if n > 0 then `Greater_than else `Equal in
  let strings =
    [ ""
    ; "a"
    ; "aa"
    ; "aaa"
    ; "aaaa"
    ; "aaaaa"
    ; "aaaaaa"
    ; "aaaaaaa"
    ; "aaaaaaaa"
    ; "aaaaaaaaa"
    ; "aaaaaaaaaa"
    ; "aaaaaaaaab"
    ; "aaaaaaaab"
    ; "aaaaaaab"
    ; "aaaaaab"
    ; "aaaaab"
    ; "aaaab"
    ; "aaab"
    ; "aab"
    ; "ab"
    ; "ac"
    ; "b"
    ; "ca"
    ]
  in
  ListLabels.iter strings ~f:(fun str1 ->
    ListLabels.iter strings ~f:(fun str2 ->
      let t1 = of_string str1 in
      let t2 = of_string str2 in
      [%test_result: [ `Less_than | `Equal | `Greater_than ]]
        (variant (Stable.V2.compare t1 t2))
        ~expect:(variant (Core.String.compare str1 str2))
        ~message:(Printf.sprintf "compare %S %S" str1 str2)))
;;

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

let%bench_fun "mem (short string) (found)" =
  let t = of_string "XXX.X  " in
  fun () -> Sys.opaque_identity (mem t '.')
;;

let%bench_fun "mem (short string) (not found)" =
  let t = of_string "XXX.X  " in
  fun () -> Sys.opaque_identity (mem t '/')
;;

let%bench_fun "mem (short string) (true), 0" =
  let t = of_string "XXX.X  " in
  fun () -> Sys.opaque_identity (mem t '\x00')
;;

let%bench_fun "mem (interned string) (found)" =
  let t = of_string "XXX.X        " in
  fun () -> Sys.opaque_identity (mem t '.')
;;

let%bench_fun "mem (interned string) (not found)" =
  let t = of_string "XXX.X        " in
  fun () -> Sys.opaque_identity (mem t '/')
;;

let%bench_fun "mem (interned string) (found, last char)" =
  let t = of_string "XXXXXXXXXXXXX." in
  fun () -> Sys.opaque_identity (mem t '.')
;;

module%test [@name "Comparable.Make(Stable.V2).compare antisymmetric (bug repro)"] _ =
struct
  let x = of_string "ca"
  let y = of_string "ac"
  let z = of_string "bb-interned"

  module C = Comparable.Make (Stable.V2)

  let a = C.Set.of_list [ x; y; z ]
  let a_sexp = [%sexp_of: C.Set.t] a
  let b = [%of_sexp: C.Set.t] a_sexp
  let b_sexp = [%sexp_of: C.Set.t] b
  let a_minus_b = Core.Set.diff a b
  let b_minus_a = Core.Set.diff b a

  let%test_unit _ =
    [%test_result: string] ~expect:"(ac bb-interned ca)" (Sexp.to_string a_sexp)
  ;;

  let%test_unit _ =
    [%test_result: string] ~expect:"(ac bb-interned ca)" (Sexp.to_string b_sexp)
  ;;

  let%test _ = C.Set.equal a b
  let%test _ = Core.Set.is_empty a_minus_b
  let%test _ = Core.Set.is_empty b_minus_a
  let%test _ = true && C.( < ) y x && C.( < ) y z && C.( < ) z x
end
