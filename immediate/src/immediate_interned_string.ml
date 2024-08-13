(* Interned_string represents strings as indices into a global intern table.  Every value
   is therefore a non-negative int63. *)

(* This code is very delicate with all the unsafe array accesses.  We're relying heavily
   on the tests in immediate_unit_tests.ml. *)

module Universe = struct
  module V1 = struct
    module Make () = struct
      open Core.Core_stable

      module Stable = struct
        module V1 = struct
          let initial_size = 512
          let placeholder_string = ""

          (* Array of interned strings: *)
          let interned_strings =
            ref (Core.Array.create ~len:initial_size placeholder_string)
          ;;

          let preallocated_high_water_mark = ref 0

          (* Mapping from strings to array indices: *)
          let interned_index_table =
            Pooled_hashtbl.Using_hashable.create
              ()
              ~size:initial_size
              ~growth_allowed:true
              ~hashable:Core.String.Table.hashable
          ;;

          (* Total length of interned strings: *)
          let interned_bytes = ref 0

          module Stats = struct
            let[@inline] interned_count () = Pooled_hashtbl.length interned_index_table
            let[@inline] interned_total_length () = !interned_bytes
          end

          let[@inline] unsafe_intern str index =
            Core.Array.unsafe_set !interned_strings index str;
            Pooled_hashtbl.set interned_index_table ~key:str ~data:index;
            interned_bytes := !interned_bytes + Core.String.length str
          ;;

          (* Pre-populate tables with the empty string and all singleton strings. This
             avoids repeated allocation of length 0-1 strings in [of_char] and
             [*of_iobuf*]. *)
          let () =
            unsafe_intern "" 0;
            for i = 0 to 255 do
              unsafe_intern (Core.String.of_char (Core.Char.unsafe_of_int i)) (i + 1)
            done
          ;;

          module T_stringable = struct
            (* We mustn't use [int [@@deriving bin_io]] (or [sexp]). The values (intern
               table keys) are not portable between heap images. *)
            type t = int [@@deriving compare, hash, globalize, stable_witness]

            let after_grow_handler = ref (fun ~before:_ ~len_before:_ ~len:_ -> ())

            let[@cold] resize_must_grow ~size ~original ~len =
              let new_len = Core.Int.ceil_pow2 size in
              let before = Core.Time_ns.now () in
              let copy = Core.Array.create ~len:new_len placeholder_string in
              Core.Array.blit ~src:original ~src_pos:0 ~dst:copy ~dst_pos:0 ~len;
              interned_strings := copy;
              Pooled_hashtbl.resize interned_index_table new_len;
              !after_grow_handler ~before ~len_before:len ~len:new_len
            ;;

            let[@inline] resize size =
              let original = !interned_strings in
              let len = Array.length original in
              if len < size then resize_must_grow ~size ~original ~len
            ;;

            let grow_by slots =
              let previous_high_water_mark =
                Core.Int.max !preallocated_high_water_mark (Stats.interned_count ())
              in
              let high_water_mark = previous_high_water_mark + slots in
              preallocated_high_water_mark := high_water_mark;
              resize high_water_mark
            ;;

            let after_grow f =
              let g = !after_grow_handler in
              after_grow_handler
              := fun ~before ~len_before ~len ->
                   g ~before ~len_before ~len;
                   f ~before ~len_before ~len
            ;;

            let[@inline] to_string t = Core.Array.unsafe_get !interned_strings t

            let[@inline] of_string_maybe_interned str ~if_not_interned =
              Pooled_hashtbl.find_and_call
                interned_index_table
                str
                ~if_found:Core.Fn.id
                ~if_not_found:if_not_interned
            ;;

            let[@inline] intern_string str =
              let index = Stats.interned_count () in
              resize (index + 1);
              unsafe_intern str index;
              index
            ;;

            let[@inline] of_string str =
              of_string_maybe_interned str ~if_not_interned:intern_string
            ;;
          end

          include T_stringable

          (* Rely on pre-interned empty string at index 0. *)
          let empty = 0
          let[@inline] is_empty t = t = empty

          (* Rely on pre-interned singleton strings at indices 1-256. *)
          let[@inline] of_char c = 1 + Core.Char.to_int c
          let[@inline] unsafe_get t i = Core.String.unsafe_get (to_string t) i
          let[@inline] get t i = Core.String.get (to_string t) i
          let[@inline] length t = Core.String.length (to_string t)

          let mem t char =
            let str = to_string t in
            Core.String.mem str char
          ;;

          let to_int_exn t = t

          let[@cold] of_int_exn_not_found int =
            Not_found_s
              [%message
                "[Immediate.Interned_string.of_int_exn]: given out-of-bounds value"
                  ~_:(int : int)]
          ;;

          let of_int_exn t =
            if t < 0 || t >= Stats.interned_count ()
            then raise (of_int_exn_not_found t)
            else t
          ;;

          let unsafe_to_int (t : t) = t
          let unsafe_of_int (t : int) = t

          include Sexpable.Of_stringable.V1 (T_stringable)

          module Buffered_create : sig
            (** Produces an interned [t] using a single call to one of [get_char], [blit],
                or [sub]. Never makes more than one call, so it is safe for [get_char],
                [blit], and/or [sub] to have side effects such as advancing an iobuf
                cursor. *)
            val unsafe_extract
              :  get_char:('buf -> 'pos -> char)
              -> blit:
                   (src:'buf
                    -> src_pos:'pos
                    -> dst:bytes
                    -> dst_pos:int
                    -> len:int
                    -> unit)
              -> sub:('buf -> pos:'pos -> len:int -> string)
              -> len:int
              -> pos:'pos
              -> 'buf
              -> t
          end = struct
            open Core

            let max_buffer_string_length = 128

            (* Individual per-size buffers are created lazily (initialized to empty byte
               strings) because we'd like to have a large [max_buffer_string_length], but
               we don't want everyone to pay the cost of allocating a set of buffers of
               each length.

               We end up allocating:
               - 2x string_length the first time we look up and intern a string of a
                 specific length
               - 1x string_length each subsequent time we intern a new string of a length
                 that has already been seen
               - 0x when the string is already interned *)
            let find_or_create_buffer =
              let buffer_by_length =
                Array.create ~len:(max_buffer_string_length + 1) (Bytes.create 0)
              in
              fun len ->
                let buffer_or_empty = buffer_by_length.(len) in
                if Bytes.length buffer_or_empty <> len
                then (
                  let new_buffer = Bytes.create len in
                  buffer_by_length.(len) <- new_buffer;
                  new_buffer)
                else buffer_or_empty
            ;;

            let copy_and_intern buffer =
              let new_string =
                Bytes.to_string (Bytes.unsafe_of_string_promise_no_mutation buffer)
              in
              intern_string new_string
            ;;

            let of_string_maybe_copy_buffer buffer =
              of_string_maybe_interned
                ~if_not_interned:copy_and_intern
                (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buffer)
            ;;

            (* [@inline always] to remove the indirect function calls we'd get otherwise
            *)
            let[@inline always] unsafe_extract ~get_char ~blit ~sub ~len ~pos buf =
              match len with
              | 0 -> empty
              | 1 -> of_char (get_char buf pos)
              | _ when Int.( <= ) len max_buffer_string_length ->
                let dst = find_or_create_buffer len in
                blit ~src:buf ~src_pos:pos ~dst ~dst_pos:0 ~len;
                of_string_maybe_copy_buffer dst
              | _ -> of_string (sub buf ~pos ~len)
            ;;
          end

          let unsafe_of_bigstring ~pos ~len buf =
            let module Bigstring = Core.Bigstring in
            Buffered_create.unsafe_extract
              ~get_char:Bigstring.unsafe_get
              ~blit:Bigstring.To_bytes.blit
              ~sub:Bigstring.To_string.sub
              ~len
              ~pos
              buf
          ;;

          include struct
            let bin_shape_t = Bin_prot.Shape.bin_shape_string
            let bin_size_t t = bin_size_string (to_string t)
            let bin_write_t buf ~pos t = bin_write_string buf ~pos (to_string t)

            let bin_writer_t =
              { Bin_prot.Type_class.size = bin_size_t; write = bin_write_t }
            ;;

            let bin_read_t buf ~pos_ref =
              let len = (Bin_prot.Read.bin_read_nat0 buf ~pos_ref :> int) in
              let pos = !pos_ref in
              let next = pos + len in
              pos_ref := next;
              (* It's safe to use [unsafe_of_bigstring] here, since binprot does bounds
                 checks on the overall message before it gets to us. *)
              unsafe_of_bigstring ~pos ~len buf
            ;;

            let __bin_read_t__ _buf ~pos_ref:_ _i =
              (* [t] is not polymorphic. this function is unused *)
              assert false
            ;;

            let bin_reader_t =
              { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ }
            ;;

            let bin_t =
              { Bin_prot.Type_class.shape = bin_shape_t
              ; writer = bin_writer_t
              ; reader = bin_reader_t
              }
            ;;
          end
        end
      end

      module Option_stable = struct
        module V1 = struct
          type t = int [@@deriving compare, hash, stable_witness]

          let none = -1
          let is_none t = t < 0
          let is_some t = not (is_none t)
          let some t = t
          let unchecked_value t = t
          let some_is_representable _ = true
          let to_option t = if is_some t then Some (unchecked_value t) else None

          let of_option = function
            | None -> none
            | Some v -> some v
          ;;

          let sexp_of_t t = to_option t |> [%sexp_of: Stable.V1.t option]
          let t_of_sexp s = [%of_sexp: Stable.V1.t option] s |> of_option

          include struct
            (* The shape of the bin representation is as follow:

               {v   +--- Boolean value corresponding to [is_some]
                    |  +----- [Stable.V1] bin representation iff [is_some]
                    |  |
                    v  v
                    b_ssss...
               v} *)

            let bin_shape_t = Bin_prot.Shape.(bin_shape_option bin_shape_string)

            let bin_size_t t =
              if is_none t
              then bin_size_bool false
              else bin_size_bool true + Stable.V1.bin_size_t (unchecked_value t)
            ;;

            let bin_write_t buf ~pos t =
              if is_none t
              then bin_write_bool buf ~pos false
              else (
                let new_pos = bin_write_bool buf ~pos true in
                let value = unchecked_value t in
                Stable.V1.bin_write_t buf ~pos:new_pos value)
            ;;

            let bin_writer_t =
              { Bin_prot.Type_class.size = bin_size_t; write = bin_write_t }
            ;;

            let bin_read_t buf ~pos_ref =
              let is_none = not (bin_read_bool buf ~pos_ref) in
              if is_none
              then none
              else (
                let value = Stable.V1.bin_read_t buf ~pos_ref in
                some value)
            ;;

            let __bin_read_t__ _buf ~pos_ref:_ _i =
              (* [t] is not polymorphic. this function is unused *)
              assert false
            ;;

            let bin_reader_t =
              { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ }
            ;;

            let bin_t =
              { Bin_prot.Type_class.shape = bin_shape_t
              ; writer = bin_writer_t
              ; reader = bin_reader_t
              }
            ;;
          end

          module For_testing_only = struct
            let of_option = of_option
            let to_option = to_option
          end
        end
      end

      open Core
      include Immediate_interned_string_intf
      include Stable.V1
      include (Int : Typerep_lib.Typerepable.S with type t := t)

      include Identifiable.Make (struct
          include Stable.V1

          let module_name = "Immediate.Interned_string"
        end)

      include Int.Replace_polymorphic_compare

      let%bench_module "comparisons" =
        (module struct
          let t = of_string "abc"

          let%bench_module "built-in" =
            (module struct
              open Poly

              let%bench "=" = t = t
              let%bench "<" = t < t
              let%bench ">" = t > t
              let%bench "<=" = t <= t
              let%bench ">=" = t >= t
              let%bench "<>" = t <> t
            end)
          ;;

          let%bench_module "exported" =
            (module struct
              let%bench "=" = t = t
              let%bench "<" = t < t
              let%bench ">" = t > t
              let%bench "<=" = t <= t
              let%bench ">=" = t >= t
              let%bench "<>" = t <> t
            end)
          ;;
        end)
      ;;

      let quickcheck_generator =
        Quickcheck.Generator.map String.quickcheck_generator ~f:of_string
      ;;

      let quickcheck_observer =
        Quickcheck.Observer.unmap String.quickcheck_observer ~f:to_string
      ;;

      let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

      module Lexicographic = struct
        type nonrec t = t

        include Identifiable.Make (struct
            include Stable.V1

            let compare x y = String.compare (to_string x) (to_string y)
            let module_name = "Immediate.Interned_string.Lexicographic"
          end)
      end

      module Option = struct
        include Option_stable.V1
        include (Int : Typerep_lib.Typerepable.S with type t := t)

        let value_exn_not_found =
          Not_found_s
            [%message "[Immediate.Interned_string.Option.value_exn]: given [none]"]
        ;;

        let value_exn t = if is_some t then t else raise value_exn_not_found
        let value t ~default = Bool.select (is_none t) default t

        let to_string_option t =
          if is_none t then None else Some (Stable.V1.to_string (unchecked_value t))
        ;;

        let of_string_option = function
          | None -> none
          | Some s -> some (Stable.V1.of_string s)
        ;;

        module Optional_syntax = struct
          module Optional_syntax = struct
            let is_none = is_none
            let unsafe_value = unchecked_value
          end
        end

        include Identifiable.Make (struct
            include Option_stable.V1

            let compare = Int.compare

            include Sexpable.To_stringable (Option_stable.V1)

            let module_name = "Immediate.Interned_string.Option"
          end)

        module Stable = Option_stable
      end

      let const_none = Fn.const Option.none
      let i_promise_not_to_persist_this str : string = Obj.magic Obj.magic str

      let of_string_no_intern str =
        Pooled_hashtbl.find_and_call
          interned_index_table
          (i_promise_not_to_persist_this str)
          ~if_found:Option.some
          ~if_not_found:const_none
      ;;

      let[@inline] of_local_string str =
        match%optional.Option of_string_no_intern str with
        | Some _ as s -> s
        | None -> of_string (String.globalize str)
      ;;

      let unsafe_of_iobuf_peek =
        let[@inline] get_char buf pos = Iobuf.Unsafe.Peek.char buf ~pos in
        let[@inline] sub buf ~pos ~len = Iobuf.Unsafe.Peek.stringo buf ~pos ~len in
        fun [@inline] ~pos ~len buf ->
          Buffered_create.unsafe_extract
            ~get_char
            ~blit:Iobuf.Unsafe.Peek.To_bytes.blit
            ~sub
            ~len
            ~pos
            (buf :> (read, _) Iobuf.t)
      ;;

      let unsafe_of_iobuf_consume =
        let get_char buf () = Iobuf.Unsafe.Consume.char buf in
        let blit ~src ~src_pos:() ~dst ~dst_pos ~len =
          Iobuf.Unsafe.Consume.To_bytes.blit ~src ~dst ~dst_pos ~len
        in
        let sub buf ~pos:() ~len = Iobuf.Unsafe.Consume.stringo buf ~len in
        fun ~len buf ->
          Buffered_create.unsafe_extract
            ~get_char
            ~blit
            ~sub
            ~len
            ~pos:()
            (buf :> (read, _) Iobuf.t)
      ;;

      let unsafe_to_iobuf_poke t ~pos buf =
        Iobuf.Unsafe.Poke.stringo buf ~pos (to_string t)
      ;;

      let unsafe_to_bigstring t ~pos buf =
        let str = to_string t in
        Bigstring.From_string.unsafe_blit
          ~dst:buf
          ~dst_pos:pos
          ~len:(String.length str)
          ~src:str
          ~src_pos:0
      ;;

      let unsafe_to_iobuf_fill t buf = Iobuf.Unsafe.Fill.stringo buf (to_string t)

      (*$ Immediate_cinaps_helpers.write ~module_name:"Immediate.Interned_string" *)
      module Unpadded = struct
        let of_iobuf_peek ?pos ?len buf =
          Iobuf_accessors.For_cinaps.checked_read_with_pos_and_len
            ?pos
            ?len
            buf
            (fun ~pos ~len buf -> unsafe_of_iobuf_peek ~pos ~len buf)
            "Immediate.Interned_string.of_iobuf_peek" [@nontail]
        ;;

        let of_iobuf_consume ?len buf =
          Iobuf_accessors.For_cinaps.checked_read_with_len
            ?len
            buf
            (fun ~len buf -> unsafe_of_iobuf_consume ~len buf)
            "Immediate.Interned_string.of_iobuf_consume" [@nontail]
        ;;

        let to_iobuf_poke t ?pos buf =
          Iobuf_accessors.For_cinaps.checked_write_with_pos
            t
            ~length
            ?pos
            buf
            (fun t ~pos buf -> unsafe_to_iobuf_poke t ~pos buf)
            "Immediate.Interned_string.to_iobuf_poke" [@nontail]
        ;;

        let to_iobuf_fill t buf =
          Iobuf_accessors.For_cinaps.checked_write
            t
            ~length
            buf
            (fun t buf -> unsafe_to_iobuf_fill t buf)
            "Immediate.Interned_string.to_iobuf_fill" [@nontail]
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
            "Immediate.Interned_string.Padded.of_iobuf_peek" [@nontail]
        ;;

        let of_iobuf_consume ~padding ?len buf =
          Iobuf_accessors.For_cinaps.checked_read_with_len
            ?len
            buf
            (fun ~len buf -> unsafe_of_iobuf_consume ~padding ~len buf)
            "Immediate.Interned_string.Padded.of_iobuf_consume" [@nontail]
        ;;

        let to_iobuf_poke t ~padding ?pos ?len buf =
          Iobuf_accessors.For_cinaps.checked_write_with_pos_and_len
            t
            ~length
            ?pos
            ?len
            buf
            (fun t ~pos ~len buf -> unsafe_to_iobuf_poke ~padding t ~pos ~len buf)
            "Immediate.Interned_string.Padded.to_iobuf_poke" [@nontail]
        ;;

        let to_iobuf_fill t ~padding ?len buf =
          Iobuf_accessors.For_cinaps.checked_write_with_len
            t
            ~length
            ?len
            buf
            (fun t ~len buf -> unsafe_to_iobuf_fill ~padding t ~len buf)
            "Immediate.Interned_string.Padded.to_iobuf_fill" [@nontail]
        ;;
      end

      (*$*)
    end
    [@@inline]
  end
end

include Universe.V1.Make ()
open Core

let%test_unit "don't reuse interned buffers" =
  let aa = unsafe_of_bigstring ~len:2 ~pos:0 (Bigstring.of_string "aa") in
  let bb = unsafe_of_bigstring ~len:2 ~pos:0 (Bigstring.of_string "bb") in
  [%test_result: string] (to_string aa) ~expect:"aa";
  [%test_result: string] (to_string bb) ~expect:"bb"
;;

let%test_unit "don't reuse interned buffers peek" =
  let aa = unsafe_of_iobuf_peek ~pos:0 ~len:2 (Iobuf.of_string "aa") in
  let bb = unsafe_of_iobuf_peek ~pos:0 ~len:2 (Iobuf.of_string "bb") in
  [%test_result: string] (to_string aa) ~expect:"aa";
  [%test_result: string] (to_string bb) ~expect:"bb"
;;

let%test_unit "don't reuse interned buffers consume" =
  let aa = unsafe_of_iobuf_consume ~len:2 (Iobuf.of_string "aa") in
  let bb = unsafe_of_iobuf_consume ~len:2 (Iobuf.of_string "bb") in
  [%test_result: string] (to_string aa) ~expect:"aa";
  [%test_result: string] (to_string bb) ~expect:"bb"
;;

let%bench_fun ("unsafe_of_bigstring" [@indexed len = [ 0; 1; 2; 3 ]]) =
  let buf = Bigstring.of_string (String.make len 'A') in
  fun () -> unsafe_of_bigstring ~len ~pos:0 buf |> (ignore : t -> unit)
;;
