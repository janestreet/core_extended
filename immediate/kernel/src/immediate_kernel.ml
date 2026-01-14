open Core.Core_stable

module Char_option_stable = struct
  module V1 = struct
    let typerep_of_int = Core.Typerep.Int

    type t = int
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , stable_witness
      , typerep]

    let[@zero_alloc] is_none t = Core.Int.(t < 0 || t > 255)
    let[@zero_alloc] is_some t = not (is_none t)
    let none = -1
    let[@zero_alloc] some t = Core.Char.to_int t
    let[@zero_alloc] some_is_representable _ = true
    let[@zero_alloc] unchecked_value t = Core.Char.unsafe_of_int t
    let[@zero_alloc] value_exn t = Core.Char.of_int_exn t

    let[@zero_alloc] value t ~default =
      Core.Bool.select (is_none t) default (unchecked_value t)
    ;;

    let to_option t = if is_some t then Some (unchecked_value t) else None

    let[@zero_alloc] of_option = function
      | None -> none
      | Some v -> some v
    ;;

    let sexp_of_t t = to_option t |> [%sexp_of: char option]
    let t_of_sexp s = [%of_sexp: char option] s |> of_option
  end
end

module Bool_option_stable = struct
  module V1 = struct
    let typerep_of_int = Core.Typerep.Int

    type t = int
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , stable_witness
      , typerep]

    let to_wire t = t
    let of_wire t = t
    let none = -1
    let[@zero_alloc] is_none t = t = none
    let[@zero_alloc] is_some t = t <> none
    let[@zero_alloc] some t = Core.Bool.to_int t
    let[@zero_alloc] some_is_representable _ = true
    let[@zero_alloc] unchecked_value t = t <> 0

    let value_exn_not_found () =
      Not_found_s (Atom "[Immediate.Bool.Option.value_exn]: given [none]")
    ;;

    let[@zero_alloc] value_exn = function
      | 0 -> false
      | 1 -> true
      | _ -> raise (value_exn_not_found ())
    ;;

    let[@zero_alloc] value t ~default =
      Core.Bool.select (is_none t) default (unchecked_value t)
    ;;

    let to_option t = if is_some t then Some (unchecked_value t) else None

    let[@zero_alloc] of_option = function
      | None -> none
      | Some v -> some v
    ;;

    let sexp_of_t t = to_option t |> [%sexp_of: bool option]
    let t_of_sexp s = [%of_sexp: bool option] s |> of_option
  end
end

module Int_option_stable = struct
  module V1 = struct
    let typerep_of_int = Core.Typerep.Int

    type t = int
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , stable_witness
      , typerep]

    let none = Core.Int.min_value
    let[@zero_alloc] is_none t = t = none
    let[@zero_alloc] is_some t = not (is_none t)

    (* We have this accept an argument of type [t] instead of [unit] because OCaml
       functions must accept at least one argument and sadly [unit] is a *concrete* part
       of the calling convention which forces the caller to actually [mov eax, 1] before
       the call. Instead by accepting [t] as an argument, the caller will emit something
       like [mov rax, rdi] (if [rdi] was the register that held the [t] we were
       inspecting), which is a 3-byte instruction as opposed the [mov] of the immediate
       above which is a 5-byte instruction. *)
    let[@cold] raise_illegal_some (_ : t) =
      failwith "Attempted to construct an illegal [Immediate_kernel.Int.Option.t]"
    ;;

    let[@zero_alloc] some t =
      (* The branch-ordering here is important to get optimal codegen. By writing
         [if is_some t then t] instead of [if is_none t then raise] the happy-path falls
         through in the generated assembly, instead of having to [jmp] over the
         exception-raising code. *)
      if is_some t then t else raise_illegal_some t
    ;;

    let[@zero_alloc] some_is_representable t = is_some t
    let[@zero_alloc] unchecked_value t = t
    let to_option t = if is_some t then Some (unchecked_value t) else None

    let to_or_null t : _ Core.Or_null.t =
      if is_some t then This (unchecked_value t) else Null
    ;;

    let[@zero_alloc] of_option = function
      | None -> none
      | Some v -> some v
    ;;

    let%template[@alloc a = (heap, stack)] sexp_of_t t =
      let o = to_or_null t in
      [%sexp (o : int Base.Or_null.t)] [@alloc a] [@exclave_if_stack a]
    ;;

    let t_of_sexp s = [%of_sexp: int option] s |> of_option
    let[@zero_alloc] to_int (t : t) : int = t
    let[@zero_alloc] of_int (i : int) : t = i
  end
end

module type Intable_sexpable = sig
  include Core.Intable
  include Core.Sexpable with type t := t

  val bin_shape_uuid : Bin_shape.Uuid.t
end

module Intable_sexpable_option_stable = struct
  module%template.portable Make (I : Intable_sexpable) = struct
    module V1 = struct
      type t = int
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , stable_witness]

      let bin_shape_t = Bin_shape.annotate I.bin_shape_uuid bin_shape_t
      let none = Int_option_stable.V1.none
      let is_none t = t = none
      let is_some t = not (is_none t)

      let some i =
        let t = I.to_int_exn i in
        assert (is_some t);
        t
      ;;

      let some_is_representable i = I.to_int_exn i |> is_some
      let unchecked_value t = I.of_int_exn t
      let to_option t = if is_some t then Some (unchecked_value t) else None

      let of_option = function
        | None -> none
        | Some v -> some v
      ;;

      let to_int (t : t) : int = t
      let of_int (i : int) : t = i

      let value_exn_not_found () =
        Not_found_s (Atom "[Immediate.Of_intable.Option.Make(_).value_exn]: given [none]")
      ;;

      let value_exn t =
        if is_some t then unchecked_value t else raise (value_exn_not_found ())
      ;;

      let value t ~default = if is_some t then unchecked_value t else default
      let sexp_of_t t = to_option t |> [%sexp_of: I.t option]
      let t_of_sexp s = [%of_sexp: I.t option] s |> of_option
    end
  end
end

open Core
include Immediate_kernel_intf

module Char = struct
  include (
  struct
    include Char

    let globalize = globalize_char
  end :
  sig
  @@ portable
    include S_no_option with type t = char
  end)

  module Option = struct
    module Stable = Char_option_stable
    include Stable.V1

    module Optional_syntax = struct
      module Optional_syntax = struct
        let[@zero_alloc] is_none t = is_none t
        let[@zero_alloc] unsafe_value t = unchecked_value t
      end
    end

    include%template Identifiable.Make [@modality portable] (struct
        include Stable.V1

        include%template Sexpable.To_stringable [@modality portable] (Stable.V1)

        let hash (t : t) = t
        let module_name = "Immediate_kernel.Char.Option"
      end)

    (* Export inlineable comparisons (those from the functor confuse the compiler). *)
    include Int.Replace_polymorphic_compare
  end
end

module Bool = struct
  include (
  struct
    include Bool

    let globalize = globalize_bool
  end :
  sig
  @@ portable
    include S_no_option with type t = bool
  end)

  module Option = struct
    module Stable = Bool_option_stable
    include Stable.V1

    include%template
      Quickcheckable.Of_quickcheckable [@modality portable]
        (struct
          type t = bool option [@@deriving quickcheck ~portable]
        end)
        (struct
          type nonrec t = t

          let of_quickcheckable = of_option
          let to_quickcheckable = to_option
        end)

    module Optional_syntax = struct
      module Optional_syntax = struct
        let[@zero_alloc] is_none t = is_none t
        let[@zero_alloc] unsafe_value t = unchecked_value t
      end
    end

    include%template Identifiable.Make [@modality portable] (struct
        include Stable.V1

        include%template Sexpable.To_stringable [@modality portable] (Stable.V1)

        let hash (t : t) = t
        let module_name = "Immediate_kernel.Bool.Option"
      end)

    (* Export inlineable comparisons (those from the functor confuse the compiler). *)
    include Int.Replace_polymorphic_compare
  end
end

module Int = struct
  include (
  struct
    include Int

    let globalize = globalize_int
  end :
  sig
  @@ portable
    include S_no_option with type t = int
  end)

  let type_immediacy = Type_immediacy.Always.of_typerep_exn typerep_of_t

  module Option = struct
    module Stable = Int_option_stable
    include Stable.V1

    include%template
      Quickcheckable.Of_quickcheckable_filtered [@modality portable]
        (struct
          type t = int option [@@deriving quickcheck ~portable]
        end)
        (struct
          type nonrec t = t

          let of_quickcheckable int_option =
            match int_option with
            | None -> Some none
            | Some i -> if some_is_representable i then Some (some i) else None
          ;;

          let to_quickcheckable = to_option
        end)

    let value_exn_not_found () =
      Not_found_s (Atom "[Immediate.Int.Option.value_exn]: given [none]")
    ;;

    let[@zero_alloc] value_exn t = if is_some t then t else raise (value_exn_not_found ())
    let[@zero_alloc] value t ~default = Core.Bool.select (is_none t) default t

    let[@zero_alloc] of_or_null = function
      | Null -> none
      | This v -> some v
    ;;

    let[@zero_alloc] to_or_null t = Or_null.this_if (is_some t) t
    let[@zero_alloc] unchecked_some t = t
    let type_immediacy = Type_immediacy.Always.of_typerep_exn typerep_of_t

    module Optional_syntax = struct
      module Optional_syntax = struct
        let[@zero_alloc] is_none t = is_none t
        let[@zero_alloc] unsafe_value t = unchecked_value t
      end
    end

    include%template Identifiable.Make [@modality portable] (struct
        include Stable.V1

        include%template Sexpable.To_stringable [@modality portable] (Stable.V1)

        let hash (t : t) = t
        let module_name = "Immediate_kernel.Int.Option"
      end)

    (* Export inlineable comparisons (those from the functor confuse the compiler). *)
    include Int.Replace_polymorphic_compare
  end
end

module Of_intable = struct
  module type S = Intable_sexpable

  module Option = struct
    module%template.portable [@modality m] Make (I : S) = struct
      module Stable = Intable_sexpable_option_stable.Make [@modality m] (I)
      include Stable.V1

      let typerep_of_t = [%typerep_of: int]

      let typename_of_t =
        Typerep_lib.Typename.create ~name:(Bin_shape.Uuid.to_string I.bin_shape_uuid) ()
      ;;

      module Optional_syntax = struct
        module Optional_syntax = struct
          let is_none t = is_none t
          let unsafe_value t = unchecked_value t
        end
      end

      include%template Identifiable.Make [@modality m] (struct
          include Stable.V1

          include%template Sexpable.To_stringable [@modality m] (Stable.V1)

          let hash (t : t) = t
          let module_name = "Immediate_kernel.Int.Option"
        end)

      (* Export inlineable comparisons (those from the functor confuse the compiler). *)
      include Int.Replace_polymorphic_compare
    end
  end
end

module Immediate_kernel_stable = struct
  module Char = struct
    module Option = Char.Option.Stable
  end

  module Bool = struct
    module Option = Bool.Option.Stable
  end

  module Int = struct
    module Option = Int.Option.Stable
  end
end
