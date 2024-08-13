open Core.Core_stable

module Char_option_stable = struct
  module V1 = struct
    type t = int [@@deriving bin_io, compare, hash]

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
    type t = int [@@deriving bin_io, compare, hash, stable_witness]

    let to_wire t = t
    let of_wire t = t
    let none = -1
    let[@zero_alloc] is_none t = t = none
    let[@zero_alloc] is_some t = t <> none
    let[@zero_alloc] some t = Core.Bool.to_int t
    let[@zero_alloc] some_is_representable _ = true
    let[@zero_alloc] unchecked_value t = t <> 0

    let value_exn_not_found =
      Not_found_s [%message "[Immediate.Bool.Option.value_exn]: given [none]"]
    ;;

    let[@zero_alloc] value_exn = function
      | 0 -> false
      | 1 -> true
      | _ -> raise value_exn_not_found
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
    [@@deriving bin_io, compare, equal, globalize, hash, stable_witness, typerep]

    let none = Core.Int.min_value
    let[@zero_alloc] is_none t = t = none
    let[@zero_alloc] is_some t = not (is_none t)

    let[@zero_alloc] some t =
      assert (is_some t);
      t
    ;;

    let[@zero_alloc] some_is_representable t = is_some t
    let[@zero_alloc] unchecked_value t = t
    let to_option t = if is_some t then Some (unchecked_value t) else None

    let[@zero_alloc] of_option = function
      | None -> none
      | Some v -> some v
    ;;

    let sexp_of_t t = to_option t |> [%sexp_of: int option]
    let t_of_sexp s = [%of_sexp: int option] s |> of_option
    let to_int (t : t) : int = t
    let of_int (i : int) : t = i
  end
end

module type Intable_sexpable = sig
  include Core.Intable
  include Core.Sexpable with type t := t

  val bin_shape_uuid : Bin_shape.Uuid.t
end

module Intable_sexpable_option_stable = struct
  module Make (I : Intable_sexpable) = struct
    module V1 = struct
      type t = int [@@deriving bin_io, compare, hash, stable_witness]

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

      let value_exn_not_found =
        Not_found_s
          [%message "[Immediate.Of_intable.Option.Make(_).value_exn]: given [none]"]
      ;;

      let value_exn t = if is_some t then unchecked_value t else raise value_exn_not_found
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
    S_no_option with type t = char)

  module Option = struct
    module Stable = Char_option_stable
    include Stable.V1
    include (Int : Typerep_lib.Typerepable.S with type t := t)

    module Optional_syntax = struct
      module Optional_syntax = struct
        let[@zero_alloc] is_none t = is_none t
        let[@zero_alloc] unsafe_value t = unchecked_value t
      end
    end

    include Identifiable.Make (struct
        include Stable.V1
        include Sexpable.To_stringable (Stable.V1)

        let hash (t : t) = t
        let module_name = "Immediate.Char.Option"
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
    S_no_option with type t = bool)

  module Option = struct
    module Stable = Bool_option_stable
    include Stable.V1
    include (Int : Typerep_lib.Typerepable.S with type t := t)

    module Optional_syntax = struct
      module Optional_syntax = struct
        let[@zero_alloc] is_none t = is_none t
        let[@zero_alloc] unsafe_value t = unchecked_value t
      end
    end

    include Identifiable.Make (struct
        include Stable.V1
        include Sexpable.To_stringable (Stable.V1)

        let hash (t : t) = t
        let module_name = "Immediate.Bool.Option"
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
    S_no_option with type t = int)

  let type_immediacy = Type_immediacy.Always.of_typerep_exn [%here] typerep_of_t

  module Option = struct
    module Stable = Int_option_stable
    include Stable.V1
    include (Int : Typerep_lib.Typerepable.S with type t := t)

    let value_exn_not_found =
      Not_found_s [%message "[Immediate.Int.Option.value_exn]: given [none]"]
    ;;

    let[@zero_alloc] value_exn t = if is_some t then t else raise value_exn_not_found
    let[@zero_alloc] value t ~default = Core.Bool.select (is_none t) default t
    let[@zero_alloc] unchecked_some t = t
    let type_immediacy = Type_immediacy.Always.of_typerep_exn [%here] typerep_of_t

    module Optional_syntax = struct
      module Optional_syntax = struct
        let[@zero_alloc] is_none t = is_none t
        let[@zero_alloc] unsafe_value t = unchecked_value t
      end
    end

    include Identifiable.Make (struct
        include Stable.V1
        include Sexpable.To_stringable (Stable.V1)

        let hash (t : t) = t
        let module_name = "Immediate.Int.Option"
      end)

    (* Export inlineable comparisons (those from the functor confuse the compiler). *)
    include Int.Replace_polymorphic_compare
  end
end

module Of_intable = struct
  module type S = Intable_sexpable

  module Option = struct
    module Make (I : S) = struct
      module Stable = Intable_sexpable_option_stable.Make (I)
      include Stable.V1
      include (Int : Typerep_lib.Typerepable.S with type t := t)

      module Optional_syntax = struct
        module Optional_syntax = struct
          let is_none t = is_none t
          let unsafe_value t = unchecked_value t
        end
      end

      include Identifiable.Make (struct
          include Stable.V1
          include Sexpable.To_stringable (Stable.V1)

          let hash (t : t) = t
          let module_name = "Immediate.Int.Option"
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
