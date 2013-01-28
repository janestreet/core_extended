open Core.Std
open Interfaces

module type Spec_no_binable = sig
  type t

  include Comparable with type t := t
  include Floatable with type t := t
  include Hashable.S_binable with type t := t
  include Sexpable with type t := t
  include Stringable with type t := t
end

module type Spec = sig
  include Spec_no_binable
  include Binable with type t := t
end

module type Verified_spec = sig
  include Spec

  val check : t -> (unit, string) Result.t
end

module type S = sig
  include Spec

  type repr

  val verify : repr -> t
end

module type S0 = sig
  include S

  val zero : t
end

module Make_verified (Spec : Verified_spec) = struct
  include (Spec : Spec_no_binable with type t = Spec.t)

  type repr = Spec.t

  let get_err_str msg n = sprintf "%s: %s" msg (to_string n)

  let verify n =
    match Spec.check n with
    | Ok () -> n
    | Error msg -> failwith (get_err_str msg n)

  let t_of_sexp sexp =
    let n = t_of_sexp sexp in
    match Spec.check n with
    | Ok () -> n
    | Error msg -> Sexplib.Conv.of_sexp_error (get_err_str msg n) sexp

  include Bin_prot.Utils.Make_binable (struct
    module Binable = struct
      type t = Spec.t with bin_io
    end

    let to_binable n = n
    let of_binable = verify

    type t = Spec.t
  end)

  let of_string str = verify (of_string str)
  let of_float n = verify (of_float n)
end

module Make_verified_unsafe = Make_verified

module type Verified_std_spec = sig
  include Spec

  val module_name : string
  val zero : t
end

module type Verified_std = sig
  type repr

  module type S = S with type repr = repr
  module type S0 = S0 with type repr = repr

  module Pos : S with type t = private repr
  module Pos0 : S0 with type t = private repr
  module Neg : S with type t = private repr
  module Neg0 : S0 with type t = private repr

  module type Bounded_spec = sig
    val name : string
    val lower : repr
    val upper : repr
  end

  module type Bounded = sig
    include Bounded_spec
    include S
  end

  module Make_bounded (Spec : Bounded_spec) : Bounded with type t = private repr

  module Pos_unsafe : S with type t = repr
  module Pos0_unsafe : S0 with type t = repr
  module Neg_unsafe : S with type t = repr
  module Neg0_unsafe : S0 with type t = repr

  module Make_bounded_unsafe (Spec : Bounded_spec) : Bounded with type t = repr
end

module Make_verified_std (Spec : Verified_std_spec) = struct
  type repr = Spec.t

  module type S = S with type repr = repr
  module type S0 = S0 with type repr = repr

  module My_verified (Spec : Verified_spec) = struct
    include Make_verified (Spec)

    let verify t =
      match Spec.check t with
      | Ok () -> t
      | Error msg -> failwith msg
  end

  module Pos_spec = struct
    include Spec

    let check =
      let error = Error (module_name ^ ".Pos.t <= 0") in
      fun n -> if n > zero then Ok () else error
  end
  module Pos = Make_verified (Pos_spec)
  module Pos_unsafe = Pos

  module Pos0_spec = struct
    include Spec

    let check =
      let error = Error (module_name ^ ".Pos0.t < 0") in
      fun n -> if n >= zero then Ok () else error
  end
  module Pos0 = struct
    include Make_verified (Pos0_spec)
    let zero = Spec.zero
  end
  module Pos0_unsafe = Pos0

  module Neg_spec = struct
    include Spec

    let check =
      let error = Error (module_name ^ ".Neg.t >= 0") in
      fun n -> if n < zero then Ok () else error
  end
  module Neg = Make_verified (Neg_spec)
  module Neg_unsafe = Neg

  module Neg0_spec = struct
    include Spec

    let check =
      let error = Error (module_name ^ ".Neg0.t > 0") in
      fun n -> if n <= zero then Ok () else error
  end
  module Neg0 = struct
    include Make_verified (Neg0_spec)
    let zero = Spec.zero
  end
  module Neg0_unsafe = Neg0

  module type Bounded_spec = sig
    val name : string
    val lower : repr
    val upper : repr
  end

  module type Bounded = sig
    include Bounded_spec
    include S
  end

  module Make_bounded (Bounded_spec : Bounded_spec) = struct
    include Bounded_spec

    module Spec = struct
      include Spec

      let check =
        if lower > upper then
          failwithf "%s.Make_bounded: %s: lower(%s) > upper(%s)"
            module_name name (to_string lower) (to_string upper) ()
        else
          let mk_error cmp_c bound =
            Error (sprintf "%s.t %c %s" name cmp_c (to_string bound))
          in
          let lower_error = mk_error '<' lower in
          let upper_error = mk_error '>' upper in
          fun n ->
            if n < lower then lower_error
            else if n > upper then upper_error
            else Ok ()
    end
    include Make_verified (Spec)
  end
  module Make_bounded_unsafe = Make_bounded
end
