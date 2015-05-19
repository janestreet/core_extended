open Core_kernel.Std

open Generator.Infix

include Janecheck_kernel.Std.Observer

type 'a gen = 'a Janecheck_kernel.Std.Generator.t

let either a b =
  unmap (variant a b)
    ~f:(function
      | First  a -> `A a
      | Second b -> `B b)
    ~f_sexp:(Sexp.Atom "variant_of_either")

let of_predicate a b ~f ~f_sexp =
  unmap (variant a b) ~f:(fun x ->
    if f x then `A x else `B x)
    ~f_sexp:(<:sexp_of< [`variant_by_predicate of Sexp.t] >>
               (`variant_by_predicate f_sexp))

let doubleton f ~f_sexp =
  of_predicate (singleton ()) (singleton ()) ~f ~f_sexp

let unit = singleton ()
let bool = doubleton Fn.id ~f_sexp:(Sexp.Atom "id")

type 'a bound = [ `Inclusive of 'a | `Exclusive of 'a | `Unbounded ]

module Make_int_observer (M : Int_intf.S) : sig
  val t_obs : M.t t
  val t_obs_between : lower_bound:M.t bound -> upper_bound:M.t bound -> M.t t
end = struct

  open M

  let bit n =
    unmap bool ~f:(fun t ->
      bit_and t (shift_left one n) = zero)
      ~f_sexp:(<:sexp_of< [`bit_n_is_zero of int] >> (`bit_n_is_zero n))

  let sign_bit =
    unmap bool ~f:(fun t -> t < zero) ~f_sexp:(Sexp.Atom "is_negative")

  let non_negative_bits_between_inclusive ~lower_bound ~upper_bound =
    assert (lower_bound >= zero);
    assert (upper_bound >= zero);
    let rec loop ~index ~lower_bound ~upper_bound ~rev_bits =
      if lower_bound >= upper_bound
      then List.rev rev_bits
      else
        loop
          ~index:(Int.succ index)
          ~lower_bound:(shift_right_logical lower_bound 1)
          ~upper_bound:(shift_right_logical upper_bound 1)
          ~rev_bits:(bit index :: rev_bits)
    in
    loop ~index:0 ~lower_bound ~upper_bound ~rev_bits:[]

  let strip_sign_bit t =
    if t < zero then bit_xor t min_value else t

  TEST_UNIT =
    assert (strip_sign_bit zero = zero);
    assert (strip_sign_bit one = one);
    assert (strip_sign_bit (- one) = max_value);
    assert (strip_sign_bit max_value = max_value);
    assert (strip_sign_bit min_value = zero)

  let negative_bits_between_inclusive ~lower_bound ~upper_bound =
    assert (lower_bound < zero);
    assert (upper_bound < zero);
    List.map ~f:(fun t -> unmap t ~f:strip_sign_bit ~f_sexp:(Sexp.Atom "strip_sign_bit"))
      (non_negative_bits_between_inclusive
         ~lower_bound:(strip_sign_bit lower_bound)
         ~upper_bound:(strip_sign_bit upper_bound))

  let unsigned_bits_between_inclusive ~lower_bound ~upper_bound =
    assert (lower_bound < zero);
    assert (upper_bound >= zero);
    List.map
      (non_negative_bits_between_inclusive
         ~lower_bound:zero
         ~upper_bound:(max (strip_sign_bit lower_bound) upper_bound))
      ~f:(fun t ->
        unmap t
          ~f:strip_sign_bit
          ~f_sexp:(Sexp.Atom "strip_sign_bit"))

  let signed_bits_between_inclusive ~lower_bound ~upper_bound =
    sign_bit :: unsigned_bits_between_inclusive ~lower_bound ~upper_bound

  let bits_between_inclusive ~lower_bound ~upper_bound =
    if lower_bound >= zero
    then non_negative_bits_between_inclusive ~lower_bound ~upper_bound
    else if upper_bound < zero
    then negative_bits_between_inclusive ~lower_bound ~upper_bound
    else signed_bits_between_inclusive ~lower_bound ~upper_bound

  let subset_weighted_toward_ends bits =
    let n = List.length bits in
    Generator.list ~unique:true ~sorted:`Arbitrarily
      (Generator.int_between
         ~lower_bound:(`Inclusive 0)
         ~upper_bound:(`Exclusive n))
    >>| fun indices ->
    List.map indices ~f:(fun index ->
      List.nth_exn bits index)

  let t_obs_of_bits bits =
    subset_weighted_toward_ends bits
    >>= fun bits ->
    let rec loop bits =
      match bits with
      | []          -> singleton ()
      | [ bit ]     -> bit
      | bit :: bits ->
        unmap (tuple bit (loop bits))
          ~f:(fun t -> (t, t))
          ~f_sexp:(Sexp.Atom "tuple_of_self_and_self")
    in
    loop bits

  let t_obs_between_inclusive ~lower_bound ~upper_bound =
    let bits = bits_between_inclusive ~lower_bound ~upper_bound in
    t_obs_of_bits bits

  let t_obs_between ~lower_bound ~upper_bound =
    match lower_bound, upper_bound with
    | `Exclusive lower, _ when lower = max_value -> singleton ()
    | _, `Exclusive upper when upper = min_value -> singleton ()
    | _ ->
      let lower_bound =
        match lower_bound with
        | `Unbounded             -> min_value
        | `Inclusive lower_bound -> lower_bound
        | `Exclusive lower_bound -> lower_bound + one
      in
      let upper_bound =
        match upper_bound with
        | `Unbounded             -> max_value
        | `Inclusive upper_bound -> upper_bound
        | `Exclusive upper_bound -> upper_bound - one
      in
      t_obs_between_inclusive ~lower_bound ~upper_bound

  let t_obs =
    t_obs_between
      ~lower_bound:`Unbounded
      ~upper_bound:`Unbounded

end

module Int_obs = Make_int_observer (Int)
let int_between = Int_obs.t_obs_between
let int         = Int_obs.t_obs

module Int63_obs = Make_int_observer (Int63)
let int63_between = Int63_obs.t_obs_between

let enum n ~f ~f_sexp =
  let index =
    int_between
      ~lower_bound:(`Inclusive 0)
      ~upper_bound:(`Exclusive n)
  in
  unmap index ~f ~f_sexp

let findi_exn x list ~equal =
  fst (Option.value_exn (List.findi list ~f:(fun _ y -> equal x y)))

let of_list list ~equal ~sexp_of_elt =
  let f x = findi_exn x list ~equal in
  enum (List.length list) ~f
    ~f_sexp:(<:sexp_of< [`find_index_in_list of elt list] >>
              (`find_index_in_list list))

let recursive f =
  let rec self () = Generator.union [ singleton () ; f (of_fun self) ] in
  of_fun self

let option t =
  unmap (variant (singleton ()) t)
    ~f:(function
      | None   -> `A ()
      | Some x -> `B x)
    ~f_sexp:(Sexp.Atom "variant_of_option")

let list t = recursive (fun list_t ->
  unmap (variant (singleton ()) (tuple t list_t))
    ~f:(function
      | []        -> `A ()
      | x :: list -> `B (x, list))
    ~f_sexp:(Sexp.Atom "variant_of_list"))

let char = enum 256 ~f:Char.to_int ~f_sexp:(Sexp.Atom "Char.to_int")

let string = unmap (list char) ~f:String.to_list ~f_sexp:(Sexp.Atom "String.to_list")

let sexp = recursive (fun sexp_t ->
  unmap (variant string (list sexp_t))
    ~f:(function
      | Sexp.Atom atom -> `A atom
      | Sexp.List list -> `B list)
    ~f_sexp:(Sexp.Atom "variant_of_sexp"))

let variant3 a b c =
  unmap (variant a (variant b c)) ~f:(function
    | `A x -> `A x
    | `B x -> `B (`A x)
    | `C x -> `B (`B x))
    ~f_sexp:(Sexp.Atom "variant_of_variant3")

let variant4 a b c d =
  unmap (variant (variant a b) (variant c d)) ~f:(function
    | `A x -> `A (`A x)
    | `B x -> `A (`B x)
    | `C x -> `B (`A x)
    | `D x -> `B (`B x))
    ~f_sexp:(Sexp.Atom "variant_of_variant4")

let variant5 a b c d e =
  unmap (variant (variant a b) (variant c (variant d e))) ~f:(function
    | `A x -> `A (`A x)
    | `B x -> `A (`B x)
    | `C x -> `B (`A x)
    | `D x -> `B (`B (`A x))
    | `E x -> `B (`B (`B x)))
    ~f_sexp:(Sexp.Atom "variant_of_variant5")

let variant6 a b c d e f =
  unmap (variant (variant a b) (variant (variant c d) (variant e f))) ~f:(function
    | `A x -> `A (`A x)
    | `B x -> `A (`B x)
    | `C x -> `B (`A (`A x))
    | `D x -> `B (`A (`B x))
    | `E x -> `B (`B (`A x))
    | `F x -> `B (`B (`B x)))
    ~f_sexp:(Sexp.Atom "variant_of_variant6")

let tuple3 a b c =
  unmap (tuple a (tuple b c))
    ~f:(fun (a,b,c) -> (a,(b,c)))
    ~f_sexp:(Sexp.Atom "tuple_of_tuple3")

let tuple4 a b c d =
  unmap (tuple (tuple a b) (tuple c d))
    ~f:(function (a,b,c,d) -> ((a,b),(c,d)))
    ~f_sexp:(Sexp.Atom "tuple_of_tuple4")

let tuple5 a b c d e =
  unmap (tuple (tuple a b) (tuple c (tuple d e)))
    ~f:(function (a,b,c,d,e) -> ((a,b),(c,(d,e))))
    ~f_sexp:(Sexp.Atom "tuple_of_tuple5")

let tuple6 a b c d e f =
  unmap (tuple (tuple a b) (tuple (tuple c d) (tuple e f)))
    ~f:(function (a,b,c,d,e,f) -> ((a,b),((c,d),(e,f))))
    ~f_sexp:(Sexp.Atom "tuple_of_tuple6")

let comparison ~compare ~eq ~lt ~gt ~compare_sexp ~sexp_of_eq =
  unmap (variant3 lt (singleton ()) gt) ~f:(fun x ->
    let c = compare x eq in
    if c < 0 then `A x else
    if c > 0 then `C x else
      `B x)
    ~f_sexp:(<:sexp_of< [`variant3_by_comparison_to of (eq * Sexp.t)] >>
              (`variant3_by_comparison_to (eq, compare_sexp)))

let min_pos_normal = Float.min_positive_normal_value
let max_pos_normal = Float.max_finite_value

let min_normal_exponent = Float.ieee_exponent min_pos_normal
let max_normal_exponent = Float.ieee_exponent max_pos_normal

let min_normal_mantissa = Float.ieee_mantissa min_pos_normal
let max_normal_mantissa = Float.ieee_mantissa max_pos_normal

let min_pos_subnormal = Float.min_positive_subnormal_value
let max_pos_subnormal = Float.one_ulp `Down min_pos_normal

let min_subnormal_mantissa = Float.ieee_mantissa min_pos_subnormal
let max_subnormal_mantissa = Float.ieee_mantissa max_pos_subnormal

(* {zero,subnormal,normal,infinite,nan}_float are observers that distinguish
   floats already known to classify as their names suggest. E.g., [zero_float]
   only distinguishes between positive and negative zero.  *)

let zero_float =
  unmap bool ~f:Float.ieee_negative ~f_sexp:(Sexp.Atom "Float.ieee_negative")

let subnormal_float =
  let mantissa =
    int63_between
      ~lower_bound:(`Inclusive min_subnormal_mantissa)
      ~upper_bound:(`Inclusive max_subnormal_mantissa)
  in
  unmap (tuple bool mantissa)
    ~f:(fun float ->
      Float.ieee_negative float,
      Float.ieee_mantissa float)
    ~f_sexp:(Sexp.Atom "ieee_negative_and_mantissa")

let normal_float =
  let exponent =
    int_between
      ~lower_bound:(`Inclusive min_normal_exponent)
      ~upper_bound:(`Inclusive max_normal_exponent)
  in
  let mantissa =
    int63_between
      ~lower_bound:(`Inclusive min_normal_mantissa)
      ~upper_bound:(`Inclusive max_normal_mantissa)
  in
  unmap (tuple3 bool exponent mantissa)
    ~f:(fun float ->
      Float.ieee_negative float,
      Float.ieee_exponent float,
      Float.ieee_mantissa float)
    ~f_sexp:(Sexp.Atom "ieee_negative_and_exponent_and_mantissa")

let infinite_float =
  unmap bool ~f:Float.ieee_negative ~f_sexp:(Sexp.Atom "Float.ieee_negative")

let nan_float = singleton ()

let float =
  unmap (variant5 zero_float subnormal_float normal_float infinite_float nan_float)
    ~f:(fun float ->
      match Float.classify float with
      | Zero      -> `A float
      | Subnormal -> `B float
      | Normal    -> `C float
      | Infinite  -> `D float
      | Nan       -> `E float)
    ~f_sexp:(Sexp.Atom "variant5_of_float_by_classification")

let fn dom_gen rng_t ~sexp_of_dom =
  Generator.list dom_gen ~unique:true ~sorted:`Arbitrarily
  >>= fun inputs ->
  let apply_to_input input =
    unmap rng_t ~f:(fun f -> f input)
      ~f_sexp:(<:sexp_of< [`apply_to of dom] >> (`apply_to input))
  in
  let rec loop inputs =
    match inputs with
    | [] -> singleton ()
    | [ input ] -> apply_to_input input
    | input :: inputs ->
      unmap (tuple (apply_to_input input) (loop inputs))
        ~f:(fun f -> (f, f))
        ~f_sexp:(Sexp.Atom "tuple_of_self_and_self")
  in
  loop inputs
