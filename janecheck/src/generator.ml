open Core_kernel.Std

include Janecheck_kernel.Std.Generator
include Monad.Make (struct
    include Janecheck_kernel.Std.Generator
    let return = singleton
    let map = `Define_using_bind
  end)

type 'a obs = 'a Janecheck_kernel.Std.Observer.t

type 'a bound = [ `Inclusive of 'a | `Exclusive of 'a | `Unbounded ]

module Infix = struct

  include Monad_infix

  let (>>=/<)  t f = bind_choice t (fun c -> f (Choice.value c) (Choice.lt c))
  let (>>=/<=) t f = bind_choice t (fun c -> f (Choice.value c) (Choice.le c))
  let (>>=/<>) t f = bind_choice t (fun c -> f (Choice.value c) (Choice.ne c))
  let (>>=/>=) t f = bind_choice t (fun c -> f (Choice.value c) (Choice.ge c))
  let (>>=/>)  t f = bind_choice t (fun c -> f (Choice.value c) (Choice.gt c))

  let (>>=/<=>) t f = bind t (fun x -> f x t)

end

include Infix

let filter_map t ~f =
  t >>= fun x ->
  match f x with
  | Some x -> return x
  | None   -> empty

let filter t ~f =
  t >>= fun x ->
  if f x
  then return x
  else empty

let union ts =
  weighted_union (List.map ts ~f:(fun t -> (1., t)))

let singleton = return

let doubleton x y = union [ singleton x ; singleton y ]

let partitions_of_list list ~max_partition_size =
  List.groupi list ~break:(fun i _ _ -> i mod max_partition_size = 0)

TEST_UNIT =
  for len = 0 to 50 do
    let list = List.init len ~f:Fn.id in
    for max_partition_size = 1 to len do
      let partitions = partitions_of_list list ~max_partition_size in
      <:test_result< int >>
        ~message:(sprintf "len = %d, max_partition_size = %d" len max_partition_size)
        (List.fold partitions ~init:0 ~f:(fun max_len_so_far partition ->
           max max_len_so_far (List.length partition)))
        ~expect:max_partition_size
    done
  done

TEST_UNIT =
  for len = 0 to 50 do
    let list = List.init len ~f:Fn.id in
    for max_partition_size = 1 to len do
      let partitions = partitions_of_list list ~max_partition_size in
      <:test_result< int list >>
        ~message:(sprintf "len = %d, max_partition_size = %d" len max_partition_size)
        (List.concat partitions)
        ~expect:list
    done
  done

let rec of_list list =
  let branching_factor = 16 in
  let len = List.length list in
  if len <= branching_factor
  then union (List.map list ~f:singleton)
  else
    let max_partition_size =
      Int.round_up len ~to_multiple_of:branching_factor
      / branching_factor
    in
    partitions_of_list list ~max_partition_size
    |> List.map ~f:(fun list -> (Float.of_int (List.length list), of_list list))
    |> weighted_union

let of_sequence seq =
  Sequence.delayed_fold seq
    ~init:()
    ~finish:(fun () -> empty)
    ~f:(fun () x ~k ->
      union [ singleton x ; of_fun k ])

module Make_int_generator (M : Int_intf.S) : sig
  val t_gen : M.t t
  val t_gen_between : lower_bound:M.t bound -> upper_bound:M.t bound -> M.t t
end = struct

  open M

  let average_rounded_down x y =
    (shift_right x 1) + (shift_right y 1) + (bit_and (bit_and x y) one)

  TEST_UNIT "average_rounded_down" =
    let check here x y ~expect =
      let actual = average_rounded_down x y in
      <:test_result<M.t>> ~here:[here] actual ~expect
    in
    let check_int here x y =
      check here (of_int_exn x) (of_int_exn y)
        ~expect:(of_int_exn Int.((x + y) / 2))
    in
    check_int _here_ 3 5;
    check_int _here_ 4 6;
    check_int _here_ 4 5;
    check _here_ min_value max_value ~expect:(- one);
    check _here_ max_value min_value ~expect:(- one);
    check _here_ max_value max_value ~expect:max_value;
    check _here_ min_value min_value ~expect:min_value

  let rec lower_ranges ~lower_bound ~upper_bound =
    if lower_bound = upper_bound
    then [ (lower_bound, upper_bound) ]
    else
      let lower_middle = average_rounded_down lower_bound upper_bound in
      let upper_middle = succ lower_middle in
      (upper_middle, upper_bound) :: lower_ranges ~lower_bound ~upper_bound:lower_middle

  let upper_ranges ~lower_bound ~upper_bound =
    let flip x = upper_bound - x + lower_bound in
    List.map (lower_ranges ~lower_bound ~upper_bound) ~f:(fun (lower, upper) ->
      (flip upper, flip lower))

  let non_negative_ranges_by_magnitude ~lower_bound ~upper_bound =
    if lower_bound = upper_bound
    then [ (lower_bound, upper_bound) ]
    else
      let lower_middle = average_rounded_down lower_bound upper_bound in
      let upper_middle = succ lower_middle in
      lower_ranges ~lower_bound ~upper_bound:lower_middle @
      upper_ranges ~lower_bound:upper_middle ~upper_bound

  let negative_ranges_by_magnitude ~lower_bound ~upper_bound =
    List.map ~f:(fun (lower, upper) -> (bit_not upper, bit_not lower))
      (non_negative_ranges_by_magnitude
         ~lower_bound:(bit_not upper_bound)
         ~upper_bound:(bit_not lower_bound))

  let ranges_by_magnitude_and_sign ~lower_bound ~upper_bound =
    if lower_bound >= zero
    then non_negative_ranges_by_magnitude ~lower_bound ~upper_bound
    else if upper_bound < zero
    then negative_ranges_by_magnitude ~lower_bound ~upper_bound
    else
      negative_ranges_by_magnitude ~lower_bound ~upper_bound:(- one) @
      non_negative_ranges_by_magnitude ~lower_bound:zero ~upper_bound

  TEST_UNIT "ranges_by_magnitude_and_sign exhaustive" =
    let low = (-100) in
    let high = 100 in
    let lower_bound = of_int_exn low and upper_bound = of_int_exn high in
    let ranges =
      ranges_by_magnitude_and_sign ~lower_bound ~upper_bound
      |> List.map ~f:(fun (a, b) -> to_int_exn a, to_int_exn b)
    in
    let mem n =
      List.exists ranges ~f:(fun (lower, upper) ->
        Int.(<=) lower n && Int.(<=) n upper)
    in
    assert (not (mem (Int.pred low)));
    assert (not (mem (Int.succ high)));
    for i = low to high do assert (mem i) done;
  ;;

  TEST_UNIT "ranges_by_magnitude_and_sign num_ranges grows slowly" =
    for i = 0 to 32 do
      let n = Int.pow 2 i in
      let num_ranges =
        ranges_by_magnitude_and_sign ~lower_bound:zero ~upper_bound:(of_int_exn n)
        |> List.length
      in
      assert (Int.(num_ranges <= 2 * (i + 1)))
    done
  ;;

  let rec weighted_uniform ~lower_bound ~upper_bound =
    if lower_bound = upper_bound
    then 1., singleton lower_bound
    else
      ( to_float (succ (upper_bound - lower_bound))
      , of_fun (fun () ->
          let lower_middle = average_rounded_down lower_bound upper_bound in
          let upper_middle = succ lower_middle in
          weighted_union
            [ weighted_uniform ~lower_bound ~upper_bound:lower_middle
            ; weighted_uniform ~lower_bound:upper_middle ~upper_bound
            ])
      )

  let t_gen_uniform ~lower_bound ~upper_bound =
    snd (weighted_uniform ~lower_bound ~upper_bound)

  let count_bits t =
    assert (t >= zero);
    let t = ref t in
    let c = ref 0 in
    while !t <> zero do
      Int.incr c;
      t := shift_right_logical !t 1;
    done;
    !c

  TEST_MODULE "count_bits" = struct
    let check x num_bits =
      <:test_result<int>>
        (count_bits (of_int_exn x))
        ~expect:num_bits
        ~message:(sprintf "bits of %s" (Int.to_string_hum x))

    TEST_UNIT = check 4 3;
    TEST_UNIT = check 5 3;
    TEST_UNIT = check 8 4;
    TEST_UNIT "log based" =
      let lg x = log x /. log 2. in
      let expect n = Int.succ (Float.iround_exn ~dir:`Down (lg (Float.of_int n))) in
      for i = 1 to 10_000 do
        check i (expect i)
      done
    ;;
  end

  let t_gen_between_inclusive ~lower_bound ~upper_bound =
    if lower_bound > upper_bound
    then empty
    else
      (* [ranges] is a list of tuples representing inclusive lower and upper bounds of
         disjoint ranges that add up to the entirety of [lower_bound, upper_bound].  These
         ranges are constructed to start at size 1 at the boundaries and approximately
         double in size as they approach the middle of the range.  Each range is converted
         into a uniform distribution of values.

         The final generator is constructed as a union of these ranges, weighted in
         inverse proportion to the log of their sizes.  The intention is to consistently
         exercise boundary conditions, while still leaving a fair probability of choosing
         arbitrary values out of the middle of the distribution. *)
      let ranges = ranges_by_magnitude_and_sign ~lower_bound ~upper_bound in
      weighted_union (List.map ranges ~f:(fun (lower, upper) ->
        1. /. Float.of_int (count_bits (succ (abs (lower - upper)))),
        t_gen_uniform ~lower_bound:lower ~upper_bound:upper))

  let t_gen_between ~lower_bound ~upper_bound =
    match lower_bound, upper_bound with
    | `Exclusive lower, _ when lower = max_value -> empty
    | _, `Exclusive upper when upper = min_value -> empty
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
      t_gen_between_inclusive ~lower_bound ~upper_bound

  let t_gen = t_gen_between ~lower_bound:`Unbounded ~upper_bound:`Unbounded

end

module Int_gen = Make_int_generator (Int)
let int_between = Int_gen.t_gen_between
let int         = Int_gen.t_gen

module Int63_gen = Make_int_generator (Int63)
let int63_between = Int63_gen.t_gen_between

let recursive f =
  let rec self () = f (of_fun self) in
  of_fun self

let either a b =
  union [ map a ~f:(fun a -> First  a)
        ; map b ~f:(fun b -> Second b)
        ]

let variant a b =
  union [ map a ~f:(fun a -> `A a)
        ; map b ~f:(fun b -> `B b)
        ]

let variant3 a b c =
  union [ map a ~f:(fun a -> `A a)
        ; map b ~f:(fun b -> `B b)
        ; map c ~f:(fun c -> `C c)
        ]

let variant4 a b c d =
  union [ map a ~f:(fun a -> `A a)
        ; map b ~f:(fun b -> `B b)
        ; map c ~f:(fun c -> `C c)
        ; map d ~f:(fun d -> `D d)
        ]

let variant5 a b c d e =
  union [ map a ~f:(fun a -> `A a)
        ; map b ~f:(fun b -> `B b)
        ; map c ~f:(fun c -> `C c)
        ; map d ~f:(fun d -> `D d)
        ; map e ~f:(fun e -> `E e)
        ]

let variant6 a b c d e f =
  union [ map a ~f:(fun a -> `A a)
        ; map b ~f:(fun b -> `B b)
        ; map c ~f:(fun c -> `C c)
        ; map d ~f:(fun d -> `D d)
        ; map e ~f:(fun e -> `E e)
        ; map f ~f:(fun f -> `F f)
        ]

let unit = singleton ()
let bool = doubleton true false

let size = of_sequence (Sequence.unfold ~init:0 ~f:(fun n -> Some (n, n+1)))

let gen_list elem_t ~lo ~hi bind_elem =
  let rec loop elem_t len tail =
    if len < lo
    then
      bind_elem elem_t (fun elem elem_t ->
        loop elem_t (len+1) (elem::tail))
    else if len > hi
    then empty
    else if len = hi
    then singleton tail
    else
      union
        [ singleton tail
        ; bind_elem elem_t (fun elem elem_t ->
            loop elem_t (len+1) (elem::tail))
        ]
  in
  loop elem_t 0 []

let sort by t =
  match by with
  | `Arbitrarily -> t
  | `By cmp -> map t ~f:(List.sort ~cmp)

let list ?(length = `At_least 0) ?(unique = false) ?sorted t =
  let lo, hi =
    match length with
    | `Exactly           n      -> n, n
    | `At_least          n      -> n, Int.max_value
    | `At_most           n      -> 0, n
    | `Between_inclusive (x, y) -> x, y
  in
  if lo < 0 || lo > hi then failwith "Generator.list: invalid length argument";
  match unique, sorted with
  | false, None    -> gen_list t ~lo ~hi (>>=/<=>)
  | true,  None    -> gen_list t ~lo ~hi (>>=/<>)
  | false, Some by -> gen_list t ~lo ~hi (>>=/<=) |> sort by
  | true,  Some by -> gen_list t ~lo ~hi (>>=/<)  |> sort by

let tuple t1 t2 =
  t1 >>= fun x1 ->
  t2 >>| fun x2 ->
  (x1, x2)

let tuple3 t1 t2 t3 =
  t1 >>= fun x1 ->
  t2 >>= fun x2 ->
  t3 >>| fun x3 ->
  (x1, x2, x3)

let tuple4 t1 t2 t3 t4 =
  t1 >>= fun x1 ->
  t2 >>= fun x2 ->
  t3 >>= fun x3 ->
  t4 >>| fun x4 ->
  (x1, x2, x3, x4)

let tuple5 t1 t2 t3 t4 t5 =
  t1 >>= fun x1 ->
  t2 >>= fun x2 ->
  t3 >>= fun x3 ->
  t4 >>= fun x4 ->
  t5 >>| fun x5 ->
  (x1, x2, x3, x4, x5)

let tuple6 t1 t2 t3 t4 t5 t6 =
  t1 >>= fun x1 ->
  t2 >>= fun x2 ->
  t3 >>= fun x3 ->
  t4 >>= fun x4 ->
  t5 >>= fun x5 ->
  t6 >>| fun x6 ->
  (x1, x2, x3, x4, x5, x6)

let option t =
  union
    [ singleton None
    ; t >>| Option.return
    ]

let rec permute list =
  match list with
  | [] -> singleton []
  | x :: list ->
    permute list
    >>= fun list ->
    int_between
      ~lower_bound:(`Inclusive 0)
      ~upper_bound:(`Inclusive (List.length list))
    >>| fun index ->
    let prefix, suffix = List.split_n list index in
    prefix @ [ x ] @ suffix

let chars_matching f =
  of_fun (fun () -> of_list (List.filter ~f (List.init 256 ~f:Char.of_int_exn)))

let char_is_punctuation c =
  Char.is_print c
  && not (Char.is_alphanum c)
  && not (Char.is_whitespace c)

let char_is_non_print c =
  not (Char.is_print c)
  && not (Char.is_whitespace c)

let char_uppercase   = chars_matching Char.is_uppercase
let char_lowercase   = chars_matching Char.is_lowercase
let char_digit       = chars_matching Char.is_digit
let char_whitespace  = chars_matching Char.is_whitespace
let char_alpha       = chars_matching Char.is_alpha
let char_alphanum    = chars_matching Char.is_alphanum
let char_punctuation = chars_matching char_is_punctuation
let char_non_print   = chars_matching char_is_non_print

let char_print =
  weighted_union
    [ 10., char_alphanum
    ;  1., char_punctuation
    ;  1., char_whitespace
    ]

let char =
  weighted_union
    [ 10., char_print
    ;  1., char_non_print
    ]

let string_of char_t =
  list char_t
  >>| String.of_char_list

let string = string_of char

let sexp = recursive (fun sexp_t ->
  variant string (list sexp_t)
  >>| function
  | `A atom -> Sexp.Atom atom
  | `B list -> Sexp.List list)

let rec int63_pair_lexicographic
          ~fst_lower_bound ~fst_upper_bound
          ~snd_lower_bound ~snd_upper_bound
          ~snd_start ~snd_final
  =
  let open Int63 in
  if fst_lower_bound > fst_upper_bound
  || snd_lower_bound > snd_upper_bound
  then empty
  else if fst_lower_bound = fst_upper_bound
  then
    let fst = fst_lower_bound in
    int63_between
      ~lower_bound:(`Inclusive snd_start)
      ~upper_bound:(`Inclusive snd_final)
    >>| fun snd ->
    fst, snd
  else if snd_start > snd_lower_bound
  then
    let top_row =
      int63_between
        ~lower_bound:(`Inclusive snd_start)
        ~upper_bound:(`Inclusive snd_upper_bound)
      >>| fun snd ->
      fst_lower_bound, snd
    in
    let rest_rows =
      let fst_lower_bound = succ fst_lower_bound in
      let snd_start       =      snd_lower_bound in
      int63_pair_lexicographic
        ~fst_lower_bound ~fst_upper_bound
        ~snd_lower_bound ~snd_upper_bound
        ~snd_start ~snd_final
    in
    union [ top_row ; rest_rows ]
  else if snd_final < snd_upper_bound
  then
    let bot_row =
      int63_between
        ~lower_bound:(`Inclusive snd_lower_bound)
        ~upper_bound:(`Inclusive snd_final)
      >>| fun snd ->
      fst_upper_bound, snd
    in
    let rest_rows =
      let fst_upper_bound = pred fst_upper_bound in
      let snd_final = snd_upper_bound in
      int63_pair_lexicographic
        ~fst_lower_bound ~fst_upper_bound
        ~snd_lower_bound ~snd_upper_bound
        ~snd_start ~snd_final
    in
    union [ rest_rows ; bot_row ]
  else
    tuple
      (int63_between
        ~lower_bound:(`Inclusive fst_lower_bound)
        ~upper_bound:(`Inclusive fst_upper_bound))
      (int63_between
        ~lower_bound:(`Inclusive snd_lower_bound)
        ~upper_bound:(`Inclusive snd_upper_bound))

let min_pos_normal = Float.min_positive_normal_value
let max_pos_normal = Float.max_finite_value

let min_pos_subnormal = Float.min_positive_subnormal_value
let max_pos_subnormal = min_pos_normal |> Float.one_ulp `Down

let min_normal_mantissa = Float.ieee_mantissa min_pos_normal
let max_normal_mantissa = Float.ieee_mantissa max_pos_normal

let nan_exponent = Float.ieee_exponent Float.nan
let min_nan_mantissa = min_normal_mantissa |> Int63.succ
let max_nan_mantissa = max_normal_mantissa

let float_nan = function
  | `None -> empty
  | `Single -> singleton Float.nan
  | `All ->
    int63_between
      ~lower_bound:(`Inclusive min_nan_mantissa)
      ~upper_bound:(`Inclusive max_nan_mantissa)
    >>= fun mantissa ->
    bool
    >>| fun negative ->
    let exponent = nan_exponent in
    Float.create_ieee_exn ~negative ~exponent ~mantissa

let float_pos_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate =
  if lower_bound > max_pos_candidate
  || upper_bound < min_pos_candidate
  then empty
  else
    let lower = Float.max lower_bound min_pos_candidate in
    let upper = Float.min upper_bound max_pos_candidate in
    int63_pair_lexicographic
      ~fst_lower_bound:(Float.ieee_exponent lower |> Int63.of_int)
      ~fst_upper_bound:(Float.ieee_exponent upper |> Int63.of_int)
      ~snd_lower_bound:(Float.ieee_mantissa min_pos_candidate)
      ~snd_upper_bound:(Float.ieee_mantissa max_pos_candidate)
      ~snd_start:(Float.ieee_mantissa lower)
      ~snd_final:(Float.ieee_mantissa upper)
    >>| fun (exponent, mantissa) ->
    let negative = false in
    let exponent = Int63.to_int_exn exponent in
    Float.create_ieee_exn ~negative ~exponent ~mantissa

let float_neg_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate =
  float_pos_range
    ~lower_bound:(Float.neg upper_bound)
    ~upper_bound:(Float.neg lower_bound)
    ~min_pos_candidate
    ~max_pos_candidate
  >>| Float.neg

let float_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate =
  union
    [ float_pos_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate
    ; float_neg_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate
    ]

let float_zero ~lower_bound ~upper_bound =
  float_range ~lower_bound ~upper_bound
    ~min_pos_candidate:Float.zero
    ~max_pos_candidate:Float.zero

let float_subnormal ~lower_bound ~upper_bound =
  float_range ~lower_bound ~upper_bound
    ~min_pos_candidate:min_pos_subnormal
    ~max_pos_candidate:max_pos_subnormal

let float_normal ~lower_bound ~upper_bound =
  float_range ~lower_bound ~upper_bound
    ~min_pos_candidate:min_pos_normal
    ~max_pos_candidate:max_pos_normal

let float_infinity ~lower_bound ~upper_bound =
  float_range ~lower_bound ~upper_bound
    ~min_pos_candidate:Float.infinity
    ~max_pos_candidate:Float.infinity

let float_between_inclusive ~with_nan ~lower_bound ~upper_bound =
  if Float.is_nan lower_bound
  || Float.is_nan upper_bound
  then failwith "Generator.float_between_inclusive: NaN bound"
  else if Float.(>) lower_bound upper_bound
  then empty
  else
    weighted_union
      [ 5., float_normal    ~lower_bound ~upper_bound
      ; 4., float_subnormal ~lower_bound ~upper_bound
      ; 3., float_zero      ~lower_bound ~upper_bound
      ; 2., float_infinity  ~lower_bound ~upper_bound
      ; 1., float_nan       with_nan
      ]

let float =
  float_between_inclusive
    ~with_nan:`Single
    ~lower_bound:Float.neg_infinity
    ~upper_bound:Float.infinity

let fn_with_sexp = Janecheck_kernel.Std.Observer.observe

let curry_with_sexp
      (type a)
      (type b)
      (type c)
      (dom : a Janecheck_kernel.Std.Observer.t)
      (rng : ((b -> c) * Sexp.t) t)
  : ((a -> b -> c) * Sexp.t) t
  =
  fn_with_sexp dom rng ~sexp_of_rng:snd
  >>| fun (f, sexp) ->
  (fun a b -> (fst (f a)) b), sexp

let fn2_with_sexp dom1 dom2 rng ~sexp_of_rng =
  curry_with_sexp dom1 (fn_with_sexp dom2 rng ~sexp_of_rng)
let fn3_with_sexp dom1 dom2 dom3 rng ~sexp_of_rng =
  curry_with_sexp dom1 (fn2_with_sexp dom2 dom3 rng ~sexp_of_rng)
let fn4_with_sexp dom1 dom2 dom3 dom4 rng ~sexp_of_rng =
  curry_with_sexp dom1 (fn3_with_sexp dom2 dom3 dom4 rng ~sexp_of_rng)
let fn5_with_sexp dom1 dom2 dom3 dom4 dom5 rng ~sexp_of_rng =
  curry_with_sexp dom1 (fn4_with_sexp dom2 dom3 dom4 dom5 rng ~sexp_of_rng)
let fn6_with_sexp dom1 dom2 dom3 dom4 dom5 dom6 rng ~sexp_of_rng =
  curry_with_sexp dom1 (fn5_with_sexp dom2 dom3 dom4 dom5 dom6 rng ~sexp_of_rng)

let compare_fn_with_sexp dom =
  fn_with_sexp dom int ~sexp_of_rng:<:sexp_of< int >>
  >>| fun (get_index, sexp) ->
  (fun x y -> Int.compare (get_index x) (get_index y)),
  <:sexp_of< [`compare_using_index_fn of Sexp.t] >> (`compare_using_index_fn sexp)

let equal_fn_with_sexp dom =
  compare_fn_with_sexp dom
  >>| fun (cmp, sexp) ->
  (fun x y -> Int.equal (cmp x y) 0),
  <:sexp_of< [`equal_fn_of_compare_fn of Sexp.t] >> (`equal_fn_of_compare_fn sexp)

let fn  a b           = fn_with_sexp  a b           ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn2 a b c         = fn2_with_sexp a b c         ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn3 a b c d       = fn3_with_sexp a b c d       ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn4 a b c d e     = fn4_with_sexp a b c d e     ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn5 a b c d e f   = fn5_with_sexp a b c d e f   ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn6 a b c d e f g = fn6_with_sexp a b c d e f g ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let compare_fn a = compare_fn_with_sexp a >>| fst
let equal_fn   a = equal_fn_with_sexp   a >>| fst

let float_between ~with_nan ~lower_bound ~upper_bound =
  match lower_bound, upper_bound with
  | (`Exclusive lower, _) | (`Inclusive lower, _) when Float.is_nan lower ->
    failwith "Janecheck.Std.Generator.float_between: lower bound = NaN"
  | (_, `Exclusive upper) | (_, `Inclusive upper) when Float.is_nan upper ->
    failwith "Janecheck.Std.Generator.float_between: upper bound = NaN"
  | `Exclusive lower, _ when Float.equal lower Float.infinity     -> float_nan with_nan
  | _, `Exclusive upper when Float.equal upper Float.neg_infinity -> float_nan with_nan
  | _ ->
    let lower_bound =
      match lower_bound with
      | `Unbounded             -> Float.neg_infinity
      | `Inclusive lower_bound -> lower_bound
      | `Exclusive lower_bound -> Float.one_ulp `Up lower_bound
    in
    let upper_bound =
      match upper_bound with
      | `Unbounded             -> Float.infinity
      | `Inclusive upper_bound -> upper_bound
      | `Exclusive upper_bound -> Float.one_ulp `Down upper_bound
    in
    float_between_inclusive ~with_nan ~upper_bound ~lower_bound
