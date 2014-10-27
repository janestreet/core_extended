open Core.Std

(* See Intel Architectures Optimization Reference Manual, section 9.2.4. to understand how
   fast division works.  In short, if Y is known in advance we replace:

     X / Y

   with:

     X * (2^N / Y) / 2^N

   This is faster than division because multiplication is much faster than division and
   [2^N / Y] can be precomputed.  Division by [2^N] is done with bit shift, which is fast.
   The largest [N] and the largest possible [X] have to be determined. *)
type t = {
  max   : int;
  (** Integers bigger and equal [max] cannot be divided fast, so a normal divide should be
      used. *)
  div   : int;
  (** The denominator. *)
  mul   : int;
  (** If fast division is used, the multiplier, [2^N / Y] in the above notation*)
  shift : int;
  (** If fast division is used, the bit shift, [N] in the above notation *)
} with bin_io, sexp

(* The exact result and the approximation are:

     d  = x / y
     d' = x * ceil(2^31 / y) / 2^31
     R  = floor(d)
     R' = floor(d')

   [|R' - R| <= 1] assuming [0 < x < 2^31] and [0 < y < 2^31]. *)
let create ~denominator:div =
  let fmax = Float.of_int (2 lsl 30) in
  let shift, mul =
    let rec loop shift mul =
      let mul2 = mul *. 2. in
      if mul2 >= fmax || Float.round_up mul =. mul then
        shift, Float.to_int (Float.round_up mul)
      else loop (shift + 1) mul2
    in
    loop 0 (1. /. (Float.of_int div))
  in
  (* Compute the maximum numerator that won't overflow *)
  let max =
    let rec loop max =
      let max' = max lsl 1 in
      if mul * max' < 0 then max else loop max'
    in
    loop 1
  in
  { max; div; mul; shift }

let div i t =
  if i < 0 || i >= t.max then i / t.div
  else
    let dividend = i * t.mul in
    let dividend = dividend asr t.shift in
    (* Correct for the possible off by one error *)
    if dividend * t.div > i then dividend - 1 else dividend

TEST_UNIT =
  let bigrandom () = Random.int (2 lsl 15) lxor (Random.int (2 lsl 15) lsl 15) in
  let xs = Array.init (16 * 1024) ~f:(fun i ->
    if i < 8192 then i + 1 else bigrandom ()) in
  let ys = Array.init (16 * 1024) ~f:(fun i ->
    if i < 8192 then i + 1 else bigrandom ()) in
  let ts = Array.map xs ~f:(fun x -> create ~denominator:x) in
  for i = 0 to Array.length xs - 1 do
    let x = Array.unsafe_get xs i in
    let t = Array.unsafe_get ts i in
    for j = 0 to Array.length ys - 1 do
      let y = Array.unsafe_get ys j in
      let d1 = div y t in
      let d2 = y / x in
      assert (d1 = d2)
    done
  done


(* See mli also. *)
BENCH_MODULE "" = struct
  BENCH "id" = ()

  BENCH_FUN "Int.(/)" =
    let x = Random.int (1024 * 1024) in
    let t =
      if Random.bool ()
      then 12345
      else 12345
    in
    let s' = ref 0 in
    fun () ->
      s' := x / t

  BENCH_FUN "div" =
    let x = Random.int (1024 * 1024) in
    let t =
      if Random.bool ()
      then create ~denominator:12345
      else create ~denominator:12345
    in
    let s' = ref 0 in
    fun () ->
      s' := div x t

  BENCH "create" =
    let t = create ~denominator:12345 in
    assert (div 12345678 t <> 0)

  (* Some longer running tests. Looping over these arrays aggregates a performance result
     for many different values. Note that these are also subject to the cache effects and
     such from accessing the arrays over which they run.

     On Sandy Bridge Intel(R) Xeon(R) CPU E5-2687W @ 3.10GHz:
     ┌────────────────────────────────────┬─────────────────┬─────────┬────────────┐
     │ Name                               │        Time/Run │ mWd/Run │ Percentage │
     ├────────────────────────────────────┼─────────────────┼─────────┼────────────┤
     │ [fast_int_div.ml:] idiv x 1M       │ 86_191_712.58ns │         │    100.00% │
     │ [fast_int_div.ml:] fast div x 1M   │ 24_299_958.23ns │         │     28.19% │
     └────────────────────────────────────┴─────────────────┴─────────┴────────────┘
  *)
  let ts = Array.init 1000 ~f:(fun _ -> create ~denominator:(Random.int (1024 * 1024)))
  let ys = Array.init 1000 ~f:(fun _ -> Random.int (512 * 1024 * 1024))

  BENCH "idiv x 1M" =
    let dummy = ref 0 in
    for i = 0 to Array.length ts - 1 do
      let x = (Array.unsafe_get ts i).div in
      for j = 0 to Array.length ys - 1 do
        let y = Array.unsafe_get ys j in
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
        dummy := !dummy + y / x;
      done
    done;
    assert (!dummy <> 0)

  BENCH "fast div x 1M" =
    let dummy = ref 0 in
    for i = 0 to Array.length ts - 1 do
      let t = Array.unsafe_get ts i in
      for j = 0 to Array.length ys - 1 do
        let y = Array.unsafe_get ys j in
        dummy := !dummy + div y t;
        dummy := !dummy + div y t;
        dummy := !dummy + div y t;
        dummy := !dummy + div y t;
        dummy := !dummy + div y t;
        dummy := !dummy + div y t;
        dummy := !dummy + div y t;
        dummy := !dummy + div y t;
        dummy := !dummy + div y t;
        dummy := !dummy + div y t
      done
    done;
    assert (!dummy <> 0)
end
