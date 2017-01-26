(**
   Extensions to [Core.Core_int]
*)

module Verified_spec = struct
  include Core.Int
  let module_name = "Int"
end

include Number.Make_verified_std (Verified_spec)

external gcd : int -> int -> int = "core_extended_extended_int_gcd" [@@noalloc]

let%test _ = gcd 120 72 = 24
let%test _ = gcd 13 17 = 1
let%test _ = gcd 12345 12345 = 12345
let%test _ = gcd 300 800 = 100
let%test _ = gcd 300 (-800) = 100
let%test _ = gcd (-300) 800 = 100
let%test _ = gcd (-300) (-800) = 100
let%test _ = gcd 800 300 = 100
let%test _ = gcd 800 (-300) = 100
let%test _ = gcd (-800) 300 = 100
let%test _ = gcd (-800) (-300) = 100
let%test _ = gcd 12 45 = 3
let%test _ = gcd min_int min_int = min_int
let%test _ = gcd min_int 0 = min_int
let%test _ = gcd min_int 24 = 8
let%test _ = gcd min_int 5 = 1
let%test _ = gcd min_int max_int = 1
let%test _ = gcd max_int max_int = max_int

let%bench_module "gcd" = (module struct
  let a = Array.init 1000 (fun _ -> Random.int 1_000_000)
  let%bench "gcd" =
    let a = Obj.magic a in
    for i = 0 to Array.length a - 1 do
      let a_i = Array.unsafe_get a i in
      for j = 0 to Array.length a - 1 do
        let a_j = Array.unsafe_get a j in
        ignore (gcd a_i a_j)
      done
    done
end)
