(**
   Extensions to [Core.Core_int]
*)

module Verified_spec = struct
  include Core.Std.Int
  let module_name = "Int"
end

include Number.Make_verified_std (Verified_spec)

external gcd : int -> int -> int = "core_extended_extended_int_gcd" "noalloc"

TEST = gcd 120 72 = 24
TEST = gcd 13 17 = 1
TEST = gcd 12345 12345 = 12345
TEST = gcd 300 800 = 100
TEST = gcd 300 (-800) = 100
TEST = gcd (-300) 800 = 100
TEST = gcd (-300) (-800) = 100
TEST = gcd 800 300 = 100
TEST = gcd 800 (-300) = 100
TEST = gcd (-800) 300 = 100
TEST = gcd (-800) (-300) = 100
TEST = gcd 12 45 = 3
TEST = gcd min_int min_int = min_int
TEST = gcd min_int 0 = min_int
TEST = gcd min_int 24 = 8
TEST = gcd min_int 5 = 1
TEST = gcd min_int max_int = 1
TEST = gcd max_int max_int = max_int

BENCH_MODULE "gcd" = struct
  let a = Array.init 1000 (fun _ -> Random.int 1_000_000)
  BENCH "gcd" =
    let a = Obj.magic a in
    for i = 0 to Array.length a - 1 do
      let a_i = Array.unsafe_get a i in
      for j = 0 to Array.length a - 1 do
        let a_j = Array.unsafe_get a j in
        ignore (gcd a_i a_j)
      done
    done
end
