(**
   Extensions to [Core.Core_int]
*)

module Verified_spec = struct
  include Core.Std.Int
  let module_name = "Int"
end

include Number.Make_verified_std (Verified_spec)

(* See en.wikipedia.org/wiki/Binary_GCD_algorithm. *)
let gcd a b =
  (* Invariant: calls to [gcd a b d] have either a = b = 0 or (a > 0 and b >= 0). *)
  let rec gcd a b d =
    if b = 0 then a * d
    else if a land 1 = 0 then begin
      let a = a asr 1 in
      if b land 1 = 0 then gcd a (b asr 1) (d * 2) else gcd a b d
    end else if b land 1 = 0 then
      gcd a (b asr 1) d
    else
      let m = a - b in
      let m = if m < 0 then - m else m in
      gcd b m d
  in
  (* gcd_with_min_int a = GCD(a, min_int).  Assumes a >= 0 or a = min_int *)
  let gcd_with_min_int a =
    if a <= 0 then min_int (* a = min_int or a = 0; either way, GCD is min_int *)
    else if a land 1 = 0 then gcd (a asr 1) (- (min_int asr 1)) 2
    else gcd a (- (min_int asr 1)) 1
  in
  let a = if a >= 0 then a else - a in
  let b = if b >= 0 then b else - b in
  if a > b then begin
    if b >= 0 then gcd a b 1
    else gcd_with_min_int a (* b must have been min_int *)
  end else begin
    if a >= 0 then gcd b a 1
    else gcd_with_min_int b (* a must have been min_int *)
  end

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
