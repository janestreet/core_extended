open Core

(* a single 63 bit chunk of the array, bounds checking is left to the main module. We can
   only use 62 bits, because of the sign bit *)
module Int63_chunk : sig
  type t

  val empty : t
  val get : t -> int -> bool
  val set : t -> int -> bool -> t
  val popcount : t -> int
  val count : t -> int -> int
end = struct
  open Int63

  type t = Int63.t

  let empty = zero
  let get t i = bit_and t (shift_left one i) > zero

  let set t i v =
    if v
    then bit_or t (shift_left one i)
    else bit_and t (bit_xor minus_one (shift_left one i))
  ;;

  let popcount = popcount
  let count t i = bit_and t (shift_left one Int.(i + 1) - one) |> popcount
end

type t =
  { data : Int63_chunk.t Array.t
  ; length : int
  }

(* We can't use the sign bit, so we only get to use 62 bits *)
let bits_per_bucket = 62

let create sz =
  let len = 1 + (sz / bits_per_bucket) in
  if sz < 0 || len > Array.max_length then invalid_argf "invalid size" ();
  { data = Array.create ~len Int63_chunk.empty; length = sz }
;;

let length t = t.length
let bucket i = i / bits_per_bucket
let index i = i mod bits_per_bucket

let bounds_check t i =
  if i < 0 || i >= t.length then invalid_argf "Bitarray: out of bounds" ()
;;

let get t i =
  bounds_check t i;
  Int63_chunk.get t.data.(bucket i) (index i)
;;

let set t i v =
  bounds_check t i;
  let bucket = bucket i in
  t.data.(bucket) <- Int63_chunk.set t.data.(bucket) (index i) v
;;

let clear t = Array.fill t.data ~pos:0 ~len:(Array.length t.data) Int63_chunk.empty

let fold =
  let rec loop t n ~init ~f =
    if n < t.length then loop t (n + 1) ~init:(f init (get t n)) ~f else init
  in
  fun t ~init ~f -> loop t 0 ~init ~f
;;

let iter t ~f = fold t ~init:() ~f:(fun _ v -> f v)

let sexp_of_t t =
  Array.sexp_of_t Bool.sexp_of_t (Array.init t.length ~f:(fun i -> get t i))
;;

let t_of_sexp sexp =
  let a = Array.t_of_sexp Bool.t_of_sexp sexp in
  let t = create (Array.length a) in
  Array.iteri a ~f:(fun i v -> set t i v);
  t
;;

let count ?len t =
  match Option.value ~default:(length t) len with
  | 0 -> 0
  | len ->
    let max_index = len - 1 in
    bounds_check t max_index;
    let bucket = bucket max_index in
    let c = ref 0 in
    for b = 0 to bucket - 1 do
      c := !c + Int63_chunk.popcount t.data.(b)
    done;
    !c + Int63_chunk.count t.data.(bucket) (index max_index)
;;
