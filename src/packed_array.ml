open Core

module Core_bool   = Bool
module Core_char   = Char
module Core_float  = Float
module Core_int    = Int
module Core_int32  = Int32
module Core_int64  = Int64
module Core_string = String

module type Basic = sig
  type elt [@@deriving sexp, bin_io]
  type t

  val length : t -> int
  val unsafe_get : t -> int -> elt
  val of_array : elt array -> t
  val unsafe_slice : t -> pos:int -> len:int -> t
end

module type S = sig
  include Basic

  include Sexpable.S with type t := t
  include Binable.S with type t := t

  val get : t -> int -> elt
  val slice : t -> pos:int -> len:int -> t
  val iter : t -> f:(elt -> unit) -> unit
  val fold : t -> init:'a -> f:('a -> elt -> 'a) -> 'a

  val of_array : elt array -> t
  val to_array : t -> elt array

  val of_list : elt list -> t
  val to_list : t -> elt list

  val empty : t
end

module Make (B : Basic) = struct
  include B

  exception Invalid_index of int [@@deriving sexp]
  exception Invalid_slice of int * int [@@deriving sexp]

  let valid_index t i = Int.(>=) i 0 && Int.(<) i (length t)

  let get t i =
    if valid_index t i
    then unsafe_get t i
    else raise (Invalid_index i)

  let slice t ~pos ~len =
    if valid_index t pos && Int.(<=) (pos + len) (length t)
    then unsafe_slice t ~pos ~len
    else raise (Invalid_slice (pos, len))

  let of_list xs = of_array (Array.of_list xs)

  let to_array t = Array.init (length t) ~f:(fun i -> unsafe_get t i)

  let to_list t = List.init (length t) ~f:(fun i -> unsafe_get t i)

  let empty = of_array [||]

  let iter t ~f = for i = 0 to length t - 1 do f (unsafe_get t i) done

  let fold t ~init ~f =
    let len = length t in
    let rec loop ix acc =
      if Int.(<) ix len
      then loop (ix+1) (f acc (unsafe_get t ix))
      else acc
    in
    loop 0 init

  include (Binable.Of_binable
             (struct type t = elt array [@@deriving bin_io] end)
             (struct
               type nonrec t = t
               let to_binable = to_array
               let of_binable = of_array
             end) : Binable.S with type t := t)

  let t_of_sexp sexp = of_array ([%of_sexp: elt array] sexp)
  let sexp_of_t t = [%sexp_of: elt array] (to_array t)
end

module type Basic_bigarray = sig
  type elt [@@deriving sexp, bin_io]
  type kind
  val kind : (elt, kind) Bigarray.kind
end

module Of_bigarray (B : Basic_bigarray) = struct
  open Bigarray

  include Make(struct
    include B

    type t = {
      buf : (elt, kind, c_layout) Array1.t;
      off : int;
      len : int
    }

    let length t = t.len
    let unsafe_get t ix = Array1.unsafe_get t.buf (ix + t.off)

    let of_array arr = {
      buf = Array1.of_array kind c_layout arr;
      off = 0;
      len = Array.length arr;
    }

    let unsafe_slice t ~pos ~len = { t with off = t.off + pos; len; }
  end)
end

module type T = sig
  type elt
  include Comparable.S with type t := elt
  module S : S with type elt := elt

  val test_list : unit -> elt list
end

module Test (T : T) = struct
  include T

  let test_list = T.test_list ()
  let test_array = Array.of_list test_list
  let test_t = S.of_array test_array
  let test_elt = List.hd_exn test_list

  let list_equal xs ys = List.equal xs ys ~equal:(=)
  let array_equal xs ys = Array.equal xs ys ~equal:(=)

  (* If this fails then some of the other test results would be invalid *)
  let%test "test_list long enough" = Int.(>) (Array.length test_array) 75

  let%test "of_list o to_list = ident  (empty)" =
    list_equal (S.to_list (S.of_list [])) []

  let%test "of_list o to_list = ident  (singleton)" =
    let x = [test_elt] in
    list_equal (S.to_list (S.of_list x)) x

  let%test "of_list o to_list = ident  (non-empty)" =
    list_equal (S.to_list (S.of_list test_list)) test_list

  let%test "of_array o to_array = ident  (empty)" =
    array_equal (S.to_array (S.of_array [||])) [||]

  let%test "of_array o to_array = ident  (singleton)" =
    let x = Array.create ~len:1 test_elt in
    array_equal (S.to_array (S.of_array x)) x

  let%test "of_array o to_array = ident  (non-empty)" =
    array_equal (S.to_array (S.of_array test_array)) test_array

  let%test "iter" =
    let xs = ref [] in
    let ys = ref [] in
    List.iter test_list ~f:(fun x -> xs := x :: !xs);
    S.iter test_t ~f:(fun y -> ys := y :: !ys);
    list_equal !xs !ys

  let%test "t_of_sexp o sexp_of_t = ident  (empty)" =
    let xs = S.of_array [||] in
    array_equal (S.to_array (S.t_of_sexp (S.sexp_of_t xs))) (S.to_array xs)

  let%test "t_of_sexp o sexp_of_t = ident  (singleton)" =
    let xs = S.of_array (Array.create ~len:1 test_elt) in
    array_equal (S.to_array (S.t_of_sexp (S.sexp_of_t xs))) (S.to_array xs)

  let%test "t_of_sexp o sexp_of_t = ident  (non-empty)" =
    array_equal (S.to_array (S.t_of_sexp (S.sexp_of_t test_t))) (S.to_array test_t)

  let%test "slice  (non-empty)" =
    list_equal (S.to_list (S.slice test_t ~pos:25 ~len:25))
      (List.sub (S.to_list test_t) ~pos:25 ~len:25)

  let%test "slice twice  (non-empty)" =
    list_equal (S.to_list (S.slice (S.slice test_t ~pos:25 ~len:25) ~pos:10 ~len:5))
      (List.sub (List.sub (S.to_list test_t) ~pos:25 ~len:25) ~pos:10 ~len:5)

  let%test "marshalable" =
    array_equal (S.to_array test_t)
      (S.to_array (Marshal.from_string (Marshal.to_string test_t [Marshal.Closures]) 0))
end

module Char = Of_bigarray (struct
  type elt = char [@@deriving sexp, bin_io]
  type kind = Bigarray.int8_unsigned_elt
  let kind = Bigarray.char
end)

let%test_module "char" = (module Test(struct
  type elt = char
  include (Core_char : Comparable.S with type t := elt)
  module S = (Char : S with type elt := elt)
  let test_list () = List.init 100 ~f:Core_char.of_int_exn
end))

module Int = Of_bigarray (struct
  type elt = int [@@deriving sexp, bin_io]
  type kind = Bigarray.int_elt
  let kind = Bigarray.int
end)

let%test_module "int" = (module Test(struct
  type elt = int
  include (Core_int : Comparable.S with type t := elt)
  module S = (Int : S with type elt := elt)
  let test_list () = List.init 100 ~f:ident
end))

module Int8_unsigned = Of_bigarray (struct
  type elt = int [@@deriving sexp, bin_io]
  type kind = Bigarray.int8_unsigned_elt
  let kind = Bigarray.int8_unsigned
end)

let%test_module "int8_unsigned" = (module Test(struct
  type elt = int
  include (Core_int : Comparable.S with type t := elt)
  module S = (Int8_unsigned : S with type elt := elt)
  let test_list () = List.init 100 ~f:ident
end))

module Int8 = Of_bigarray (struct
  type elt = int [@@deriving sexp, bin_io]
  type kind = Bigarray.int8_signed_elt
  let kind = Bigarray.int8_signed
end)

let%test_module "int8" = (module Test(struct
  type elt = int
  include (Core_int : Comparable.S with type t := elt)
  module S = (Int8 : S with type elt := elt)
  let test_list () = List.init 100 ~f:ident
end))

module Int16_unsigned = Of_bigarray (struct
  type elt = int [@@deriving sexp, bin_io]
  type kind = Bigarray.int16_unsigned_elt
  let kind = Bigarray.int16_unsigned
end)

let%test_module "int16_unsigned" = (module Test(struct
  type elt = int
  include (Core_int : Comparable.S with type t := elt)
  module S = (Int16_unsigned : S with type elt := elt)
  let test_list () = List.init 100 ~f:ident
end))

module Int16 = Of_bigarray (struct
  type elt = int [@@deriving sexp, bin_io]
  type kind = Bigarray.int16_signed_elt
  let kind = Bigarray.int16_signed
end)

let%test_module "int16" = (module Test(struct
  type elt = int
  include (Core_int : Comparable.S with type t := elt)
  module S = (Int16 : S with type elt := elt)
  let test_list () = List.init 100 ~f:ident
end))

module Int32 = Of_bigarray (struct
  type elt = int32 [@@deriving sexp, bin_io]
  type kind = Bigarray.int32_elt
  let kind = Bigarray.int32
end)

let%test_module "int32" = (module Test(struct
  type elt = int32
  include (Core_int32 : Comparable.S with type t := elt)
  module S = (Int32 : S with type elt := elt)
  let test_list () = List.init 100 ~f:Core_int.to_int32_exn
end))

module Int64 = Of_bigarray (struct
  type elt = int64 [@@deriving sexp, bin_io]
  type kind = Bigarray.int64_elt
  let kind = Bigarray.int64
end)

let%test_module "int64" = (module Test(struct
  type elt = int64
  include (Core_int64 : Comparable.S with type t := elt)
  module S = (Int64 : S with type elt := elt)
  let test_list () = List.init 100 ~f:Core_int.to_int64
end))

module Float32 = Of_bigarray (struct
  type elt = float [@@deriving sexp, bin_io]
  type kind = Bigarray.float32_elt
  let kind = Bigarray.float32
end)

let%test_module "float32" = (module Test(struct
  type elt = float
  include (Core_float : Comparable.S with type t := elt)
  module S = (Float32 : S with type elt := elt)
  let test_list () = List.init 100 ~f:Core_float.of_int
end))

module Float = Of_bigarray (struct
  type elt = float [@@deriving sexp, bin_io]
  type kind = Bigarray.float64_elt
  let kind = Bigarray.float64
end)

let%test_module "float" = (module Test(struct
  type elt = float
  include (Core_float : Comparable.S with type t := elt)
  module S = (Float : S with type elt := elt)
  let test_list () = List.init 100 ~f:Core_float.of_int
end))

module Bool = Make (struct
  open Bigarray

  type elt = bool [@@deriving sexp, bin_io]
  type t = {
    data: (int64, int64_elt, c_layout) Array1.t;
    pos: int;
    len: int;
  }

  let length t = t.len

  let mask ix =
    let int64_ix = ix / 64 in
    let bit_ix = ix mod 64 in
    let mask = Core_int64.shift_left Core_int64.one bit_ix in
    int64_ix, mask

  let unsafe_set data ix p =
    let int64_ix, mask = mask ix in
    let x = Array1.unsafe_get data int64_ix in
    let x =
      if p
      then Core_int64.bit_or x mask
      else Core_int64.bit_and x (Core_int64.bit_not mask)
    in
    Array1.unsafe_set data int64_ix x

  let unsafe_get t ix =
    let ix = ix + t.pos in
    let int64_ix, mask = mask ix in
    let x = Array1.unsafe_get t.data int64_ix in
    Core_int64.(<>) (Core_int64.bit_and x mask) Core_int64.zero

  let of_array arr =
    let len = Array.length arr in
    let data = Array1.create int64 c_layout ((len + 63) / 64) in
    Array.iteri arr ~f:(fun ix elt -> unsafe_set data ix elt);
    { data; pos = 0; len; }

  let unsafe_slice t ~pos ~len = { t with pos = t.pos + pos; len; }
end)

let%test_module "bool" = (module Test(struct
  type elt = bool
  include (Core_bool : Comparable.S with type t := elt)
  module S = (Bool : S with type elt := elt)
  let test_list () = List.init 100 ~f:(fun ix -> Core_int.(=) 0 (ix mod 2))
end))

module Tuple2 (A : Basic) (B : Basic) : sig
  type t
  type elt = A.elt * B.elt
  include (S with type elt := elt and type t := t)
  val zip_exn : A.t -> B.t -> t
  val unzip : t -> A.t * B.t
end = struct
  module T = struct
    type t = A.t * B.t
    type elt = A.elt * B.elt [@@deriving sexp, bin_io]

    let length (a, _) = A.length a
    let unsafe_get (a, b) ix = A.unsafe_get a ix, B.unsafe_get b ix
    let unsafe_slice (a, b) ~pos ~len =
      (A.unsafe_slice a ~pos ~len, B.unsafe_slice b ~pos ~len)
    let of_array arr =
      let len = Array.length arr in
      let arr1, arr2 =
        if Core_int.(=) len 0 then
          [||], [||]
        else begin
          let arr1 = Array.create ~len (Tuple2.get1 arr.(0)) in
          let arr2 = Array.create ~len (Tuple2.get2 arr.(0)) in
          for i = len - 1 downto 1 do
            arr1.(i) <- Tuple2.get1 arr.(i);
            arr2.(i) <- Tuple2.get2 arr.(i);
          done;
          arr1, arr2
        end
      in
      A.of_array arr1, B.of_array arr2
  end

  include T

  include (Make(T) : S with type elt := elt and type t := t)

  let t_of_sexp sexp = of_array ([%of_sexp: (A.elt * B.elt) array] sexp)
  let sexp_of_t t = [%sexp_of: (A.elt * B.elt) array] (to_array t)

  exception Different_lengths [@@deriving sexp]

  let zip_exn a b =
    if Core_int.(=) (A.length a) (B.length b)
    then a, b
    else raise Different_lengths

  let unzip (a, b) = a, b
end

let%test_module "int*float" = (module struct
  module T = Tuple2(Int)(Float)

  module Test = Test(struct
    type elt = int * float
    include Tuple.Comparable(Core_int)(Core_float)
    module S = (T : S with type elt := elt)
    let test_list () = List.init 100 ~f:(fun i -> i, Core_float.of_int i)
  end)

  let%test_module _ = (module Test)

  include Test

  let%test "zip_exn o unzip = id  (empty)" =
    let xs = T.of_list [] in
    let a, b = T.unzip xs in
    array_equal (T.to_array (T.zip_exn a b)) (T.to_array xs)

  let%test "zip_exn o unzip = id  (singleton)" =
    let xs = T.of_list [test_elt] in
    let a, b = T.unzip xs in
    array_equal (T.to_array (T.zip_exn a b)) (T.to_array xs)

  let%test "zip_exn o unzip = id  (non-empty)" =
    let xs = T.of_list test_list in
    let a, b = T.unzip xs in
    array_equal (T.to_array (T.zip_exn a b)) (T.to_array xs)
end)

module Of_binable (B : sig
  include Binable.S
  include Sexpable.S with type t := t
end) = Make(struct
  type elt = B.t [@@deriving sexp, bin_io]
  type t = int array * Bin_prot.Common.buf

  let length (ps, _) = Array.length ps

  let unsafe_get (ps, buf) ix = B.bin_read_t buf ~pos_ref:(ref (Array.unsafe_get ps ix))

  let of_array arr =
    let buf_size = Array.fold arr ~init:0 ~f:(fun acc elt ->
      acc + B.bin_size_t elt) in
    let buf = Bigstring.create buf_size  in
    let posarr = Array.create ~len:(Array.length arr) 0 in
    ignore (Array.foldi arr ~init:0 ~f:(fun ix pos elt ->
      Array.unsafe_set posarr ix pos;
      B.bin_write_t buf ~pos elt));
    posarr, buf

  let unsafe_slice (ixs, buf) ~pos ~len = Array.sub ixs ~pos ~len, buf
end)

module String = Of_binable(String)

let%test_module "string" = (module Test(struct
  type elt = string
  include (Core_string : Comparable.S with type t := elt)
  module S = (String : S with type elt := elt)
  let test_list () = List.init 100 ~f:Core_int.to_string
end))

module Of_packed_array(P : S) = struct
  module T = struct
    module Slices = Tuple2(Int)(Int)

    type elt = P.t [@@deriving sexp, bin_io]
    type t = Slices.t * P.t

    let length (ss, _) = Slices.length ss

    let unsafe_get (ss, buf) ix =
      let pos, len = Slices.unsafe_get ss ix in
      P.unsafe_slice buf ~pos ~len

    let of_array arr =
      let len = Array.length arr in
      let buf_size = Array.fold arr ~init:0 ~f:(fun acc elt ->
        acc + P.length elt) in
      let slices = Array.create ~len (0, 0) in
      let buf =
        if Core_int.(=) buf_size 0
        then [||]
        else begin
          (* No bounds check is necessary in this loop because buf_size <> 0 only when at
             least one element of the array is nonempty. *)
          let rec loop ix =
            let p = Array.unsafe_get arr ix in
            if Core_int.(=) (P.length p) 0
            then loop (ix+1)
            else Array.create ~len:buf_size (P.unsafe_get p 0)
          in
          loop 0
        end
      in
      ignore (Array.foldi arr ~init:0 ~f:(fun ix pos elt ->
        let old_pos = pos in
        let pos =
          P.fold elt ~init:pos ~f:(fun pos elt ->
            Array.unsafe_set buf pos elt;
            pos+1)
        in
        Array.unsafe_set slices ix (old_pos, pos - old_pos);
        pos));
      Slices.of_array slices, P.of_array buf

    let unsafe_slice (ss, buf) ~pos ~len = Slices.unsafe_slice ss ~pos ~len, buf
  end

  include T

  include (Make(T) : S with type elt := elt and type t := t)

  let concat (ss, buf) =
    let ss_len = Slices.length ss in
    if Core_int.(=) ss_len 0
    then P.empty
    else begin
      let (pos, _) = Slices.unsafe_get ss 0 in
      let (last_pos, last_len) = Slices.unsafe_get ss (ss_len - 1) in
      let len = last_pos + last_len - pos in
      P.unsafe_slice buf ~pos ~len
    end
end

let%test_module "packed array of packed arrays of strings" = (module struct
  module T = struct
    type elt = String.t
    include Comparable.Make(struct
      type t = String.t [@@deriving sexp]
      let compare a b =
        List.compare Core_string.compare (String.to_list a) (String.to_list b)
    end)
    module S = Of_packed_array(String)

    let test_list () =
      List.init 100 ~f:(fun i -> String.of_list (List.init 100 ~f:(fun j ->
        Core_int.to_string (i*100 + j))))

    let test_concat test_t =
      List.equal
        (String.to_list (S.concat test_t))
        (List.concat_map (S.to_list test_t) ~f:String.to_list)
        ~equal:Core_string.equal

    let%test "concat" = test_concat (S.of_list (test_list ()))
    let%test "concat after slice" =
      test_concat (S.slice (S.of_list (test_list ())) ~pos:25 ~len:25)
  end

  let%test_module _ = (module Test(T))
end)
