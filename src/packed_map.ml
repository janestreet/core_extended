open Core

module type Key = sig
  type t [@@deriving sexp, bin_io]
  include Comparable.S with type t := t
  module Packed_array : Packed_array.S with type elt := t
end

module type Value = sig
  type t [@@deriving sexp, bin_io]
  module Packed_array : Packed_array.S with type elt := t
end

module type S = sig
  type t     [@@deriving sexp, bin_io]
  type key   [@@deriving sexp, bin_io]
  type value [@@deriving sexp, bin_io]

  val empty            : t
  val of_alist         : (key * value) list -> t
  val to_alist         : t -> (key * value) list
  val of_aarray        : (key * value) array -> t
  val of_sorted_aarray : (key * value) array -> t
  val of_hashtbl       : (key, value) Hashtbl.t -> t

  val find  : t -> key -> value option
  val mem   : t -> key -> bool
  val iteri : t -> f:(key:key -> data:value -> unit) -> unit
  val fold  : t -> init:'acc -> f:(key:key -> data:value -> 'acc -> 'acc) -> 'acc
end

(* TODO How should we handle duplicate keys? *)
module Make (K : Key) (V : Value) : S with type key := K.t and type value := V.t = struct
  type key = K.t [@@deriving sexp, bin_io]
  type value = V.t [@@deriving sexp, bin_io]

  module T = Packed_array.Tuple2 (struct
    include K.Packed_array
    type elt = K.t
  end) (struct
    include V.Packed_array
    type elt = V.t
  end)

  type t = T.t [@@deriving sexp, bin_io]

  let empty = T.empty

  exception Duplicate_key of key [@@deriving sexp]
  exception Unsorted_array of (key * value) array [@@deriving sexp]

  let fst (a, _) = a
  let cmp a b = K.compare (fst a) (fst b)

  let duplicate_key_sorted' arr =
    let len = Array.length arr in
    if len < 2
    then None
    else
      with_return (fun r ->
        for i = 0 to len - 2 do
          if K.equal (fst (Array.unsafe_get arr i)) (fst (Array.unsafe_get arr (i+1)))
          then r.return (Some (fst (Array.unsafe_get arr i)))
        done;
        None)

  let duplicate_key_sorted arr =
    if not (Array.is_sorted arr ~compare:cmp) then raise (Unsorted_array arr);
    duplicate_key_sorted' arr

  let of_sorted_aarray' = T.of_array

  let of_sorted_aarray arr =
    match duplicate_key_sorted arr with
    | None -> of_sorted_aarray' arr
    | Some k -> raise (Duplicate_key k)

  let of_aarray arr = of_sorted_aarray (Array.sorted_copy arr ~compare:cmp)

  let destructive_of_aarray arr =
    Array.sort arr ~compare:cmp;
    match duplicate_key_sorted' arr with
    | None -> of_sorted_aarray' arr
    | Some k -> raise (Duplicate_key k)

  let of_alist kvs = destructive_of_aarray (Array.of_list kvs)

  let to_alist = T.to_list

  exception Bottom of int;;

  let sort ~cmp a =
    let get = Array.unsafe_get in
    let set = Array.unsafe_set in
    let maxchild l i =
      let i31 = i+i+i+1 in
      let x = ref i31 in
      if i31+2 < l then begin
        if cmp (get a i31) (get a (i31+1)) < 0 then x := i31+1;
        if cmp (get a !x) (get a (i31+2)) < 0 then x := i31+2;
        !x
      end else
        if i31+1 < l && cmp (get a i31) (get a (i31+1)) < 0
        then i31+1
        else if i31 < l then i31 else raise (Bottom i)
    in
    let rec trickledown l i e =
      let j = maxchild l i in
      if cmp (get a j) e > 0 then begin
        set a i (get a j);
        trickledown l j e;
      end else begin
        set a i e;
      end;
    in
    let trickle l i e = try trickledown l i e with Bottom i -> set a i e in
    let rec bubbledown l i =
      let j = maxchild l i in
      set a i (get a j);
      bubbledown l j
    in
    let bubble l i = try bubbledown l i with Bottom i -> i in
    let rec trickleup i e =
      let parent = (i - 1) / 3 in
      assert (i <> parent);
      if cmp (get a parent) e < 0 then begin
        set a i (get a parent);
        if parent > 0 then trickleup parent e else set a 0 e;
      end else begin
        set a i e;
      end;
    in
    let l = Array.length a in
    for i = (l + 1) / 3 - 1 downto 0 do trickle l i (get a i); done;
    for i = l - 1 downto 2 do
      let e = (get a i) in
      set a i (get a 0);
      trickleup (bubble i 0) e;
    done;
    if l > 1 then (let e = (get a 1) in set a 1 (get a 0); set a 0 e);
  ;;

  let of_hashtbl tbl =
    let len = Hashtbl.length tbl in
    let arr =
      let x = ref None in
      ignore (Hashtbl.existsi tbl ~f:(fun ~key ~data ->
        x := Some (key, data);
        true));
      match !x with
      | None -> [||]
      | Some kv -> Array.create ~len kv
    in
    let ix = ref 0 in
    Hashtbl.iteri tbl ~f:(fun ~key ~data ->
      let i = !ix in
      Array.unsafe_set arr i (key, data);
      ix := i + 1);
    sort arr ~cmp;
    of_sorted_aarray' arr

  let binary_search keys k =
    let rec binary_search ~low_inclusive ~high_inclusive =
      match compare low_inclusive high_inclusive with
      |  1 -> None
      |  0 ->
        Option.some_if (K.equal k (K.Packed_array.unsafe_get keys low_inclusive))
          low_inclusive
      | -1 ->
        let ix = (low_inclusive + high_inclusive) / 2 in
        let k' = K.Packed_array.unsafe_get keys ix in
        begin match K.compare k k' with
        | -1 -> binary_search ~low_inclusive ~high_inclusive:(ix-1)
        |  0 -> Some ix
        |  1 -> binary_search ~low_inclusive:(ix+1) ~high_inclusive
        |  _ -> assert false
        end
      |  _ -> assert false
    in
    binary_search ~low_inclusive:0 ~high_inclusive:(K.Packed_array.length keys - 1)

  let find t k =
    let keys, values = T.unzip t in
    let ix = binary_search keys k in
    Option.map ix ~f:(fun ix -> V.Packed_array.unsafe_get values ix)

  let mem t k =
    let keys, _ = T.unzip t in
    Option.is_some (binary_search keys k)

  let iteri t ~f = T.iter t ~f:(fun (key, data) -> f ~key ~data)

  let fold t ~init ~f = T.fold t ~init ~f:(fun acc (key, data) -> f ~key ~data acc)
end

let%test_module "string->int" = (module struct
  module K = struct
    include String
    module Packed_array = (Packed_array.String : Packed_array.S with type elt := t)
  end

  module V = struct
    include Int
    module Packed_array = (Packed_array.Int : Packed_array.S with type elt := t)
  end

  module M = (Make(K)(V) : S with type key := string and type value := int)

  let keys = List.init 99 ~f:(fun ix ->
    let ix = if ix > 72 then ix+1 else ix in
    Int.to_string ix)
  let values = List.init 99 ~f:(fun ix -> if ix > 72 then ix+1 else ix)
  let alist = List.zip_exn keys values
  let packed_map = M.of_alist alist

  let%test "find  (present)" =
    List.for_all keys ~f:(fun k ->
      match M.find packed_map k with
      | None -> false
      | Some v -> List.exists alist ~f:(fun (k', v') -> k = k' && v = v'))

  let%test "find  (too low)" =
    assert ('/' < '0');
    match M.find packed_map "/foo" with
    | None -> true
    | Some _ -> false

  let%test "find  (too high)" =
    assert ('f' > '9');
    not (M.mem packed_map "foo")

  let%test "find  (not present)" = not (M.mem packed_map "73")

  let%test "find  (empty)" = not (M.mem M.empty "foo")
end)
