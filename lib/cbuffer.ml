open Core.Std


type 'a t = { mutable data: 'a array; (** base of circular buffer *)
              mutable start: int;  (** first position at which data is found *)
              mutable length: int; (** number of elements in buffer *)
              never_shrink: bool; (** whether to refrain from shrinking the buffer *)
              dummy: 'a; (** value used to pack into newly allocated arrays *)
            }
with sexp

let create ?(never_shrink=false) dummy length =
  { data = Array.create (Int.max 10 length) dummy;
    start = 0;
    length = 0;
    never_shrink = never_shrink;
    dummy = dummy;
  }

let length buf = buf.length

let phys_length buf = Array.length buf.data

let is_full buf = buf.length >= Array.length buf.data

let check_index fname buf i =
  if i < 0 || i >= buf.length then
    invalid_arg (Printf.sprintf "Cbuffer.%s: index %i is not between 0 and %d"
                   fname i (buf.length - 1))

let get buf i =
  check_index "get" buf i;
  buf.data.((buf.start + i) mod phys_length buf)

let set buf i v =
  check_index "set" buf i;
  buf.data.((buf.start + i) mod phys_length buf) <- v

let copy_data ~src ~dst start length =
  if start + length <= Array.length src
  then Array.blit ~src ~dst ~src_pos:start ~dst_pos:0 ~len:length
  else
    let length1 = Array.length src - start in
    let length2 = length - length1 in
    Array.blit ~src ~dst ~src_pos:start ~dst_pos:0 ~len:length1;
    Array.blit ~src ~dst ~src_pos:0 ~dst_pos:length1 ~len:length2

(* [swap_array buf len] copies the contents of [buf] to a new array of length [len] and
   places that new data into the buffer *)
let swap_array buf new_length =
  let newdata = Array.create new_length buf.dummy in
  copy_data  ~src:buf.data ~dst:newdata buf.start buf.length;
  buf.data <- newdata;
  buf.start <- 0

(** double the size of the buffer *)
let expand buf = swap_array buf (phys_length buf * 2)

(** half the size of the buffer *)
let shrink buf =
  if buf.length > phys_length buf / 2 then
    invalid_arg (Printf.sprintf "Cbuffer.shrink: buffer is too big (%d > %d)"
                   buf.length (phys_length buf / 2))
  else
    swap_array buf (phys_length buf / 2)

let to_array buf =
  let ar = Array.create buf.length buf.dummy in
  copy_data ~src:buf.data ~dst:ar buf.start buf.length;
  ar

let add buf v =
  if is_full buf then expand buf;
  if buf.start = 0 then
    buf.start <- phys_length buf - 1
  else buf.start <- buf.start - 1;
  buf.data.(buf.start) <- v;
  buf.length <- buf.length + 1

let drop_from buf i =
  check_index "drop_from" buf i;
  for j = i to buf.length - 1 do
    set buf j buf.dummy
  done;
  buf.length <- i;
  if not buf.never_shrink && buf.length < phys_length buf / 4
  then shrink buf

let drop_last buf =
  drop_from buf (buf.length - 1)

(****** not well code-reviewed from here down ******)
let iter buf ~f =
  for i = 0 to (length buf) - 1 do
    f i (get buf i);
  done

let iterr buf ~f =
  for i = (length buf) - 1 downto 0 do
    f i (get buf i);
  done

let of_array arr =
  let len = Array.length arr in
  if len = 0 then invalid_arg "Cbuffer.of_array: empty array argument";
  let buf = create arr.(0) len in
  (* we do not have access to buf.data here -- reimplement blit! *)
  (* Array.blit ~src:arr ~dst:buf.data ~src_pos:0 ~dst_pos:0 ~len; *)
  for i =  len - 1 downto 0 do add buf arr.(i) done;
  buf

let rec cb_compare ~f ~b1 ~b2 ~s1 ~s2 ~n =
  if n = 0 then true
  else if (length b1) - s1 < n || (length b2) - s2 < n then false
  else if f (get b1 s1) (get b2 s2)
  then cb_compare ~f ~b1 ~b2 ~s1:(s1+1) ~s2:(s2+1) ~n:(n-1)
  else false

TEST_MODULE "compare" = struct
  let cb1 = of_array [|0;1;2;3;4|]
  let cb2 = of_array [|0;1;2;2;2|]

  TEST "yes" = cb_compare ~f:(=) ~b1:cb1 ~b2:cb2 ~s1:0 ~s2:0 ~n:3
  TEST "no1" = not (cb_compare ~f:(=) ~b1:cb1 ~b2:cb2 ~s1:0 ~s2:0 ~n:4)
  TEST "no2"= not (cb_compare ~f:(=) ~b1:cb1 ~b2:cb2 ~s1:1 ~s2:0 ~n:3)
end

let drop_old ?(cmp = compare) ?free ~f ~cutoff buf =
  (* should be ?(f=ident), but then ocaml thinks that f is 'a -> 'a *)
  let len = length buf in
  let last = ref len in
  while !last > 0 && cmp (f (get buf (!last - 1))) cutoff <= 0 do
    decr last;
    Option.iter free ~f:(fun f -> f (get buf !last));
  done;
  if !last < len then drop_from buf !last;
  len - !last                   (* number of objects dropped *)


(* This is a bit of a hacky test module because it's a straightforward copy of
   the  one that was in lib_test *)
TEST_MODULE = struct
  let (@?) = OUnit.(@?)

  TEST "mapping" =
    let cb = of_array [|0;1;2;3;4|] in
    let bu = Buffer.create 10 in
    let out n x = bprintf bu "(%d %d)" n x in
    iter cb ~f:out;
    "map" @? (Buffer.contents bu = "(0 0)(1 1)(2 2)(3 3)(4 4)");
    Buffer.clear bu;
    iterr cb ~f:out;
    "mapr" @? (Buffer.contents bu = "(4 4)(3 3)(2 2)(1 1)(0 0)");
    true

  TEST "drop_old" =
    let cb = of_array [|4;3;2;1;0|] in
    let list = ref [] in
    let free obj = list := obj::!list in
    "ret-1" @? (drop_old ~cutoff:(-1) ~free cb ~f:ident = 0);
    "buf-1" @? (to_array cb = [|4;3;2;1;0|]);
    "free-1" @? (!list = []);
    "ret0" @? (drop_old ~cutoff:0 ~free cb ~f:ident = 1);
    "buf0" @? (to_array cb = [|4;3;2;1|]);
    "free0" @? (!list = [0]);
    "ret1" @? (drop_old ~cutoff:1 ~free cb ~f:ident = 1);
    "buf1" @? (to_array cb = [|4;3;2|]);
    "free1" @? (!list = [1;0]);
    "ret2" @? (drop_old ~cutoff:2 ~free cb ~f:ident = 1);
    "buf2" @? (to_array cb = [|4;3|]);
    "free2" @? (!list = [2;1;0]);
    "ret3" @? (drop_old ~cutoff:3 ~free cb ~f:ident = 1);
    "buf3" @? (to_array cb = [|4|]);
    "free3" @? (!list = [3;2;1;0]);
    "ret4" @? (drop_old ~cutoff:4 ~free cb ~f:ident = 1);
    "buf4" @? (to_array cb = [||]);
    "free4" @? (!list = [4;3;2;1;0]);
    true

   TEST "drop_old2" =
    let cb = of_array [|4;3;2;1;0|] in
    "ret5" @? (drop_old ~cutoff:5 cb ~f:ident = 5);
    "buf5" @? (to_array cb = [||]);
    true
end
