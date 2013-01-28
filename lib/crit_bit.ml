open Core.Std

module type S = sig
  type key
  type 'data t

  val empty   : 'data t
  val mem     : 'data t -> key -> bool
  val find    : 'data t -> key -> 'data option
  val add     : 'data t -> key:key -> data:'data -> 'data t
  val remove  : 'data t -> key -> 'data t
  val iter    : 'data t -> f:(key:key -> data:'data -> unit) -> unit
  val map     : 'data t -> f:('data -> 'b) -> 'b t
  val fold    : 'data t -> init:'b -> f:(key:key -> data:'data -> 'b -> 'b) -> 'b
end

module type Bit_indexable = sig
  type t = string

  (* return the byte at the given position in the data structure t.  [get] must never
     raise an exception and must return 0 for all bytes past the end of the structure *)
  val get : t -> int -> int
  val length : t -> int
end

module Crit_bit (B : Bit_indexable) : (S with type key = B.t) = struct
  let get = B.get

  module Node = struct
    module Inner = struct
      type 'node t = {
        byte_pos : int;
        bit_mask : int;
        left     : 'node;
        right    : 'node;
      }
    end

    type 'a t =
      | Inner of 'a t Inner.t
      | Edge of B.t * 'a
  end

  type key = B.t
  type 'a t = 'a Node.t option

  let empty = None

  let rec best_mem t s =
    match t with
    | Node.Edge (k,v) -> (k,v)
    | Node.Inner node ->
      if get s node.Node.Inner.byte_pos land node.Node.Inner.bit_mask = 0 then
        best_mem node.Node.Inner.left s
      else
        best_mem node.Node.Inner.right s
  ;;

  (* returns 0 or 1, indicating direction *)
  (*let dir_test ~bit_mask c = ((c lor bit_mask) + 1) lsr 8*)
  let go_left ~bit_mask c = c land bit_mask = 0

  let find t k =
    match t with
    | None   -> None
    | Some t ->
      let (k',v) = best_mem t k in
      if k = k' then Some v else None
  ;;

  let mem t s = Option.is_some (find t s)

  let find_most_significant_bit i =
    if i land      0b10000000 > 0 then 0b10000000
    else if i land 0b01000000 > 0 then 0b01000000
    else if i land 0b00100000 > 0 then 0b00100000
    else if i land 0b00010000 > 0 then 0b00010000
    else if i land 0b00001000 > 0 then 0b00001000
    else if i land 0b00000100 > 0 then 0b00000100
    else if i land 0b00000010 > 0 then 0b00000010
    else 0b00000001
  ;;

  (* [find_crit_bit] returns a partially filled inner node containing the byte position
    and bit mask of the crit_bit difference between base and s *)
  let find_crit_bit ~base ~key ~data =
    let b_length = B.length base in
    let k_length = B.length key in
    let rec loop byte_pos =
      if byte_pos = b_length && byte_pos = k_length then None
      else begin
        let k_byte = get key byte_pos in
        let b_byte = get base byte_pos in
        if b_byte <> k_byte then
          let bit_mask = find_most_significant_bit (b_byte lxor k_byte) in
          let edge = Node.Edge (key, data) in
          let make_node n =
            Node.Inner
              (if go_left ~bit_mask b_byte then
                {Node.Inner.
                  byte_pos;
                  bit_mask;
                  left  = n;
                  right = edge }
              else
                {Node.Inner.
                  byte_pos;
                  bit_mask;
                  left  = edge;
                  right = n })
          in
          Some (byte_pos, bit_mask, make_node)
        else loop (byte_pos + 1)
      end
    in
    loop 0
  ;;

  let add t ~key ~data =
    match t with
    | None -> Some (Node.Edge (key, data))
    | Some node ->
      let base,_ = best_mem node key in
      match find_crit_bit ~base ~key ~data with
      | None -> t
      | Some (byte_pos, bit_mask, make_node) ->
        let rec loop node =
          match node with
          | Node.Edge _ -> make_node node
          | Node.Inner inner ->
            let next_byte_pos = inner.Node.Inner.byte_pos in
            let next_bit_mask = inner.Node.Inner.bit_mask in
            if   (byte_pos > next_byte_pos)
              || (next_byte_pos = byte_pos && next_bit_mask > bit_mask)
            then begin
              if
                go_left ~bit_mask:inner.Node.Inner.bit_mask
                  (get key inner.Node.Inner.byte_pos)
              then
                Node.Inner {inner with Node.Inner.left = loop inner.Node.Inner.left}
              else
                Node.Inner {inner with Node.Inner.right = loop inner.Node.Inner.right}
            end else
              make_node node
        in
        Some (loop node)
  ;;

  let remove t key =
    match t with
    | None      -> None
    | Some node ->
      let rec loop node =
        match node with
        | Node.Edge (k,_)  ->
          if k = key
          then None
          else raise Not_found
        | Node.Inner inner ->
          if go_left ~bit_mask:inner.Node.Inner.bit_mask
            (get key inner.Node.Inner.byte_pos)
          then begin
            match loop inner.Node.Inner.left with
            | None   -> Some inner.Node.Inner.right
            | Some n -> Some (Node.Inner {inner with Node.Inner.left = n})
          end
          else begin
            match loop inner.Node.Inner.right with
            | None   -> Some inner.Node.Inner.left
            | Some n -> Some (Node.Inner {inner with Node.Inner.right = n})
          end
      in
      try
        loop node
      with
      | Not_found -> t
  ;;

  let fold t ~init ~f =
    match t with
    | None -> init
    | Some node ->
      let rec loop acc node =
        match node with
        | Node.Edge (key,data) -> f ~key ~data init
        | Node.Inner inner     ->
          loop (loop acc inner.Node.Inner.left) inner.Node.Inner.right
      in
      loop init node
  ;;

  let iter t ~f = fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)

  let map t ~f =
    match t with
    | None -> None
    | Some node ->
      let rec loop node =
        match node with
        | Node.Edge (key,data) -> Node.Edge (key, f data)
        | Node.Inner inner ->
          let left  = loop inner.Node.Inner.left in
          let right = loop inner.Node.Inner.right in
          Node.Inner {Node.Inner.
            byte_pos = inner.Node.Inner.byte_pos;
            bit_mask = inner.Node.Inner.bit_mask;
            left;
            right;
          }
      in
      Some (loop node)
  ;;
end

include (Crit_bit (struct
  type t = string

  let get s pos =
    try
      int_of_char s.[pos]
    with
    | _ -> 0
  ;;

  let length = String.length
end))

module Test = struct
  let random_data () =
    let length = Random.int 60 + 1 in
    let s = String.create length in
    for i = 0 to length - 1 do
      s.[i] <- Char.of_int_exn (Random.int (126 - 47) + 47)
    done;
    Some s
  ;;

  let existing_data map =
    let data = Array.of_list (Map.keys map) in
    if Array.length data > 0 then
      Some (data.(Random.int (Array.length data)))
    else
      None
  ;;

  let random_action map =
    match Random.int 4 with
    | 0 -> `Insert (random_data ())
    | 1 -> `Insert (existing_data map)
    | 2 -> `Delete (random_data ())
    | 3 -> `Delete (existing_data map)
    | _ -> assert false
  ;;

  let test () =
    Random.init 769_305;
    let rec loop base_map crit_bit n =
      if n = 0 then ()
      else begin
        match random_action base_map with
        | `Delete None
        | `Insert None -> loop base_map crit_bit n
        | `Insert (Some key) ->
          let base_map = Map.add base_map ~key ~data:() in
          let crit_bit = add crit_bit ~key ~data:() in
          if (Map.find base_map key <> find crit_bit key) then
            failwithf "insertion of '%s' failed" key ();
          loop base_map crit_bit (n - 1)
        | `Delete (Some key) ->
          let base_map = Map.remove base_map key in
          let crit_bit = remove crit_bit key in
          if (Map.find base_map key <> find crit_bit key) then
            failwithf "deletion of '%s' failed (%b, %b)"
              key (Map.mem base_map key) (mem crit_bit key) ();
          loop base_map crit_bit (n - 1)
      end
    in
    try
      loop String.Map.empty empty 100_000;
      true
    with
    | _e -> false
  ;;

  TEST = test ()
end
