 open Core.Std

let ( <|> ) ar ij = if snd ij <= fst ij  then [||] else ar <|> ij

(* This is an implementation of the patience sorting algorithm as explained at
   http://en.wikipedia.org/wiki/Patience_sorting *)
module Patience = struct

  module type Tagged_array_elt = sig
    (* the significance is this type is just that, given some [Tagged.t]'s, the [value] field
       is the one you want to sort by, whereas the [tag] field is just some extra info. *)
    type 'a tag
    type 'a t =
        { value : 'a;
          tag : 'a tag option;
        }
  end

  module Pile = struct
    type 'a t = 'a Stack.t
    let create x = let t = Stack.create () in Stack.push t x; t
    let top t = Stack.top t |! Option.value_exn
    let put_on_top t x = Stack.push t x
  end

  module Piles = struct
    type 'a t = 'a Pile.t Dequeue.t

    (* If [dummy] is of type [a], [empty ~dummy] returns an empty thing with type
       [a Piles.t].  It doesn't matter what [dummy] actually is, only its type matters. *)
    let empty ~dummy = Dequeue.create ~never_shrink:true ~dummy:(Pile.create dummy) ()

    let get_ith_pile t i dir =
      let i =
        match dir with
        | `From_left -> Dequeue.front_index t + i
        | `From_right -> Dequeue.back_index t - i
      in
      try Some (Dequeue.get_exn t i) with _ -> None

    exception Exit
    let findi_from_left t ~f =
      let found = ref None in
      begin
        try Dequeue.iteri t ~f:(fun i pile ->
          if f pile then (found := Some (i,pile); raise Exit))
        with Exit -> ()
      end;
      !found

    let new_rightmost_pile t pile = Dequeue.push_back t pile
    let length t = Dequeue.back_index t
  end

  module Play_patience (Tagged:Tagged_array_elt) = struct

    (* version of Piles.find_i_from_left optimized for this specific application.
       It assumes that if pile1 is to the left of pile2 then (f pile1) implies
       (f pile2). *)
    let optimized_findi_from_left piles ~f ~prev =
      let (>>=) = Option.(>>=) in
      let (>>|) = Option.(>>|) in
      (* first see if any work *)
      let last_pile = Piles.get_ith_pile piles 0 `From_right in
      last_pile >>= fun last_pile ->
        if not (f last_pile) then None
        else
          (* see if the one after the previous one works *)
          let one_after_prev =
            prev >>= fun prev -> Piles.get_ith_pile piles (prev + 1) `From_left in
          if Option.value_map one_after_prev ~f ~default:false then
            prev >>= fun i -> one_after_prev >>| fun x -> (i,x)
          else begin
            (* do binary search *)
            try
              Search_foo.bsearch_opt ~f:(fun i ->
                let pile = Piles.get_ith_pile piles i `From_left |! Option.value_exn in
                if f pile then 1 else -1) ~low:0 ~high:(Piles.length piles)
              |! Option.map ~f:(fun i -> (i,Piles.get_ith_pile piles i `From_left |!
                     Option.value_exn))
            with _ -> None
          end
    ;;
    (* [play_patience ar ~get_tag] plays patience with the greedy algorithm as described
       in the Wikipedia article, taking [ar] to be the deck of cards.  It returns the
       resulting [Piles.t].  Before putting an element of [ar] in a pile, it tags it using
       [get_tag].  [get_tag] takes as its arguments the full [Piles.t] in its current
       state, and also the specific [Pile.t] that the element of [ar] is being added to.
    *)
    let play_patience ar ~get_tag =
      if Array.length ar = 0 then raise (Invalid_argument "Patience_diff.play_patience");
      let piles = Piles.empty ~dummy:{Tagged.value = ar.(0);tag = None} in
      let prev = ref None in
      Array.iter ar ~f:(fun x ->
        let pile_opt = optimized_findi_from_left piles ~prev:!prev ~f:(fun pile -> x < (Pile.top pile).Tagged.value) in
        let tagged_x = {Tagged.value = x;tag = get_tag ~pile_opt ~piles} in
        match pile_opt with
        | None -> begin
            prev := None;
            Piles.new_rightmost_pile piles (Pile.create tagged_x)
          end
        | Some (i,pile) -> begin
            prev := Some i;
            Pile.put_on_top pile tagged_x
          end);
      piles
  end

  module Backpointers = struct
    (* in the terminology of the Wikipedia article, this corresponds to a card together with its backpointers *)
    type 'a tag = 'a t
    and 'a t =
        { value : 'a;
          tag : 'a tag option}

    let to_list t =
      let rec to_list acc t =
        match t.tag with
        | None -> t.value::acc
        | Some t' -> to_list (t.value::acc) t'
      in
      to_list [] t
  end

  let longest_increasing_subsequence ar =
    if ar = [||] then [] else begin
      let module P = Play_patience (Backpointers) in
      let get_tag ~pile_opt ~piles =
        match pile_opt with
        | None -> Piles.get_ith_pile piles 0 `From_right |! Option.map ~f:Pile.top
        | Some (i,_pile) ->
            if i = 0 then None
            else Piles.get_ith_pile piles (i-1) `From_left |! Option.value_exn |! Pile.top |! Option.some
      in
      let piles = P.play_patience ar ~get_tag in
      Piles.get_ith_pile piles 0 `From_right |! Option.value_exn |! Pile.top |!
          Backpointers.to_list
    end
end

(* this seems simpler, same complexity, but might be a bit slower in practice
let longest_increasing_subsequence ar =
  let len = Array.length ar in
  if len <= 1 then
    Array.to_list ar
  else begin
    let maxlen = ref 0 in
    let m = Array.create (len + 1) (-1) in
    let pred = Array.create (len + 1) (-1) in
    for i = 0 to len - 1 do
      let p =
        match Search_foo.bsearch ~low:1 ~high:!maxlen ~f:(fun p -> snd(ar.(i)) - snd(ar.(p))) with
        | None -> 0
        | Some p -> p
      in
      pred.(i) <- m.(p);
      if (p = !maxlen) || (ar.(i) < ar.(p + 1)) then begin
        m.(p + 1) <- i;
        if (p + 1) > !maxlen then maxlen := p + 1;
      end;
    done;
    let rec loop ac p =
      if p = (-1) then ac
      else
        loop (ar.(p) :: ac) pred.(p)
    in
    loop [] m.(!maxlen)
  end
;;
*)

(* This is an implementation of the patience diff algorithm by Bram Cohen as seen in
   Bazaar version 1.14.1 *)

let unique_lcs (a,alo,ahi) (b,blo,bhi) =
  (* Create a hashtable which takes elements of a to their index in a iff they're unique *)
  let unique = Hashtbl.Poly.create () ~size:(min (ahi - alo) (bhi - blo)) in
  for x's_pos_in_a = alo to ahi - 1 do
    let x = a.(x's_pos_in_a) in
    match Hashtbl.find unique x with
    | None -> Hashtbl.replace unique ~key:x ~data:(`Unique_in_a x's_pos_in_a)
    | Some _ -> Hashtbl.replace unique ~key:x ~data:`Not_unique
  done;

  for x's_pos_in_b = blo to bhi - 1 do
    let x = b.(x's_pos_in_b) in
    Hashtbl.find unique x |! Option.iter ~f:(fun pos ->
      match pos with
      | `Not_unique -> ()
      | `Unique_in_a x's_pos_in_a ->
          Hashtbl.replace unique
            ~key:x ~data:(`Unique_in_a_b (x's_pos_in_a, x's_pos_in_b))
      | `Unique_in_a_b _ ->
          Hashtbl.replace unique ~key:x ~data:`Not_unique);
  done;
  let a_b =
    let unique = Hashtbl.filter_map unique ~f:(function
      | `Not_unique
      | `Unique_in_a _ -> None
      | `Unique_in_a_b pos_in_a_b -> Some pos_in_a_b)
    in
    let a_b = Array.of_list (Hashtbl.data unique) in
    Array.sort a_b ~cmp:(fun (_,posb1) (_,posb2) -> Int.compare posb1 posb2);
    a_b
  in
  Patience.longest_increasing_subsequence a_b

(* [matches a b] returns a list of pairs (i,j) such that a.(i) = b.(j) and such that
   the list is strictly increasing in both its first and second coordinates.

   This is done by first applying unique_lcs to find matches from a to b among those
   elements which are unique in both a and b, and then recursively applying [matches] to
   each subinterval determined by those matches.  The uniqueness requirement is waived
   for blocks of matching lines at the beginning or end.

   I couldn't figure out how to do this efficiently in a functional way, so
   this is pretty much a straight translation of the original Python code. *)
let matches ~compare a b =
  let matches_ref_length = ref 0 in
  let matches_ref = ref [] in
  let add_match m =
    incr matches_ref_length;
    matches_ref := m :: !matches_ref
  in
  let rec recurse_matches alo blo ahi bhi =
(*    printf "alo %d blo %d ahi %d bhi %d\n%!" alo blo ahi bhi; *)
    let old_length = !matches_ref_length in
    if not (alo >= ahi || blo >= bhi) then begin
      let last_a_pos = ref (alo - 1) in
      let last_b_pos = ref (blo - 1) in
      unique_lcs (a,alo,ahi) (b,blo,bhi)
      |! List.iter ~f:(fun (apos,bpos) ->
(*           printf "found apos %d bpos %d\n%!" apos bpos; *)
           if !last_a_pos + 1 <> apos || !last_b_pos + 1 <> bpos
           then begin
             (*printf "recurse last_a_pos %d last_b_pos %d\n%!" !last_a_pos !last_b_pos;*)
             recurse_matches (!last_a_pos + 1) (!last_b_pos + 1) apos bpos;
           end;
           last_a_pos := apos;
           last_b_pos := bpos;
           add_match (apos,bpos));
      if !matches_ref_length > old_length
      then recurse_matches (!last_a_pos+1) (!last_b_pos+1) ahi bhi
      else if (compare a.(alo) b.(blo) = 0)
      then
        begin
          let alo = ref alo in
          let blo = ref blo in
          while (!alo < ahi && !blo < bhi && (compare a.(!alo) b.(!blo) = 0)) do
            add_match (!alo,!blo);
            incr alo; incr blo;
          done;
          recurse_matches !alo !blo ahi bhi;
        end
      else if (compare a.(ahi - 1) b.(bhi - 1) = 0)
      then
        begin
          let nahi = ref (ahi - 1) in
          let nbhi = ref (bhi - 1) in
          while (!nahi > alo && !nbhi > blo && a.(!nahi-1) = b.(!nbhi - 1)) do
            decr nahi; decr nbhi;
          done;
          recurse_matches (!last_a_pos+1) (!last_b_pos+1) !nahi !nbhi;
          for i = 0 to (ahi - !nahi - 1) do
            add_match (!nahi + i,!nbhi + i)
          done;
        end
    end
  in
  recurse_matches 0 0 (Array.length a) (Array.length b);
  List.rev !matches_ref
;;

module Matching_block = struct
  type t = {
    mine_start  : int;
    other_start : int;
    length      : int;
  }
end

let collapse_sequences matches =
  let collapsed = ref [] in
  let start_a = ref None in
  let start_b = ref None in
  let length = ref 0 in
  List.iter matches ~f:(fun (i_a,i_b) ->
    begin
      match !start_a, !start_b with
      | Some start_a_val , Some start_b_val when
            (i_a = start_a_val + !length && i_b = start_b_val + !length) -> incr length
      | _ -> begin
          begin
            match !start_a, !start_b with
            | Some start_a_val, Some start_b_val ->
              let matching_block = {
                Matching_block.
                mine_start = start_a_val;
                other_start = start_b_val;
                length = !length;
              }
              in
              collapsed := matching_block::!collapsed
            | _ -> ()
          end;
          start_a := Some i_a;
          start_b := Some i_b;
          length := 1
        end
    end);
  begin
    match !start_a, !start_b with
    | Some start_a_val, Some start_b_val when !length <> 0 ->
      let matching_block = {
        Matching_block.
        mine_start = start_a_val;
        other_start = start_b_val;
        length = !length;
      }
      in
      collapsed := matching_block :: !collapsed

    | _ -> ()
  end;
  List.rev !collapsed

let get_matching_blocks ~transform ~compare ~mine ~other =
  let mine = Array.map mine ~f:transform in
  let other = Array.map other ~f:transform in
  let matches = matches ~compare mine other |! collapse_sequences in
  let last_match = {
    Matching_block.
    mine_start = Array.length mine;
    other_start = Array.length other;
    length = 0;
  }
  in
  List.append matches [last_match]

module Range = struct
  type 'a t =
      | Same of ('a * 'a) array
      | Old of 'a array
      | New of 'a array
      | Replace of 'a array * 'a array
      | Unified of 'a array
end

module Hunk = struct
  type 'a t = {
    mine_start: int;
    mine_size: int;
    other_start: int;
    other_size: int;
    ranges: 'a Range.t list;
  }

  (* Does the nitty gritty of turning indexes into
     line numbers and reversing the ranges, returning a nice new hunk *)
  let create mine_start mine_stop other_start other_stop ranges = {
    mine_start = mine_start + 1;
    mine_size = mine_stop - mine_start;
    other_start = other_start + 1;
    other_size = other_stop - other_start;
    ranges = List.rev ranges;
  }

  let all_same hunk =
    List.fold hunk.ranges ~init:true ~f:(fun flag range ->
      match flag, range with
      | true, Range.Same _ -> true
      |  _ -> false)
end

let get_ranges_rev ~transform ~compare ~mine ~other =
  let module R = Range in
  let module M = Matching_block in
  let rec aux matching_blocks i j l =
    match matching_blocks with
    | current_block :: remaining_blocks -> (
        let mine_index, other_index, size =
          current_block.M.mine_start, current_block.M.other_start,
          current_block.M.length
        in
        (* Throw away crossover matches *)
        if mine_index < i || other_index < j then
          aux remaining_blocks i j l
        else
          let range_opt = (
            if i < mine_index && j < other_index then
              let mine_range = mine <|> (i, mine_index) in
              let other_range = other <|> (j, other_index) in
              Some (R.Replace (mine_range, other_range))
            else if i < mine_index then
              let mine_range = mine <|> (i, mine_index) in
              Some (R.Old mine_range)
            else if j < other_index then
              let other_range = other <|> (j, other_index) in
              Some (R.New other_range)
            else None)
          in
          let l = match range_opt with
            | Some range -> range :: l
            | None -> l
          in
          let mine_stop, other_stop = mine_index + size, other_index + size in
          let l =
            if size = 0 then l
            else
              let mine_range = mine <|> (mine_index, mine_stop) in
              let other_range = other <|> (other_index, other_stop) in
              let range = Array.map2 mine_range other_range
                ~f:(fun x y -> (x, y)) in
              R.Same range :: l
          in
          aux remaining_blocks mine_stop other_stop l
      )
    | [] -> List.rev l
  in
  let matching_blocks = get_matching_blocks ~transform ~compare ~mine ~other in
  aux matching_blocks 0 0 []

(* Debug function.  Expose in the interface if needed. *)
let print_ranges hunk =
  let module R = Range in
  List.iter hunk.Hunk.ranges ~f:(fun r ->
    print_endline "";
    let s = match r with
      | R.Same ar -> sprintf "sam %i" (Array.length ar)
      | R.Old _ -> "old"
      | R.New ar -> sprintf "new %i" (Array.length ar)
      | R.Replace _ -> "rep"
      | R.Unified _ -> "uni"
    in
    printf "%s\n%!" s)

let all_same hunks =
  (* Empty hunks means no differences *)
  match hunks with
  | [] -> true
  | _ ->
      match List.filter hunks ~f:Hunk.all_same with
      | [] -> false
      | _ -> true

let get_hunks ~transform ~compare ~context ~mine ~other =
  let ranges = get_ranges_rev ~transform ~compare ~mine ~other in
  let module R = Range in
  let a = mine in
  let b = other in
  if context < 0 then
    let singleton_hunk =
      Hunk.create 0 (Array.length a) 0 (Array.length b) (List.rev ranges) in
    [singleton_hunk]
  else
  let rec aux ranges_remaining curr_ranges alo ahi blo bhi acc_hunks =
    match ranges_remaining with
    | [] ->
        (* Finish the last hunk *)
        let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
        (* Add it to the accumulator *)
        let acc_hunks = new_hunk :: acc_hunks in
        (* Finished! Return the accumulator *)
        List.rev acc_hunks
    | R.Same range :: [] ->
        (* If the last range is a Same, we might need to crop to context. *)
        let stop = min (Array.length range) (context) in
        let new_range = R.Same (range <|> (0, stop)) in
        let curr_ranges = new_range :: curr_ranges in
        (* Finish the current hunk *)
        let ahi = ahi + stop in
        let bhi = bhi + stop in
        let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
        (* Add it to the accumulator *)
        let acc_hunks = new_hunk :: acc_hunks in
        (* Finished! Return the accumulator *)
        List.rev acc_hunks
    | R.Same range :: rest ->
        let size = Array.length range in
        if size > context * 2 then (
          (* If this Same range is sufficiently large, split off a new hunk *)
          let new_range = R.Same (range <|> (0, context)) in
          let curr_ranges = new_range :: curr_ranges in
          (* Advance both hi's by context *)
          let ahi = ahi + context in
          let bhi = bhi + context in
          (* Finish the current hunk *)
          let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
          (* Add it to the accumulator *)
          let acc_hunks = new_hunk :: acc_hunks in
          (* Calculate ranges for the next hunk *)
          let alo = ahi + size - 2 * context in
          let ahi = alo in
          let blo = bhi + size - 2 * context in
          let bhi = blo in
          (* Push the remainder of the Equal range back onto the remaining_ranges *)
          let rest = R.Same (range <|> (size - context, size)) :: rest in
          aux rest [] alo ahi blo bhi acc_hunks
        ) else (
          (* Otherwise, this range is small enough that it qualifies as context for
             the both the previous and forthcoming range, so simply add it to
             curr_ranges untouched *)
          let curr_ranges = R.Same range :: curr_ranges in
          let ahi = ahi + size in
          let bhi = bhi + size in
          aux rest curr_ranges alo ahi blo bhi acc_hunks
        )
    | range :: rest ->
        (* Any range that isn't an Equal is important and not just context, so keep
           it in curr_ranges *)
        let curr_ranges = range :: curr_ranges in
        (* rest could be anything, so extract hunk_info from range *)
        let ahi, bhi =
          match range with
          | R.Same _ ->
              (* We eliminate the possibility of a Same above *)
              assert false
          | R.Unified _ ->
              (* get_ranges_rev never returns a Unified range *)
              assert false
          | R.New range ->
              let stop = bhi + (Array.length range) in
              (ahi, stop)
          | R.Old range ->
              let stop = ahi + (Array.length range) in
              (stop, bhi)
          | R.Replace (a_range, b_range) ->
              let mine_stop = ahi + Array.length a_range in
              let other_stop = bhi + Array.length b_range in
              (mine_stop, other_stop)
        in
        aux rest curr_ranges alo ahi blo bhi acc_hunks
  in
  let ranges, alo, ahi, blo, bhi =
    match ranges with
      (* If the first range is an Equal, shave off the front of the range, according to
         context.  Keep it on the ranges list so hunk construction can see where the range
         begins *)
    | R.Same range :: rest ->
        let stop = Array.length range in
        let start = max 0 (stop - context) in
        let new_range = R.Same (range <|> (start, stop)) in
        (new_range :: rest, start, start, start, start)
    | rest -> (rest, 0, 0, 0, 0)
  in
  aux ranges [] alo ahi blo bhi []

let concat_map_ranges hunks ~f =
  let f hunk =
    { hunk with
      Hunk.ranges = List.concat_map hunk.Hunk.ranges ~f;
    }
  in
  List.map hunks ~f

let unified hunks =
  let module R = Range in
  let f = function
    | R.Replace (l_range, r_range) -> [R.Old l_range; R.New r_range]
    | range -> [range]
  in
  concat_map_ranges hunks ~f


let old_only hunks =
  let module R = Range in
  let f = function
    | R.Replace (l_range, _) -> [R.Old l_range]
    | R.New _ -> []
    | range -> [range]
  in
  concat_map_ranges hunks ~f

let new_only hunks =
  let module R = Range in
  let f = function
    | R.Replace (_, r_range) -> [R.New r_range]
    | R.Old _ -> []
    | range -> [range]
  in
  concat_map_ranges hunks ~f

let ratio a b =
  (matches ~compare a b |! List.length |! ( * ) 2 |! float) /. (Array.length a + Array.length b |! float)

let collapse_multi_sequences matches =
  let collapsed = ref [] in
  if matches = [] then [] else
    let start = Array.create (List.length (List.hd_exn matches)) None in
    let length = ref 0 in
    List.iter matches ~f:(fun il ->
      begin
        if Array.for_all start ~f:Option.is_some && (
          List.mapi il ~f:(fun i x -> x = Option.value_exn start.(i) + !length)
        |! List.for_all ~f:(fun x -> x))
        then incr length
        else begin if Array.for_all start ~f:Option.is_some then
          collapsed := ((Array.map start ~f:Option.value_exn |! Array.to_list),
                       !length)::!collapsed;
        List.iteri il ~f:(fun i x -> start.(i) <- Some x);
        length := 1;
      end
    end);
    if Array.for_all start ~f:Option.is_some && !length <> 0 then
      collapsed := ((Array.map start ~f:Option.value_exn |! Array.to_list),!length) ::
        !collapsed;
    List.rev !collapsed

type 'a segment =
    | Same of 'a array
    | Different of 'a array array

type 'a merged_array = 'a segment list

let array_mapi2 ar1 ar2 ~f =
  Array.combine ar1 ar2
                       |! Array.mapi ~f:(fun i (x,y) -> f i x y)

let merge ar =
  if Array.length ar = 0 then [] else
    if Array.length ar = 1 then [Same ar.(0)] else
      let matches's = Array.map (ar <|> (1,(Array.length ar))) ~f:(matches ~compare ar.(0)) in
      let len = Array.length ar in
      let hashtbl = Hashtbl.Poly.create () ~size:0 in
      Array.iteri matches's ~f:(fun i matches ->
        List.iter matches ~f:(fun (a,b) ->
          match Hashtbl.find hashtbl a with
          | None -> Hashtbl.replace hashtbl ~key:a ~data:[(i,b)]
          | Some l -> Hashtbl.replace hashtbl ~key:a ~data:((i,b)::l)));
      let list =
        Hashtbl.to_alist hashtbl
        |! List.filter_map ~f:(fun (a,l) ->
          if List.length l = len - 1
          then Some (a::(List.sort l ~cmp:compare |! List.map ~f:snd))
          else None)
        |! List.sort ~cmp:compare
      in
      let matching_blocks = collapse_multi_sequences list in
      let last_pos = Array.create (Array.length ar) 0 in
      let merged_array = ref [] in
      List.iter matching_blocks ~f:(fun (l,len) ->
        let ar' = Array.of_list l in
        begin
          if last_pos <> ar'
          then merged_array :=
            (Different (array_mapi2 last_pos ar' ~f:(fun i n m -> ar.(i) <|>
                (n,m))))::!merged_array
        end;
        merged_array := Same (ar.(0) <|> (ar'.(0),ar'.(0) + len)):: !merged_array;
        Array.iteri last_pos ~f:(fun i _ -> last_pos.(i) <- ar'.(i) + len);
      );
      List.rev !merged_array
