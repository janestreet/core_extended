
open Core

module Raw = struct

  (*
     Invariants:
     1. Unions are right-associative, with at most one Set as top left child.
     2. Inters are right-associative, with at most one Set as top left child.
     3. Diffs have at most one Set child.
     4. Unions, Diffs, and Inters do not have empty Sets as children.
  *)

  type ('base, 'set) t =
    | Base  of 'base
    | Set   of 'set
    | Union of ('base, 'set) t * ('base, 'set) t
    | Inter of ('base, 'set) t * ('base, 'set) t
    | Diff  of ('base, 'set) t * ('base, 'set) t
  [@@deriving bin_io, compare]

end

open Raw

type ('base, 'elt, 'cmp) t = ('base, ('elt, 'cmp) Set.t) Raw.t [@@deriving compare]

(****************************************
   Constructors
 ****************************************)

let base b = Base b
let set s = Set s

let u s1 s2 = Set (Set.union s1 s2)
let i s1 s2 = Set (Set.inter s1 s2)
let d s1 s2 = Set (Set.diff s1 s2)

let rec union2 t1 t2 =
  match t1, t2 with

  | Union (Set s1, t1), Union (Set s2, t2)
    -> union2 (u s1 s2) (union2 t1 t2)

  | Union (Set s1, t), Set s2
  | Set s1, Union (Set s2, t)
    -> union2 (u s1 s2) t

  | Set s1, Set s2
    -> u s1 s2

  | Union (Set s, t1), t2
  | t1, Union (Set s, t2)
    -> union2 (Set s) (union2 t1 t2)

  | Set s, t
    when Set.is_empty s -> t

  | t, Set s
    when Set.is_empty s -> t

  | Set s, t
  | t, Set s
    -> Union (Set s, t)

  | Union (t1, t2), t3
    -> union2 t1 (union2 t2 t3)

  | t1, t2
    -> Union (t1, t2)

let rec inter2 t1 t2 =
  match t1, t2 with

  | Inter (Set s1, t1), Inter (Set s2, t2)
    -> inter2 (i s1 s2) (inter2 t1 t2)

  | Inter (Set s1, t), Set s2
  | Set s1, Inter (Set s2, t)
    -> inter2 (i s1 s2) t

  | Set s1, Set s2
    -> i s1 s2

  | Inter (Set s, t1), t2
    -> inter2 (Set s) (inter2 t1 t2)

  | t1, Inter (Set s, t2)
    -> inter2 (Set s) (inter2 t1 t2)

  | Set s, _
    when Set.is_empty s -> Set s

  | _, Set s
    when Set.is_empty s -> Set s

  | Set s, t
  | t, Set s
    -> Inter (Set s, t)

  | Inter (t1, t2), t3
    -> inter2 t1 (inter2 t2 t3)

  | t1, t2
    -> Inter (t1, t2)

let rec diff t1 t2 =
  match t1, t2 with

  (*
     The next three clauses do not enforce invariants,
     but they do allow us to simplify the term by
     precomputing the difference of two concrete sets.
  *)

  | Set s1, Union (Set s2, t)
    -> diff (d s1 s2) t

  | Inter (Set s1, t), Set s2
    -> inter2 (d s1 s2) t

  | Inter (Set s1, t1), Union (Set s2, t2)
    -> diff (inter2 (d s1 s2) t1) t2

  (*
     The next clause does not enforce an invariant,
     and does not necessarily simplify the term.
     However, it does turn nested diff into union,
     and union has more ways it can simplify.
  *)

  | Diff (t1, t2), t3
    -> diff t1 (union2 t2 t3)

  | Set s1, Set s2
    -> d s1 s2

  | Set s, _
    when Set.is_empty s -> Set s

  | t, Set s
    when Set.is_empty s -> t

  | t1, t2
    -> Diff (t1, t2)

let union (t, ts) = List.fold ts ~init:t ~f:union2
let inter (t, ts) = List.fold ts ~init:t ~f:inter2

let union_list = function
  | [] -> Or_error.error_string "Set_lang.union_list: empty list is not allowed"
  | t::ts -> Or_error.return (union (t,ts))

let inter_list = function
  | [] -> Or_error.error_string "Set_lang.inter_list: empty list is not allowed"
  | t::ts -> Or_error.return (inter (t,ts))

let union_list_exn ts = Or_error.ok_exn (union_list ts)
let inter_list_exn ts = Or_error.ok_exn (inter_list ts)

let make_union empty ts =
  match ts with
  | [] -> set empty
  | t::ts -> union (t,ts)

(****************************************
   Invariant
 ****************************************)

let rec invariant = function

  (* Union, Inter, and Diff do not have empty Set as child *)
  | Union (Set s, _)
  | Inter (Set s, _)
  | Diff  (Set s, _)
    when Set.is_empty s
    -> assert false

  | Union (_, Set s)
  | Inter (_, Set s)
  | Diff  (_, Set s)
    when Set.is_empty s
    -> assert false

  (* Union and Inter are right-associative *)
  | Union (Union (_,_), _)
  | Inter (Inter (_,_), _)
    -> assert false

  (* Union and Inter only have Set as left child *)
  | Union (_, Set _)
  | Inter (_, Set _)
    -> assert false

  (* Union and Inter only have Set as top child *)
  | Union (_, Union (Set _,_))
  | Inter (_, Inter (Set _,_))
    -> assert false

  (* Diff has at most one Set as child *)
  | Diff (Set _, Set _)
    -> assert false

  | Union (t1,t2)
  | Inter (t1,t2)
  | Diff  (t1,t2)
    -> invariant t1; invariant t2

  | Set _
  | Base _
    -> ()

(****************************************
   Accessors
 ****************************************)

let rec values_acc t acc =
  match t with
  | Base b -> b :: acc
  | Set  _ -> acc
  | Union (t1, t2) -> values_acc t1 (values_acc t2 acc)
  | Inter (t1, t2) -> values_acc t1 (values_acc t2 acc)
  | Diff  (t1, t2) -> values_acc t1 (values_acc t2 acc)

let values t = values_acc t []

let constant_value = function
  | Set s -> Some s
  | _ -> None

(****************************************
   Sexp Conversion
 ****************************************)

let rec gather_union = function
  | Union (t1, t2) -> t1 :: gather_union t2
  | t -> [t]

let rec gather_inter = function
  | Inter (t1, t2) -> t1 :: gather_inter t2
  | t -> [t]

let sexp_of_t sexp_of_base sexp_of_set =
  let rec loop = function
    | Base b -> sexp_of_base b
    | Union (t1, t2) ->
      Sexp.List
        (Sexp.Atom "union"
         :: loop t1
         :: List.map (gather_union t2) ~f:loop)
    | Inter (t1, t2) ->
      Sexp.List
        (Sexp.Atom "inter"
         :: loop t1
         :: List.map (gather_inter t2) ~f:loop)
    | Diff (t1, t2) ->
      Sexp.List [ Sexp.Atom "diff"; loop t1; loop t2 ]
    | Set s ->
      Sexp.List [Sexp.Atom "set" ; sexp_of_set s ]
  in
  loop

let is_tag str1 str2 =
  String.lowercase str1 = String.lowercase str2

let make_t_of_sexp ~module_name ~union base_of_sexp set_of_sexp =
  let type_name = "Set_lang." ^ module_name ^ ".t" in
  let rec loop sexp0 =
    match sexp0 with
    | Sexp.List (Sexp.Atom tag :: sexps) when is_tag tag "union" ->
      union (List.map sexps ~f:loop)
    | Sexp.List (Sexp.Atom tag :: sexps) when is_tag tag "inter" ->
      (match sexps with
       | sexp1 :: rest -> inter (loop sexp1, List.map rest ~f:loop)
       | _ -> Sexplib.Conv_error.stag_incorrect_n_args type_name tag sexp0)
    | Sexp.List (Sexp.Atom tag :: sexps) when is_tag tag "diff" ->
      (match sexps with
       | [sexp1; sexp2] -> diff (loop sexp1) (loop sexp2)
       | _ -> Sexplib.Conv_error.stag_incorrect_n_args type_name tag sexp0)
    | Sexp.List (Sexp.Atom tag :: sexps) when is_tag tag "set" ->
      (match sexps with
       | [sexp] -> set (set_of_sexp sexp)
       | _ -> Sexplib.Conv_error.stag_incorrect_n_args type_name tag sexp0)
    | _ -> base (base_of_sexp sexp0)
  in
  loop

(****************************************
   Modules and Functors
 ****************************************)

module Make_monadic_eval( M : Monad.S ) = struct

  include M.Monad_infix

  let rec subst t ~f =
    match t with
    | Base b -> f b
    | (Set _) as t -> M.return t
    | Union (t1, t2) -> subst t1 ~f >>= fun t3 -> subst t2 ~f >>| fun t4 -> union2 t3 t4
    | Inter (t1, t2) -> subst t1 ~f >>= fun t3 -> subst t2 ~f >>| fun t4 -> inter2 t3 t4
    | Diff  (t1, t2) -> subst t1 ~f >>= fun t3 -> subst t2 ~f >>| fun t4 -> diff   t3 t4

  let map t ~f =
    subst t ~f:(fun b -> f b >>| base)

  let specialize t ~f =
    subst t ~f:(fun b -> f b >>| fun opt -> Option.value_map opt ~f:set ~default:(base b))

  let rec eval t ~f =
    match t with
    | Base b -> f b
    | Set s -> M.return s
    | Union (t1, t2) -> eval t1 ~f >>= fun s1 -> eval t2 ~f >>| fun s2 -> Set.union s1 s2
    | Inter (t1, t2) -> eval t1 ~f >>= fun s1 -> eval t2 ~f >>| fun s2 -> Set.inter s1 s2
    | Diff  (t1, t2) -> eval t1 ~f >>= fun s1 -> eval t2 ~f >>| fun s2 -> Set.diff  s1 s2

end

module Identity_basic = struct
  type 'a t = 'a
  let bind x ~f = f x
  let return x = x
  let map x ~f = f x
  let map = `Custom map
end

module Identity = struct
  include Identity_basic
  include Monad.Make(Identity_basic)
end

include Make_monadic_eval(Identity)

module type S = Set_lang_intf.S with module Raw := Raw
module type S_binable = Set_lang_intf.S_binable with module Raw := Raw

module Make (Elt : Comparable.S) = struct

  module Set = Elt.Set

  type 'base t = ('base, Set.t) Raw.t [@@deriving compare]

  let base = base
  let set = set
  let union2 = union2
  let inter2 = inter2
  let diff = diff
  let inter = inter
  let inter_list = inter_list
  let inter_list_exn = inter_list_exn

  let union ts = make_union Set.empty ts

  let sexp_of_t sexp_of_base =
    sexp_of_t sexp_of_base Set.sexp_of_t

  let t_of_sexp base_of_sexp =
    make_t_of_sexp
      ~module_name:"Make(_)"
      ~union
      base_of_sexp
      Set.t_of_sexp

  let values = values
  let constant_value = constant_value
  let map = map
  let subst = subst
  let specialize = specialize
  let eval = eval

  module Make_monadic_eval(M : Monad.S) = Make_monadic_eval(M)

end

module Make_binable (Elt : Comparable.S_binable) = struct

  module Set = Elt.Set

  type 'base t = ('base, Set.t) Raw.t [@@deriving bin_io, compare]

  let base = base
  let set = set
  let union2 = union2
  let inter2 = inter2
  let diff = diff
  let inter = inter
  let inter_list = inter_list
  let inter_list_exn = inter_list_exn

  let union ts = make_union Set.empty ts

  let sexp_of_t sexp_of_base =
    sexp_of_t sexp_of_base Set.sexp_of_t

  let t_of_sexp base_of_sexp =
    make_t_of_sexp
      ~module_name:"Make_binable(_)"
      ~union
      base_of_sexp
      Set.t_of_sexp

  let values = values
  let constant_value = constant_value
  let map = map
  let subst = subst
  let specialize = specialize
  let eval = eval

  module Make_monadic_eval(M : Monad.S) = Make_monadic_eval(M)

end

let%test_module "set lang" = (module struct

  module Set = Char.Set
  module Slang = Make(Char)

  let compare_slang = Slang.compare String.compare
  let sexp_of_slang = Slang.sexp_of_t sexp_of_string
  let slang_of_sexp = Slang.t_of_sexp string_of_sexp

  module Random = struct

    let prng =
      Random.State.make
        (String.to_list "In theory, it does not matter what is written here."
         |> Array.of_list
         |> Array.map ~f:Char.to_int)

    let int i () = Random.State.int prng i
    let elem xs () = List.nth_exn xs (int (List.length xs) ())
    let list ?(length=(int 6 ())) ~f () = List.init length ~f:(fun _ -> f ())

    let chars = ['a';'b';'c']

    let char () = elem chars ()
    let char_list () = list ~f:char ()
    let set () = Set.of_list (char_list ())
    let string () = String.of_char_list (char_list ())

  end

  let set_of_string str = Char.Set.of_list (String.to_list str)

  let eval_string str = set_of_string str
  let map_string str = String.rev str
  let subst_string str =
    if String.length str < 3
    then set (eval_string str)
    else base (map_string str)
  let specialize_string str =
    if String.length str < 3
    then Some (eval_string str)
    else None

  let eval_slang slang = eval slang ~f:eval_string
  let map_slang slang = map slang ~f:map_string
  let subst_slang slang = subst slang ~f:subst_string
  let specialize_slang slang = specialize slang ~f:specialize_string

  let cache = ref [(Char.Set.empty, set Char.Set.empty)]

  let set_and_slang () =
    let recur = Random.elem (!cache) in
    let recur2 () = recur(), recur() in
    let (set, slang) =
      Lazy.force
        (Random.elem
           [ lazy (let s = Random.set () in s, set s)
           ; lazy (let str = Random.string () in set_of_string str, base str)
           ; lazy (let (s1,l1),(s2,l2) = recur2() in Set.union s1 s2, Slang.union2 l1 l2)
           ; lazy (let (s1,l1),(s2,l2) = recur2() in Set.inter s1 s2, Slang.inter2 l1 l2)
           ; lazy (let (s1,l1),(s2,l2) = recur2() in Set.diff s1 s2, Slang.diff l1 l2)
           ; lazy (let s,l = recur() in s, map_slang l)
           ; lazy (let s,l = recur() in s, subst_slang l)
           ; lazy (let s,l = recur() in s, specialize_slang l)
           ]
           ())
    in
    cache := (set,slang) :: !cache;
    (set,slang)

  let test set slang =
    let sexp = sexp_of_slang slang in
    let test name bool =
      if bool
      then ()
      else failwithf "%s of:\n%s" name (Sexp.to_string_hum sexp) ()
    in
    test "invariant" (try invariant slang; true with _ -> false);
    test "eval" (0 = Set.compare set (eval_slang slang));
    test "map" (0 = Set.compare set (eval_slang (map_slang slang)));
    test "subst" (0 = Set.compare set (eval_slang (subst_slang slang)));
    test "specialize" (0 = Set.compare set (eval_slang (specialize_slang slang)));
    test "sexp" (0 = compare_slang slang (slang_of_sexp sexp))

  let%test_unit _ =
    for _ = 1 to 50 * 1000 do
      let s,l = set_and_slang() in test s l
    done

end)
