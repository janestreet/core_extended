open Core.Std
let rec count__loop cnt el = function
  | [] -> cnt
  | h::t when h = el -> count__loop (cnt + 1) el t
  | _::t -> count__loop cnt el t

(*
  [ count l el]
  Counts the occurences of [el] in [l]
*)
let count (l:'a list) (el:'a) : int = count__loop 0 el l

let rec number__loop seen acc = function
  | [] -> List.rev acc
  | h::t -> number__loop (h::seen) ((h,count seen h)::acc) t

let number (l:'a list) : ('a * int) list = number__loop [] [] l



let unnumber = List.map ~f:fst


type 'node graph = ('node * 'node list) list

let insert_edge (graph:'a graph) (node:'a) (child:'a) : 'a graph =
  let rec loop acc = function
    | [] -> (node,[child])::graph
    | (node',children)::l when node' = node ->
        if List.mem children child then
          graph
        else
          (node,(child::children))::(List.rev_append l acc)
    | h::t -> loop (h::acc) t
  in
  loop [] graph

let insert_node (graph:'a graph) (node:'a) : 'a graph =
  if List.Assoc.mem graph node then
    graph
  else
    (node,[]) :: graph

let children (graph:'a graph) (node:'a) : 'a list =
  List.Assoc.find_exn graph node

(** A topological sort that will degrade nicely in the presence of cycles. *)
let top_sort (graph:'a graph) : 'a list =
  let rec visit (dead,l) v =
    if List.mem dead v then
      (dead,l)
    else
      let dead,l =
        List.fold (children graph v)
          ~f:visit
          ~init:((v::dead),l)
      in
      dead,(v::l)
  in
  let _,l = List.fold graph
    ~f:(fun acc (node,_child) -> visit acc node)
    ~init:([],[])
  in
  l

let rec add_dep_list graph = function
  | []  -> graph
  | [node] -> insert_node graph node
  | node::((child::_) as l) -> add_dep_list (insert_edge graph node child) l

let multimerge_unique l =
  let graph = List.fold ~f:add_dep_list ~init:[] l in
  top_sort graph

let multimerge l =
  let l = List.map ~f:number l in
  unnumber (multimerge_unique l)
