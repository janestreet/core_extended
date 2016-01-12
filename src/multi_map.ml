open Core.Std

include Fold_map.Make2_sexpable
(struct
   type 'a t = 'a list [@@deriving sexp]
   let init = []
   let f list x = x :: list
 end)


let iteri ~f m =
  iteri ~f:(fun ~key ~data -> List.iter data ~f:(fun data -> f ~key ~data)) m

(* DEPRECATED - leaving here for a little while so as to ease the transition for
   external core users. (But marking as deprecated in the mli *)
let iter = iteri

let mapi ~f m =
  of_map (Map.mapi (to_map m)
             ~f:(fun ~key ~data -> List.map ~f:(fun data -> f ~key ~data) data))

let map ~f m =
  of_map (Map.map (to_map m) ~f:(List.map ~f))

let fold ~f m ~init =
  fold m
    ~f:(fun ~key ~data acc ->
      List.fold data
        ~f:(fun acc data -> f ~key ~data acc)
        ~init:acc)
    ~init

let set ~key ~data m =
  if data=[] then
    remove m key
  else
    set ~key ~data m

let filteri ~f m =
  of_map
    (Map.filter_mapi (to_map m)
        ~f:(fun ~key ~data ->
          let data = List.filter data
            ~f:(fun data -> f ~key ~data)
          in
          if data = [] then
            None
          else
            Some data))

(* DEPRECATED - leaving here for a little while so as to ease the transition for
   external core users. (But marking as deprecated in the mli *)
let filter = filteri

let reduce ~f m = Map.map ~f (to_map m)
