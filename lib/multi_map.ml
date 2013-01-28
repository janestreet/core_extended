open Core.Std

include Fold_map.Make2_sexpable
(struct
   type 'a data = 'a
   type 'a t = 'a list with sexp
   let init = []
   let f list x = x :: list
 end)


let iter ~f m =
  iter ~f:(fun ~key ~data -> List.iter data ~f:(fun data -> f ~key ~data)) m

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

let filter ~f m =
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

let reduce ~f m = Map.map ~f (to_map m)
