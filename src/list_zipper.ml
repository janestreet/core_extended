open Core.Std

type 'a t = {
  l : 'a list;
  r : 'a list
}

let create l r = {
  l = List.rev l;
  r = r
}

let drop_before = function
  | {l = []; r= _} ->  None
  | {l = h::t ; r = r } -> Some (h,{l=t;r=r})

let drop_after = function
  | { l = _; r = [] } -> None
  | { l = l ; r = h::t } -> Some (h,{ l=l; r=t })

let drop_all_before = function
  | {l = []; r= _} ->  None
  | {l = l; r = r } -> Some (l,{l=[];r=r})

let drop_all_after = function
  | { l = _; r = [] } -> None
  | { l = l; r = r } -> Some (r,{ l=l; r=[] })

let insert_before z v = {z with l = v::z.l}

let insert_after z v = {z with r = v::z.r}

let previous zip =
  match drop_before zip with
  | None -> None
  | Some (e,line) -> Some (insert_after line e)

let next zip =
  match drop_after zip with
  | None -> None
  | Some (e,line) -> Some (insert_before line e)

let replace_left z l = {z with l}
let replace_right z r = {z with r}
