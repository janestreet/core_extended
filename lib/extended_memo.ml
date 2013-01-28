open Core.Std

let general_rec g =
  let fref = ref (fun _ -> assert false) in
  let f = Memo.general (fun x -> g !fref x) in
  fref := f;
  f
;;

