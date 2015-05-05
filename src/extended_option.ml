
let rec of_list = function
  | [] -> None
  | (Some _ as x) :: _ -> x
  | _ :: xs -> of_list xs

let value_raise t ~exn =
  match t with
  | Some x -> x
  | None -> raise exn
