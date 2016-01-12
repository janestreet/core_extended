type 'k t =
  | Inc of 'k
  | Exc of 'k
[@@deriving sexp]

let key_of = function
  | Inc k -> k
  | Exc k -> k

let compare compare_k t1 t2 =
  match compare_k (key_of t1) (key_of t2) with
  | 0 ->  begin
      match t1, t2 with
      | Inc _, Inc _
      | Exc _, Exc _ -> 0
      | Inc _, Exc _ -> -1
      | Exc _, Inc _ -> 1
    end
  | n -> n
