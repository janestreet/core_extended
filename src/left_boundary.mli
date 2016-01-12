(* This is the leftmost value of an interval. Inc / Exc determine if that value is
   actually included in the interval or not
*)
type 'k t =
  | Inc of 'k
  | Exc of 'k
[@@deriving sexp, compare]
