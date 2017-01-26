open Core

type t = [ `Prod | `Test ] [@@deriving sexp, bin_io, compare]

let is_prod t = t = `Prod
let is_test t = t = `Test

let of_prod_bool = function
  | true -> `Prod
  | false -> `Test

let of_test_bool = function
  | true -> `Test
  | false -> `Prod
