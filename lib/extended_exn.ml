open Core.Std

let to_string = function
  | Failure s -> "Failure: " ^ s
  | e -> Caml.Printexc.to_string e

let to_string_hum = function
  | Failure s ->  s
  | e -> Caml.Printexc.to_string e

let rec unwrap = function
  | Finally (e,_) -> unwrap e
  | e -> e

module Exn_string = struct
  module T = struct
    type t = string with sexp, bin_io
  end
  include T
  include Sexpable.To_stringable (T)

  let of_exn exn = Exn.to_string exn
end
