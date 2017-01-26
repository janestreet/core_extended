open Core

module Access_control = struct
  type ('a,'b,'z) any = ('a,'b) Hashtbl.Poly.t [@@deriving sexp, bin_io]
  module Immutable = struct
    type ('a,'b) t = ('a,'b,immutable) any [@@deriving sexp, bin_io]
  end
  module Read_only = struct
    type ('a,'b) t = ('a,'b,read) any [@@deriving sexp, bin_io]
  end
  module Read_write = struct
    type ('a,'b) t = ('a,'b,read_write) any [@@deriving sexp, bin_io]
  end
  let of_hashtbl = Fn.id
  include Hashtbl
end
