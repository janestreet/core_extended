open Core.Std

module Access_control = struct
  type ('a,'b,'z) any = ('a,'b) Hashtbl.Poly.t with sexp, bin_io
  module Immutable = struct
    type ('a,'b) t = ('a,'b,immutable) any with sexp, bin_io
  end
  module Read_only = struct
    type ('a,'b) t = ('a,'b,read_only) any with sexp, bin_io
  end
  module Read_write = struct
    type ('a,'b) t = ('a,'b,read_write) any with sexp, bin_io
  end
  let of_hashtbl = Fn.id
  include Hashtbl
end
