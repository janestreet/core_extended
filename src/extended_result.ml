open Core
module Ok = Result

module Error = struct
  module T = struct
    type ('a,'b) t = ('b,'a) Result.t
  end
  include T
  include Monad.Make2
    (struct
      include T

      let bind x ~f = match x with
        | Error x -> f x
        | Ok _ as x -> x

      let map x ~f = match x with
        | Error x -> Error (f x)
        | Ok _ as x -> x

      let map = `Custom map

      let return x = Error x
    end)
end

module Exn = struct
  module T = struct
    type 'a t = ('a, exn) Result.t [@@deriving sexp_of]
  end
  include T

  include Monad.Make (struct
    include T

    let return x = Ok x

    let bind (t : 'a t) ~f =
      match t with
      | Ok x -> f x
      | Error e -> Error e
    ;;

    let map (t : 'a t) ~f =
      match t with
      | Ok x -> Ok (f x)
      | Error e -> Error e
    ;;

    let map = `Custom map
  end)

  let ok = function
    | Ok a -> a
    | Error exn -> raise exn
  ;;
end
