
open! Core_kernel.Std
open! Janecheck.Std

TEST_MODULE "common case generation" = struct

  module Q = Janecheck
  module G = Generator
  module O = Observer

  TEST_MODULE "empty" = struct
    let cannot_generate gen =
      assert (does_raise (fun () ->
        Q.test_can_generate gen ~f:(fun _ -> true)))

    TEST_UNIT = cannot_generate G.empty
    TEST_UNIT = cannot_generate G.(filter unit ~f:(fun _ -> false))
    TEST_UNIT = cannot_generate G.(filter int  ~f:(fun _ -> false))
    TEST_UNIT = cannot_generate G.(recursive (fun self -> union [ self ; self ]))
  end

  TEST_MODULE "unit" = struct
    let sexp_of = Unit.sexp_of_t
    let by = `Compare Unit.compare
    let gen = G.unit
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun () -> true)
  end

  TEST_MODULE "bool" = struct
    let sexp_of = Bool.sexp_of_t
    let by = `Compare Bool.compare
    let gen = G.bool
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun x -> x = true)
    TEST_UNIT = can_generate (fun x -> x = false)
  end

  TEST_MODULE "int" = struct
    let sexp_of = Int.Hex.sexp_of_t
    let by = `Compare Int.compare
    let gen = G.int
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun x -> x = Int.max_value)
    TEST_UNIT = can_generate (fun x -> x = Int.min_value)
    TEST_UNIT = can_generate (fun x -> x = 0)
    TEST_UNIT = can_generate (fun x -> x = 1)
    TEST_UNIT = can_generate (fun x -> x = -1)
    TEST_UNIT = can_generate (fun x -> x > 256)
    TEST_UNIT = can_generate (fun x -> x < -256)

    let middle_bits x = x land 0x0000_ffff_ffff_0000
    TEST_UNIT = can_generate (fun x ->
      middle_bits x <> middle_bits   0 &&
      middle_bits x <> middle_bits (-1))
  end

  TEST_MODULE "float" = struct
    let bits_compare x y = Int64.compare (Int64.bits_of_float x) (Int64.bits_of_float y)
    let bits_equal x y = (bits_compare x y) = 0
    let sexp_of = Float.sexp_of_t
    let by = `Compare bits_compare
    let gen = G.float
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    let has_class x c =
      match Float.classify x, (c : Float.Class.t) with
      | Infinite,  Infinite
      | Nan,       Nan
      | Normal,    Normal
      | Subnormal, Subnormal
      | Zero,      Zero
        -> true
      | _ -> false

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun x -> has_class x Infinite)
    TEST_UNIT = can_generate (fun x -> has_class x Nan)
    TEST_UNIT = can_generate (fun x -> has_class x Normal)
    TEST_UNIT = can_generate (fun x -> has_class x Subnormal)
    TEST_UNIT = can_generate (fun x -> has_class x Zero)
    TEST_UNIT = can_generate (fun x -> Float.(<) x 0.)
    TEST_UNIT = can_generate (fun x -> Float.(>) x 0.)
    TEST_UNIT = can_generate (fun x -> Float.(=) x 0. && bits_equal x 0.)
    TEST_UNIT = can_generate (fun x -> Float.(=) x 0. && not (bits_equal x 0.))
    TEST_UNIT = can_generate (fun x -> Float.(=) x Float.neg_infinity)
  end

  TEST_MODULE "string" = struct
    let sexp_of = String.sexp_of_t
    let by = `Compare String.compare
    let gen = G.string
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun x -> String.length x = 0)
    TEST_UNIT = can_generate (fun x -> String.length x = 1)
    TEST_UNIT = can_generate (fun x -> String.length x = 2)
    TEST_UNIT = can_generate (fun x -> String.length x > 2)
    TEST_UNIT = can_generate (fun x -> String.uppercase x <> x)
    TEST_UNIT = can_generate (fun x -> String.lowercase x <> x)
    TEST_UNIT = can_generate (fun x ->
      match Int.of_string x with
      | _ -> true
      | exception _ -> false)
  end

  TEST_MODULE "char" = struct
    let sexp_of = Char.sexp_of_t
    let by = `Compare Char.compare
    let gen = G.char
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate Char.is_digit
    TEST_UNIT = can_generate Char.is_lowercase
    TEST_UNIT = can_generate Char.is_uppercase
    TEST_UNIT = can_generate Char.is_print
    TEST_UNIT = can_generate Char.is_whitespace
    TEST_UNIT = can_generate (fun c ->
      not (Char.is_digit c)
      && not (Char.is_lowercase c)
      && not (Char.is_uppercase c)
      && not (Char.is_print c)
      && not (Char.is_whitespace c))
  end

  TEST_MODULE "tuple" = struct
    let sexp_of = <:sexp_of< Int.Hex.t * Int.Hex.t >>
    let by = `Compare <:compare< int * int >>
    let gen = G.(tuple int int)
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun (x,y) -> x = y)
    TEST_UNIT = can_generate (fun (x,y) -> x < y)
    TEST_UNIT = can_generate (fun (x,y) -> x > y)
  end

  TEST_MODULE "option" = struct
    let sexp_of = <:sexp_of< Int.Hex.t option >>
    let by = `Compare <:compare< int option >>
    let gen = G.(option int)
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate Option.is_none
    TEST_UNIT = can_generate Option.is_some
  end

  TEST_MODULE "function" = struct
    let sexp_of = snd
    let by = `Compare <:compare< Sexp.t >>
    let gen = G.(fn_with_sexp O.int int ~sexp_of_rng:<:sexp_of< int >>)
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates Generator.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    TEST_UNIT = can_generate (fun (f, _) -> f 0 = f (-1))
    TEST_UNIT = can_generate (fun (f, _) -> f 0 < f (-1))
    TEST_UNIT = can_generate (fun (f, _) -> f 0 > f (-1))

    TEST_UNIT = can_generate (fun (f, _) -> f 1 = f 0)
    TEST_UNIT = can_generate (fun (f, _) -> f 1 < f 0)
    TEST_UNIT = can_generate (fun (f, _) -> f 1 > f 0)

    TEST_UNIT = can_generate (fun (f, _) -> f 2 = f 1)
    TEST_UNIT = can_generate (fun (f, _) -> f 2 < f 1)
    TEST_UNIT = can_generate (fun (f, _) -> f 2 > f 1)

    TEST_UNIT = can_generate (fun (f, _) -> f (-1) <> f 0 && f 0 <> f 1 && f 1 <> f (-1))
    TEST_UNIT = can_generate (fun (f, _) -> f (-1) =  f 0 && f 0 =  f 1 && f 1 =  f (-1))

    TEST_UNIT = can_generate (fun (f, _) -> f 0x0000_ffff_ffff_0000 <> f 0)

  end

  TEST_MODULE "higher-order function" = struct
    let sexp_of = snd
    let by = `Compare <:compare< Sexp.t >>
    let gen =
      G.fn_with_sexp
        (O.fn G.int O.int ~sexp_of_dom:<:sexp_of< int >>)
        G.int ~sexp_of_rng:<:sexp_of< int >>
    let can_generate ?trials f = Q.test_can_generate gen ~sexp_of ~f ?trials

    TEST_UNIT = Q.test_no_duplicates Generator.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    TEST_UNIT = can_generate (fun (f, _) -> f Int.succ = f Int.pred)
    TEST_UNIT = can_generate (fun (f, _) -> f Int.succ > f Int.pred)
    TEST_UNIT = can_generate (fun (f, _) -> f Int.succ < f Int.pred)

    TEST_UNIT = can_generate (fun (f, _) -> f Int.neg <> f Int.abs)
    TEST_UNIT = can_generate (fun (f, _) -> f Int.neg <> f Fn.id)
    TEST_UNIT = can_generate (fun (f, _) -> f Int.abs <> f Fn.id)

    TEST_UNIT = can_generate ~trials:4_000 (fun (f, _) ->
      let x = f Fn.id   in
      let y = f Int.neg in
      let z = f Int.abs in
      x <> y && y <> z && z <> x)

  end

  TEST_MODULE "list" = struct
    let sexp_of = <:sexp_of< Int.Hex.t list >>
    let by = `Compare <:compare< int list >>
    let gen = G.(list int)
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    let is_sorted_with_dup list ~compare =
      List.is_sorted list ~compare
      && not (List.is_sorted_strictly list ~compare)

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate List.is_empty
    TEST_UNIT = can_generate (function [_] -> true | _ -> false)
    TEST_UNIT = can_generate (function [x;y] -> x=y | _ -> false)
    TEST_UNIT = can_generate (function [x;y] -> x<y | _ -> false)
    TEST_UNIT = can_generate (function [x;y] -> x>y | _ -> false)
    TEST_UNIT = can_generate (fun list ->
      List.length list > 2
      && List.is_sorted_strictly list ~compare:Int.compare)
    TEST_UNIT = can_generate (fun list ->
      List.length list > 2
      && is_sorted_with_dup list ~compare:Int.compare)
    TEST_UNIT = can_generate (fun list ->
      List.length list > 2
      && List.is_sorted_strictly (List.rev list) ~compare:Int.compare)
    TEST_UNIT = can_generate (fun list ->
      List.length list > 2
      && is_sorted_with_dup (List.rev list) ~compare:Int.compare)
  end

  TEST_MODULE "sexp" = struct
    let sexp_of = <:sexp_of< Sexp.t >>
    let by = `Compare <:compare< Sexp.t >>
    let gen =
      G.recursive (fun sexp_gen ->
        G.map ~f:(function `A s -> Sexp.Atom s | `B l -> Sexp.List l)
          (G.variant G.string (G.list sexp_gen)))
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (function Sexp.Atom _       -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List _       -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.Atom ""      -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List []      -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List [_]     -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List [_;_]   -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List [_;_;_] -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.Atom _ -> false | Sexp.List list ->
      let is_atom = function Sexp.Atom _ -> true | Sexp.List _ -> false in
      List.length list >= 2 && List.for_all list ~f:is_atom)
  end

  TEST_MODULE "function on recursive data" = struct
    let sexp_of = snd
    let by = `Compare <:compare< Sexp.t >>
    let gen = G.(fn_with_sexp O.(list bool) G.char ~sexp_of_rng:<:sexp_of< char >>)
    let can_generate f = Q.test_can_generate gen ~sexp_of ~f

    TEST_UNIT = Q.test_no_duplicates Generator.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    TEST_UNIT = can_generate (fun (f, _) -> f [] = f [true])
    TEST_UNIT = can_generate (fun (f, _) -> f [] = f [false])
    TEST_UNIT = can_generate (fun (f, _) -> f [true] = f [false])

    TEST_UNIT = can_generate (fun (f, _) -> f [] <> f [true])
    TEST_UNIT = can_generate (fun (f, _) -> f [] <> f [false])
    TEST_UNIT = can_generate (fun (f, _) -> f [true] <> f [false])

    TEST_UNIT = can_generate (fun (f, _) -> f [true;true] <> f [true;false])

  end

end
