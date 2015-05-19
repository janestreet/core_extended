open Core.Std
open Janecheck.Std

TEST_UNIT "count vs length" =
  Janecheck.test
    (* (\* Initial example that fails on NaN: *\)
     * Generator.(list float) *)
    (* Working example that filters out NaN: *)
    Generator.(list (float_between
                       ~lower_bound:(`Inclusive Float.neg_infinity)
                       ~upper_bound:(`Inclusive Float.infinity)
                       ~with_nan:`None))
    (* (\* Alternate version of filtering out NaN: *\)
     * Generator.(list (filter float ~f:(Fn.non Float.is_nan))) *)
    ~sexp_of:<:sexp_of< float list >>
    ~f:(fun float_list ->
      <:test_result< int >>
        (List.count float_list ~f:(fun x -> x = x))
        ~expect:(List.length float_list))

let list_gen elt_gen =
  Generator.(recursive (fun list_gen ->
    variant unit (tuple elt_gen list_gen)
    >>| function
    | `A () -> []
    | `B (head, tail) -> head :: tail))

let sexp_gen =
  Generator.(recursive (fun sexp_gen ->
    variant string (list_gen sexp_gen)
    >>| function
    | `A atom -> Sexp.Atom atom
    | `B list -> Sexp.List list))

