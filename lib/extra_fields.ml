open Core.Std

exception Not_a_record_type of Sexp.t with sexp

let fields sexp =
  match sexp with
  | Sexp.Atom _ -> raise (Not_a_record_type sexp)
  | Sexp.List l ->
    List.map l ~f:(function
      | Sexp.List (Sexp.Atom field::_) -> field
      | _ -> raise (Not_a_record_type sexp)
    )

module Make (M:Sexpable) = struct
  type t = {
    value : M.t;
    extra_fields : string list
  }

  let sexp_of_t t = M.sexp_of_t t.value

  let t_of_sexp sexp =
    let ef_ref = Sexplib.Conv.record_check_extra_fields in
    let prev_ef = !ef_ref in
    ef_ref := false;
    let value = M.t_of_sexp sexp in
    ef_ref := prev_ef;
    let used_fields = fields (M.sexp_of_t value) in
    let all_fields = fields sexp in
    let extra_fields = List.filter all_fields ~f:(fun field ->
      not (List.mem used_fields field)
    )
    in
    {
      value = value;
      extra_fields = extra_fields;
    }
end
