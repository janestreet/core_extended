open Base

type -'a t =
  { headers : string list
  ; to_columns : 'a -> tail:string list -> string list
  }
[@@deriving fields]

let empty = { headers = []; to_columns = (fun _ ~tail -> tail) }

let column to_string ~header =
  { headers = [ header ]; to_columns = (fun x ~tail -> to_string x :: tail) }
;;

let append l r =
  let to_columns_l = l.to_columns and to_columns_r = r.to_columns in
  { headers = List.append l.headers r.headers
  ; to_columns = (fun x ~tail -> to_columns_l x ~tail:(to_columns_r x ~tail))
  }
;;

let of_list = function
  | [] -> empty
  | [ x ] -> x
  | first :: others -> List.fold others ~init:first ~f:append
;;

let contra_map x ~f =
  let to_columns = x.to_columns in
  { x with to_columns = (fun x ~tail -> to_columns (f x) ~tail) }
;;

let map_headers t ~f = { t with headers = List.map t.headers ~f }

let to_columns t x = to_columns t x ~tail:[]

module Fields_O = struct
  let ( !! ) to_string field =
    let read_field = Field.get field in
    column (fun r -> to_string (read_field r)) ~header:(Field.name field)
  ;;

  let ( !> ) inner field =
    map_headers
      inner
      ~f:
        (let prefix = Field.name field ^ "_" in
         fun name -> prefix ^ name)
    |> contra_map ~f:(Field.get field)
  ;;
end

module O = struct
  let ( <<| ) t f = contra_map t ~f

  let ( <> ) = append
end

let to_string_m (type t) (module T : Stringable.S with type t = t) = T.to_string

let column_m m ~header = column (to_string_m m) ~header

let column_m_opt ?(default="") m ~header =
  column (Option.value_map ~default ~f:(to_string_m m)) ~header
;;
