open Core.Std

module type S = sig
  module Key : sig
    type 'a t
    val create : unit -> 'a t
  end
  type t
  val empty : t
  val add : t -> 'a Key.t -> 'a -> t
  val find : t -> 'a Key.t -> 'a option
  val find_all : t -> 'a Key.t -> 'a list
end

(* simple but inefficient implementation *)

module Dumb : S = struct

  module Key = struct

    type 'a t = {
      inject : 'a -> Univ.t;
      project : Univ.t -> 'a option;
    }

    let create () =
      let constr = Univ.Constr.create "DUMMY" (fun _ -> Sexp.Atom "DUMMY") in
      let inject a = Univ.create constr a in
      let project univ = Univ.match_ univ constr in
      {inject; project}

  end

  type t = Univ.t list

  let empty = []

  let add t {Key.inject; _} value = inject value :: t

  let find t {Key.project; _} = List.find_map t ~f:project

  let find_all t {Key.project; _} = List.filter_map t ~f:project

end

(* efficient but more complex implementation *)

module Smart : S = struct

  module Id = Unique_id.Int (struct end) (* perfect uniqueness of ids is not required *)

  module Key = struct
    type 'a t = Id.t * 'a Dumb.Key.t
    let create () = (Id.create (), Dumb.Key.create ())
  end

  type t = Dumb.t Id.Map.t

  let empty = Id.Map.empty

  let add t (uid, key) value =
    Id.Map.change t uid (fun entry ->
      let simple = Option.value entry ~default:Dumb.empty in
      Some (Dumb.add simple key value))

  let find t (uid, key) =
    Option.bind (Id.Map.find t uid) (fun simple -> Dumb.find simple key)

  let find_all t (uid, key) =
    match Id.Map.find t uid with
    | None -> []
    | Some simple -> Dumb.find_all simple key

end

include Smart

