open Base

(** Used to describe a way to create a single row of a CSV
    from a given type.
*)
type -'a t

val empty : 'a t
val column : ('a -> string) -> header:string -> 'a t
val column_m : (module Stringable.S with type t = 'a) -> header:string -> 'a t

(** [default] is printed in place of [None], and if not supplied
    is the empty string.
*)
val column_m_opt
  :  ?default:string
  -> (module Stringable.S with type t = 'a)
  -> header:string
  -> 'a option t

val of_list : 'a t list -> 'a t
val append : 'a t -> 'a t -> 'a t
val contra_map : 'b t -> f:('a -> 'b) -> 'a t
val map_headers : 'a t -> f:(string -> string) -> 'a t
val headers : 'a t -> string list
val to_columns : 'a t -> 'a -> string list

(** Open for prefix operators useful for using with Fields.to_list.

    e.g.
    {[
      let csv =
        Builder.Fields_O.(
          Fields.to_list
            ~a_string:!!Fn.id
            ~a_date:!!Date.to_string
            ~mv:!>Long_short.csv)
        |> Builder.of_list
    ]}
*)
module Fields_O : sig
  (** Create a single column from a field of a record. *)
  val ( !! ) : ('a -> string) -> ('b, 'a) Field.t -> 'b t

  (** Nest a builder in a field of a record.

      Column headers will be prefixed with the name of the field.
  *)
  val ( !> ) : 'a t -> ('b, 'a) Field.t -> 'b t
end

module O : sig
  val ( <<| ) : 'b t -> ('a -> 'b) -> 'a t
  val ( <> ) : 'a t -> 'a t -> 'a t
end
