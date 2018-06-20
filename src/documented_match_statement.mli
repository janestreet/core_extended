(** A [t] represents a match statement where every case is documented.  You can then use it
    in one way to extract documentation, and in another way as the underlying function.

    More specifically, an [('input,'output) t] represents a match statement matching things
    of type ['input] and producing things of type ['output]. It consists of
    [specific_cases] and a [catchall_case].

    There is also a type [('input, 'input_pattern, 'output) t_]; this allows you to have a
    separate ['input_pattern] type representing the patterns that an input of type
    ['input] can be matched against.

    The [specific_cases] are straightforward: [{pattern;documentation;value}] represents
    "| pattern -> value", with [documentation] explaining what's going on.

    The [catchall_case] can be either [`Unused x], representing "| _ -> x" with no
    documentation, or [`Used case], representing "| x -> f x", where [f] is [case.value],
    and [case.documentation] explains what's going on with [f].  This is intended to allow
    many input values to be handled uniformly without having to document each one individually.
*)

type ('input, 'output) case =
  { pattern : 'input list
  ; documentation : string
  ; value : 'output
  }

type ('input, 'input_pattern, 'output) t_ =
  { specific_cases : ('input_pattern, unit -> 'output) case list
  ; catchall_case : [ `Used of ([`Catchall], 'input -> 'output) case
                    | `Unused of unit -> 'output ]
  }

type ('input, 'output) t = ('input, 'input, 'output) t_

val map
  :  ('input, 'input_pattern, 'output1) t_
  -> f:('output1 -> 'output2)
  -> ('input, 'input_pattern, 'output2) t_

val map_case
  :  ('input, unit -> 'output1) case
  -> f:('output1 -> 'output2)
  -> ('input, unit -> 'output2) case


val map_cases
  :  ('input, unit -> 'output1) case list
  -> f:('output1 -> 'output2)
  -> ('input, unit -> 'output2) case list

val map_pattern
  :  ('input1, 'input_pattern1, 'output) t_
  -> f1:('input_pattern1 -> 'input_pattern2)
  -> f2:('input2 -> 'input1)
  -> ('input2, 'input_pattern2, 'output) t_

(** [prepend ~specific_cases t] matches on [specific_cases] before moving on to [t].

    A common situation is representing

    {[
      let f t x =
        match x with
        | `A -> ...
        | `B -> ...
        | _ -> {t with field = g t.field x}
    ]}

    which can be done by combining [prepend] and [map]:

    {[
      let f' =
        prepend ~specific_cases:[{pattern = `A;...};{pattern = `B;...}]
          (map g' ~f:(fun h t -> {t with field = h t.field}))
    ]}
*)
val prepend
  :  specific_cases:('input_pattern, unit -> 'output) case list
  -> ('input, 'input_pattern, 'output) t_
  -> ('input, 'input_pattern, 'output) t_

(** [match_ t] pulls out the underlying function of [t]  *)
val match_
  :  ('input, 'input_pattern, 'output) t_
  -> 'input
  -> matches:('input -> 'input_pattern -> bool)
  -> 'output

val documentation
  :  ('input, 'input_pattern, 'output) t_
  -> input_to_string:('input_pattern -> string)
  -> title:string
  -> string list
