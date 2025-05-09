module type S = sig
  type t

  val of_immediate_string : Immediate_string.t -> t
  val to_immediate_string : t -> Immediate_string.t [@@zero_alloc]
end
