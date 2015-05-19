open Core_kernel.Std
open Janecheck_kernel.Std

include Janecheck_intf

module type Janecheck_config =
  Janecheck_kernel_config with module Random_state := Random.State

module type Janecheck = sig

  include Janecheck_kernel with module Random_state := Random.State

  (** [random_sequence ?seed gen] produces a sequence of values chosen from [gen]. *)
  val random_sequence
    :  ?seed:seed
    -> 'a Generator.t
    -> 'a Sequence.t

end
