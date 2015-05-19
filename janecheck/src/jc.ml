(* This module alias appears to be necessary to build with ocamlbuild. Not sure why. *)
module Janecheck = Janecheck_kernel.Std.Janecheck
open Core_kernel.Std
open Janecheck_kernel.Std
open Jc_intf

module Configure (Config : Janecheck_config) = struct

  include Janecheck.Configure (struct
      include Config
      module Random_state = Random.State
    end)

  let random_sequence ?(seed = default_seed) gen =
    let random_state = random_state_of_seed seed in
    let random_float_between_zero_and_one () =
      Random.State.float random_state 1.
    in
    Sequence.unfold_step ~init:gen ~f:(fun gen ->
      match Generator.choose gen ~random_float_between_zero_and_one with
      | `Empty          -> Done
      | `Failure gen    -> Skip  gen
      | `Success choice -> Yield ( Generator.Choice.value choice
                                 , Generator.Choice.ne    choice ))

end

include Configure (Janecheck)
