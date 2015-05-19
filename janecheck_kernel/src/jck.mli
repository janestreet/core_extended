open Janecheck_intf

include Janecheck_kernel
  with module Random_state := Random.State

module Configure (Config : Janecheck_kernel_config)
  : Janecheck_kernel with module Random_state := Config.Random_state
