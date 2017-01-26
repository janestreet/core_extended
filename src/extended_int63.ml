module Verified_spec = struct
  include Core.Int63
  let module_name = "Int63"
end

include Number.Make_verified_std (Verified_spec)
