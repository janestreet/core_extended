module Verified_spec = struct
  include Core.Nativeint
  let module_name = "Nativeint"
end

include Number.Make_verified_std (Verified_spec)
