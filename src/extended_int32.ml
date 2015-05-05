module Verified_spec = struct
  include Core.Std.Int32
  let module_name = "Int32"
end

include Number.Make_verified_std (Verified_spec)
