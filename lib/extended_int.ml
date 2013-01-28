(**
   Extensions to [Core.Core_int]
*)

module Verified_spec = struct
  include Core.Std.Int
  let module_name = "Int"
end

include Number.Make_verified_std (Verified_spec)
