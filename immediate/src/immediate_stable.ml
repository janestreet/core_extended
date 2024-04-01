include Immediate_kernel.Immediate_kernel_stable

module Interned_string = struct
  include Immediate_interned_string.Stable
  module Option = Immediate_interned_string.Option.Stable
end

module Short_string = struct
  include Immediate_short_string.Stable
  module Option = Immediate_short_string.Option.Stable
end

module String = struct
  include Immediate_string.Stable
  module Option = Immediate_string.Option.Stable
end
