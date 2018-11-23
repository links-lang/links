module Column = Lens_column
module Sort = Lens_sort
module Fun_dep = Lens_fun_dep
module Alias = Lens_alias

module Helpers = struct
  include LensHelpers

  module Record = LensRecordHelpers
  module Classic = LensHelpersClassic
  module Incremental = LensHelpersIncremental
  module Query = LensQueryHelpers
end

module Types = struct
  include LensTypes
end
