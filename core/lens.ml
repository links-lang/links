module Helpers = struct
  include LensHelpers

  module Record = LensRecordHelpers
  module Classic = LensHelpersClassic
  module Incremental = LensHelpersIncremental
  module Query = LensQueryHelpers
  module FunDeps = LensFDHelpers
end

module Utility = struct
  include LensUtility
end

module Types = struct
  include LensTypes
end

module SetOperations = struct
  include LensSetOperations
end
